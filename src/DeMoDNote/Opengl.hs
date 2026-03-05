{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

-- =============================================================================
-- DeMoDNote.Opengl
-- =============================================================================
-- Multi-threaded OpenGL visualizer for DeMoD-Note
--   • Runs on dedicated OS thread (forkOS) for GL context isolation
--   • Reads from TVar ReactorState for lock-free state sharing
--   • Real-time note visualization from JACK audio detection
--   • Low-latency, high-FPS OpenGL core profile
--   • MIDI file playback with falling notes (Rocksmith/Guitar Hero style)
--   • Modular shaders with live reload (F5)
--   • 3D model support (OBJ), SVG rendering, ImGui menu
--   • VTY/Brick TUI rendering in NanoVG sub-window
--
-- THREADING ARCHITECTURE:
--   ┌──────────────┐     STM TVar      ┌──────────────────┐
--   │   Core 0     │    ─────────►     │     Core 1+      │
--   │  JACK Audio  │   ReactorState    │  OpenGL Render   │
--   │  Thread      │   (lock-free)     │  Thread (forkOS) │
--   │  • RT Priority│                  │  • 60+ FPS       │
--   │  • 2.66ms lat │                  │  • VSync off     │
--   └──────────────┘                   └──────────────────┘
--
-- DEPENDENCIES:
--   - GLFW-b, gl, stm, bytestring, optparse-applicative
--   - filepath, directory, time, text, zmidi-core
--   - containers, binary, dear-imgui, nanovg, svg-tree, vty
-- =============================================================================

module DeMoDNote.Opengl
  ( -- * Entry points
    runOpenGLVisualizer
  , runOpenGLStandalone
  , OpenGLConfig(..)
  , defaultOpenGLConfig
    -- * State types
  , GLRenderState(..)
  , GLNote(..)
    -- * Re-exports for integration
  , ShaderSource(..)
  , ShaderConfig(..)
  ) where

import qualified Graphics.UI.GLFW as GLFW
import Graphics.GL
import Foreign
import Foreign.C.String (withCString)
import Control.Monad (when, unless, forever, forM_, void)
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, atomically, writeTVar)
import Control.Concurrent (forkIO, forkOS, threadDelay, ThreadId)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime, utctDayTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.Exit (exitSuccess, exitFailure)
import System.FilePath ((</>))
import System.Directory (doesFileExist, getHomeDirectory)
import Text.Printf (printf)
import qualified ZMidi.Core as ZM
import Data.List (sortBy, foldl')
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import Data.Word (Word8, Word16)
import Data.Char (isSpace)
import Data.Maybe (isJust, fromMaybe)
import Data.Bits ((.&.))
import qualified DearImGui as ImGui
import qualified DearImGui.GLFW as ImGuiGLFW
import qualified DearImGui.OpenGL3 as ImGuiGL3
import qualified NanoVG as NVG
import qualified Graphics.Vty as VTY
import qualified Text.SVG.Tree as SVGTree
import qualified Text.SVG.Types as SVG
import System.Process (createProcess, proc, std_out, std_err, StdStream(..), ProcessHandle, terminateProcess, waitForProcess)
import Control.Exception (SomeException, try)
import Paths_DeMoD_Note (getDataFileName)

-- Import DeMoD-Note types for integration
import DeMoDNote.Types (ReactorState(..), NoteState(..), MIDINote, Velocity, emptyReactorState, defaultPLLState, defaultOnsetFeatures)
import qualified DeMoDNote.Config as Config

-- =============================================================================
-- Configuration Types
-- =============================================================================

-- | Intro phases
data IntroPhase
  = IntroPresents    -- "DeMoD LLC presents" typewriter
  | IntroReveal      -- Sierpinski triangle scan-line reveal
  | IntroAstart      -- "Astart" banner display
  | IntroFadeOut     -- Fade to main visualization
  | IntroComplete    -- Intro done, show normal UI
  deriving (Eq, Show)

-- | Intro animation state
data IntroState = IntroState
  { introPhase    :: !IntroPhase
  , introTick     :: !Int           -- Frame counter
  , charCount     :: !Int           -- Chars revealed in Presents phase
  , scanRow       :: !Int           -- Rows revealed in Reveal phase
  , fadeAlpha     :: !Float         -- 1.0 = intro, 0.0 = main viz
  } deriving (Show)

-- | Audio player state
data AudioPlayer = NoAudio | Player ProcessHandle

-- | Constants for visualization
defaultNumLanes :: Int
defaultNumLanes = 5

defaultPreviewSeconds :: Double
defaultPreviewSeconds = 3.0

minNoteHeight :: Double
minNoteHeight = 0.02

-- | Default lane colors (RGB)
defaultLaneColors :: [(GLfloat, GLfloat, GLfloat)]
defaultLaneColors = 
  [ (0.0, 1.0, 0.0)  -- Green
  , (1.0, 0.0, 0.0)  -- Red
  , (1.0, 1.0, 0.0)  -- Yellow
  , (0.0, 0.0, 1.0)  -- Blue
  , (1.0, 0.5, 0.0)  -- Orange
  ]

-- | Intro timing constants (in frames @ ~30fps)
presentsDuration :: Int
presentsDuration = 90  -- ~3 seconds

revealDuration :: Int
revealDuration = 120   -- ~4 seconds

astartDuration :: Int
astartDuration = 60    -- ~2 seconds

fadeOutDuration :: Int
fadeOutDuration = 30   -- ~1 second

-- Import shared intro assets (banners, Sierpiński renderer, timeToDepthAndChar)
import DeMoDNote.Intro

-- =============================================================================
-- Audio Playback (Cross-platform with caching)
-- =============================================================================

-- Note: the working audio player command is cached per-AppState in
-- asAudioPlayerCmd (IORef (Maybe (String, [String]))); see AppState below.
-- This avoids module-level unsafePerformIO mutable state.

spawnIntroAudio :: IORef (Maybe (String, [String])) -> FilePath -> IO AudioPlayer
spawnIntroAudio cacheRef path = do
  cached <- readIORef cacheRef
  case cached of
    Just (cmd, args) -> do
      result <- try $ createProcess
        (proc cmd (args ++ [path])) { std_out = CreatePipe, std_err = CreatePipe }
      case result of
        Right (_, _, _, ph) -> return (Player ph)
        Left (_ :: SomeException) -> do
          writeIORef cacheRef Nothing
          spawnIntroAudio cacheRef path
    Nothing -> tryPlayers candidates
  where
    candidates =
      [ ("mpg123",  ["-q"])
      , ("afplay",  [])
      , ("mplayer", ["-really-quiet"])
      ]
    tryPlayers [] = do
      putStrLn "[OpenGL] WARNING: no audio player found (mpg123/afplay/mplayer). Continuing without audio."
      return NoAudio
    tryPlayers ((cmd, args) : rest) = do
      result <- try $ createProcess
        (proc cmd (args ++ [path])) { std_out = CreatePipe, std_err = CreatePipe }
      case result of
        Left  (_ :: SomeException) -> tryPlayers rest
        Right (_, _, _, ph)        -> do
          writeIORef cacheRef $ Just (cmd, args)
          return (Player ph)

stopAudioPlayer :: AudioPlayer -> IO ()
stopAudioPlayer NoAudio     = return ()
stopAudioPlayer (Player ph) = void . try @SomeException $
  terminateProcess ph >> waitForProcess ph

fadeOutAndStopAudio :: AudioPlayer -> IO ()
fadeOutAndStopAudio NoAudio = return ()
fadeOutAndStopAudio (Player ph) = void . forkIO $ do
  -- Gradually reduce volume over 0.5 seconds (best effort)
  threadDelay 500000  -- 0.5 second delay before stopping
  void . try @SomeException $ terminateProcess ph >> waitForProcess ph

data ShaderSource
  = Embedded !BS.ByteString
  | FromFile !FilePath
  deriving (Show, Eq)

data ShaderConfig = ShaderConfig
  { scVertex   :: !ShaderSource
  , scFragment :: !ShaderSource
  } deriving (Show, Eq)

data OpenGLConfig = OpenGLConfig
  { oglWidth        :: !Int
  , oglHeight       :: !Int
  , oglTitle        :: !String
  , oglNoVSync      :: !Bool
  , oglShaderConfig :: !ShaderConfig
  , oglMidiFile     :: !(Maybe FilePath)
  , oglTracks       :: ![Int]
  , oglObjFile      :: !(Maybe FilePath)
  , oglSvgFile      :: !(Maybe FilePath)
  , oglFontRegular  :: !FilePath
  , oglFontBold     :: !FilePath
  , oglSkipIntro    :: !Bool
  , oglIntroDuration :: !Int
  } deriving (Show)

defaultOpenGLConfig :: OpenGLConfig
defaultOpenGLConfig = OpenGLConfig
  { oglWidth        = 1280
  , oglHeight       = 720
  , oglTitle        = "DeMoD-Note OpenGL Visualizer"
  , oglNoVSync      = True
  , oglShaderConfig = ShaderConfig (Embedded defaultVertexSrc) (Embedded defaultFragmentSrc)
  , oglMidiFile     = Nothing
  , oglTracks       = []
  , oglObjFile      = Nothing
  , oglSvgFile      = Nothing
  , oglFontRegular  = "DejaVuSansMono.ttf"
  , oglFontBold     = "DejaVuSansMono-Bold.ttf"
  , oglSkipIntro    = False
  , oglIntroDuration = 10
  }

-- | Find a font file in common system locations
findFont :: String -> IO (Maybe FilePath)
findFont fontName = do
  home <- getHomeDirectory
  let candidates = 
        [ "/usr/share/fonts/truetype/dejavu/" ++ fontName
        , "/usr/share/fonts/truetype/" ++ fontName
        , "/usr/local/share/fonts/" ++ fontName
        , home </> ".local/share/fonts/" ++ fontName
        , home </> ".fonts/" ++ fontName
        , "/System/Library/Fonts/" ++ fontName  -- macOS
        , "C:\\Windows\\Fonts\\" ++ fontName    -- Windows
        ]
  go candidates
  where
    go [] = pure Nothing
    go (p:ps) = do
      exists <- doesFileExist p
      if exists
        then pure (Just p)
        else go ps

-- | Load fonts with fallback to system search
loadFonts :: NVG.Context -> String -> String -> IO ()
loadFonts ctx regularFont boldFont = do
  -- '/' elem check: String is [Char], so we test for the Char '/', not the String "/".
  mbRegPath <- if '/' `elem` regularFont
                then pure (Just regularFont)  -- Absolute path
                else findFont regularFont
  mbBoldPath <- if '/' `elem` boldFont
                 then pure (Just boldFont)
                 else findFont boldFont
  
  case mbRegPath of
    Nothing -> putStrLn $ "Warning: Could not find regular font: " ++ regularFont
    Just path -> do
      result <- NVG.createFont ctx "regular" path
      case result of
        NVG.FontHandle _ -> pure ()
        _ -> putStrLn $ "Warning: Failed to load regular font from: " ++ path
  
  case mbBoldPath of
    Nothing -> putStrLn $ "Warning: Could not find bold font: " ++ boldFont
    Just path -> do
      result <- NVG.createFont ctx "bold" path
      case result of
        NVG.FontHandle _ -> pure ()
        _ -> putStrLn $ "Warning: Failed to load bold font from: " ++ path

-- =============================================================================
-- Render State (renamed to avoid conflict with Backend.AudioState)
-- =============================================================================

-- | State for OpenGL rendering thread - updated from ReactorState
data GLRenderState = GLRenderState
  { glrsTime         :: !Double      -- ^ Current audio time
  , glrsAmplitude    :: !Float       -- ^ Current amplitude
  , glrsCurrentNote  :: !(Maybe (MIDINote, Velocity))
  , glrsNoteHistory  :: ![(MIDINote, Velocity)]
  , glrsConfidence   :: !Double
  , glrsWaveform     :: ![Double]
  , glrsBPM          :: !Double
  } deriving (Show)

-- | Note for MIDI file playback visualization
data GLNote = GLNote
  { glnStart    :: !Double
  , glnDur      :: !Double
  , glnPitch    :: !Int
  , glnVel      :: !Int
  , glnChannel  :: !Int
  } deriving (Show)

-- | Convert from ReactorState to GLRenderState.
-- Previously glrsAmplitude was hardcoded to 0.5 and glrsConfidence to 0.8.
fromReactorState :: ReactorState -> GLRenderState
fromReactorState rs = GLRenderState
  { glrsTime         = fromIntegral (lastOnsetTime rs) / 1e6  -- μs → seconds
  , glrsAmplitude    = rmsAmplitude (latestWaveform rs)
  , glrsCurrentNote  = case currentNotes rs of
      []    -> Nothing
      (n:_) -> Just n
  , glrsNoteHistory  = take 20 (currentNotes rs)
  , glrsConfidence   = detectionConfidence rs
  , glrsWaveform     = latestWaveform rs
  , glrsBPM          = reactorBPM rs
  }
  where
    -- RMS amplitude: √(mean of squares), clamped to [0,1].
    rmsAmplitude [] = 0.0
    rmsAmplitude xs = realToFrac . min 1.0 . sqrt $
        sum (map (\x -> x * x) xs) / fromIntegral (length xs)

-- =============================================================================
-- Shaders
-- =============================================================================

defaultVertexSrc :: BS.ByteString
defaultVertexSrc = BS8.pack
  "#version 410 core\n\
  \layout (location = 0) in vec2 aPos;\n\
  \void main() {\n\
  \  gl_Position = vec4(aPos, 0.0, 1.0);\n\
  \}\0"

defaultFragmentSrc :: BS.ByteString
defaultFragmentSrc = BS8.pack
  "#version 410 core\n\
  \out vec4 FragColor;\n\
  \uniform float uTime;\n\
  \uniform float uAmp;\n\
  \uniform vec2  uResolution;\n\
  \uniform vec2  uMouse;\n\
  \void main() {\n\
  \  vec2 uv = gl_FragCoord.xy / uResolution;\n\
  \  vec2 mouseNorm = uMouse / uResolution;\n\
  \  float dist = length(uv - mouseNorm);\n\
  \  float wave = sin(uTime * 15.0 + uv.x * 30.0 + uv.y * 20.0) * uAmp * 0.6 + 0.5;\n\
  \  float glow = exp(-dist * 8.0) * uAmp;\n\
  \  FragColor = vec4(wave, glow, 0.9, 1.0);\n\
  \}\0"

noteVertexSrc :: BS.ByteString
noteVertexSrc = BS8.pack
  "#version 410 core\n\
  \layout (location = 0) in vec2 aPos;\n\
  \layout (location = 1) in vec3 aColor;\n\
  \out vec3 fragColor;\n\
  \void main() {\n\
  \  gl_Position = vec4(aPos, 0.0, 1.0);\n\
  \  fragColor = aColor;\n\
  \}\0"

noteFragmentSrc :: BS.ByteString
noteFragmentSrc = BS8.pack
  "#version 410 core\n\
  \in vec3 fragColor;\n\
  \out vec4 FragColor;\n\
  \void main() {\n\
  \  FragColor = vec4(fragColor, 1.0);\n\
  \}\0"

modelVertexSrc :: BS.ByteString
modelVertexSrc = BS8.pack
  "#version 410 core\n\
  \layout (location = 0) in vec3 aPos;\n\
  \layout (location = 1) in vec3 aNormal;\n\
  \uniform mat4 model;\n\
  \uniform mat4 view;\n\
  \uniform mat4 projection;\n\
  \out vec3 Normal;\n\
  \out vec3 FragPos;\n\
  \void main() {\n\
  \  FragPos = vec3(model * vec4(aPos, 1.0));\n\
  \  Normal = mat3(transpose(inverse(model))) * aNormal;\n\
  \  gl_Position = projection * view * vec4(FragPos, 1.0);\n\
  \}\0"

modelFragmentSrc :: BS.ByteString
modelFragmentSrc = BS8.pack
  "#version 410 core\n\
  \out vec4 FragColor;\n\
  \in vec3 Normal;\n\
  \in vec3 FragPos;\n\
  \uniform vec3 lightPos;\n\
  \uniform vec3 viewPos;\n\
  \uniform vec3 lightColor;\n\
  \uniform vec3 objectColor;\n\
  \void main() {\n\
  \  float ambientStrength = 0.1;\n\
  \  vec3 ambient = ambientStrength * lightColor;\n\
  \  vec3 norm = normalize(Normal);\n\
  \  vec3 lightDir = normalize(lightPos - FragPos);\n\
  \  float diff = max(dot(norm, lightDir), 0.0);\n\
  \  vec3 diffuse = diff * lightColor;\n\
  \  float specularStrength = 0.5;\n\
  \  vec3 viewDir = normalize(viewPos - FragPos);\n\
  \  vec3 reflectDir = reflect(-lightDir, norm);\n\
  \  float spec = pow(max(dot(viewDir, reflectDir), 0.0), 32);\n\
  \  vec3 specular = specularStrength * spec * lightColor;\n\
  \  vec3 result = (ambient + diffuse + specular) * objectColor;\n\
  \  FragColor = vec4(result, 1.0);\n\
  \}\0"

-- =============================================================================
-- Logging
-- =============================================================================

type Logger = String -> IO ()

mkLogger :: IO Logger
mkLogger = do
  startTime <- getCurrentTime
  return $ \msg -> do
    now <- getCurrentTime
    let elapsed = realToFrac (diffUTCTime now startTime) :: Double
    printf "[OpenGL %.3f] %s\n" elapsed msg

-- =============================================================================
-- MIDI Note Extraction
-- =============================================================================

extractNotes :: Logger -> ZM.MidiFile -> [Int] -> IO [GLNote]
extractNotes log mf selectedTracks = do
  let header = ZM.mf_header mf
  case ZM.mh_time_div header of
    ZM.SMPTE _ _ -> do
      log "ERROR: SMPTE time division not supported"
      pure []
    ZM.PPQN division -> do
      let tracks = ZM.mf_tracks mf
          numTracks = length tracks
          -- Filter out invalid track indices
          validTracks = filter (\i -> i >= 0 && i < numTracks) selectedTracks
          selTracks = [tracks !! i | i <- validTracks]
      if null selTracks
        then do
          log $ "WARNING: No valid MIDI tracks selected (file has " ++ show numTracks ++ " tracks)"
          pure []
        else do
          log $ "Processing " ++ show (length selTracks) ++ " MIDI tracks"
          pure $ computeNotes (fromIntegral division) selTracks

computeNotes :: Double -> [ZM.MidiTrack] -> [GLNote]
computeNotes division tracks = reverse finalNotes'
  where
    -- Compute absolute tick timestamps for each event per track, then merge.
    -- Previously used scanl' with initial value `0`, which cannot unify with
    -- the accumulator type `(Double, MidiEvent)` — a type error.  We now
    -- compute cumulative ticks as a separate Integer scanl and zip with events.
    absEvents track =
      let msgs     = ZM.getTrackMessages track
          cumTicks = tail $ scanl' (\cum (dt, _) -> cum + fromIntegral dt) (0 :: Integer) msgs
      in  zipWith (\t (_, ev) -> (t, ev)) cumTicks msgs

    trackEvents = concatMap absEvents tracks
    allEvents   = sortBy (comparing fst) trackEvents

    initialState :: (Double, Integer, Double, Map.Map (Int, Int) (Double, Int), [GLNote])
    initialState = (0.0, 0, 500000.0, Map.empty, [])

    process (curTime, lastTick, usPQ, opens, notes) (tick, ev) =
      let dTime = fromIntegral (tick - lastTick) * usPQ / 1e6 / division
          t'    = curTime + dTime
      in case ev of
        ZM.MidiMetaEvent (ZM.SetTempo uspq) ->
          (t', tick, fromIntegral uspq, opens, notes)
        ZM.MidiVoiceEvent (ZM.NoteOn chan pitch vel) | vel > 0 ->
          (t', tick, usPQ,
           Map.insert (fromIntegral chan, fromIntegral pitch) (t', fromIntegral vel) opens,
           notes)
        ZM.MidiVoiceEvent (ZM.NoteOn chan pitch 0) ->
          let (o', ns') = closeNote t' (fromIntegral chan) (fromIntegral pitch) opens notes
          in  (t', tick, usPQ, o', ns')
        ZM.MidiVoiceEvent (ZM.NoteOff chan pitch _) ->
          let (o', ns') = closeNote t' (fromIntegral chan) (fromIntegral pitch) opens notes
          in  (t', tick, usPQ, o', ns')
        _ -> (t', tick, usPQ, opens, notes)

    closeNote time chan pitch opens notes = case Map.lookup (chan, pitch) opens of
      Nothing         -> (opens, notes)
      Just (start, v) ->
        ( Map.delete (chan, pitch) opens
        , GLNote start (time - start) pitch v chan : notes
        )

    (_, _, _, finalOpens, finalNotes) = foldl' process initialState allEvents

    -- Close any still-open notes at end-of-file with zero duration.
    finalNotes' = foldl' (\ns ((c,p),(s,v)) -> GLNote s 0 p v c : ns)
                         finalNotes
                         (Map.toList finalOpens)

-- =============================================================================
-- OBJ Model Loading
-- =============================================================================

data Model = Model
  { modelVertices :: ![GLfloat]
  , modelNormals  :: ![GLfloat]
  , modelIndices  :: ![GLuint]
  } deriving (Show)

loadOBJ :: FilePath -> IO (Either String Model)
loadOBJ path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ "OBJ file not found: " ++ path
    else do
      content <- BS.readFile path
      let lines' = BS8.lines content
          (verts, norms, faces) = foldl' parseLine ([], [], []) lines'
      if null verts
        then pure $ Left $ "OBJ file contains no vertices: " ++ path
        else do
          -- verts/norms were prepended during fold, so reverse to restore file order.
          let vertices = concatMap reverse (reverse verts)
              normals  = concatMap reverse (reverse norms)
              indices  = concatMap expandFace faces
          pure $ Right $ Model vertices normals indices
  where
    parseLine (vs, ns, fs) line 
      | BS8.null line || BS8.head line == '#' = (vs, ns, fs)
      | "v " `BS8.isPrefixOf` line = 
          let parts = BS8.words (BS8.drop 2 line)
          in if length parts == 3
             then (map (safeRead . BS8.unpack) parts : vs, ns, fs)
             else (vs, ns, fs)
      | "vn " `BS8.isPrefixOf` line = 
          let parts = BS8.words (BS8.drop 3 line)
          in if length parts == 3
             then (vs, map (safeRead . BS8.unpack) parts : ns, fs)
             else (vs, ns, fs)
      | "f " `BS8.isPrefixOf` line = 
          let parts = BS8.words (BS8.drop 2 line)
          in if length parts >= 3
             then (vs, ns, map parseFacePart (take 3 parts) : fs)
             else (vs, ns, fs)
      | otherwise = (vs, ns, fs)

    safeRead s = case reads s of
      [(x, "")] -> x
      _         -> 0.0

    parseFacePart bs = 
      let parts = BS8.split '/' bs
          v = case parts of
                (vStr:_) -> safeReadInt (BS8.unpack vStr) - 1
                _ -> 0
          n = case parts of
                (_:_:nStr:_) -> safeReadInt (BS8.unpack nStr) - 1
                _ -> 0
      in (v, n)

    safeReadInt s = case reads s of
      [(x, "")] -> x
      _ -> 0

    expandFace [(v1,_), (v2,_), (v3,_)] = [v1, v2, v3]
    expandFace _ = []

setupModel :: Model -> IO (GLuint, GLuint, GLuint, GLuint)
setupModel Model{..} = do
  vao <- alloca $ \p -> glGenVertexArrays 1 p >> peek p
  vboV <- alloca $ \p -> glGenBuffers 1 p >> peek p
  vboN <- alloca $ \p -> glGenBuffers 1 p >> peek p
  ebo <- alloca $ \p -> glGenBuffers 1 p >> peek p

  glBindVertexArray vao

  glBindBuffer GL_ARRAY_BUFFER vboV
  withArray modelVertices $ \ptr -> glBufferData GL_ARRAY_BUFFER (fromIntegral $ length modelVertices * sizeOf (0 :: GLfloat)) (castPtr ptr) GL_STATIC_DRAW
  glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (3 * sizeOf (0 :: GLfloat)) nullPtr
  glEnableVertexAttribArray 0

  glBindBuffer GL_ARRAY_BUFFER vboN
  withArray modelNormals $ \ptr -> glBufferData GL_ARRAY_BUFFER (fromIntegral $ length modelNormals * sizeOf (0 :: GLfloat)) (castPtr ptr) GL_STATIC_DRAW
  glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE (3 * sizeOf (0 :: GLfloat)) nullPtr
  glEnableVertexAttribArray 1

  glBindBuffer GL_ELEMENT_ARRAY_BUFFER ebo
  withArray modelIndices $ \ptr -> glBufferData GL_ELEMENT_ARRAY_BUFFER (fromIntegral $ length modelIndices * sizeOf (0 :: GLuint)) (castPtr ptr) GL_STATIC_DRAW

  glBindVertexArray 0

  pure (vao, vboV, vboN, ebo)

-- =============================================================================
-- SVG Loading
-- =============================================================================

data SVGData = SVGData
  { svgDoc :: !SVG.Document
  } deriving (Show)

loadSVG :: FilePath -> IO (Either String SVGData)
loadSVG path = do
  exists <- doesFileExist path
  if not exists
    then pure $ Left $ "SVG file not found: " ++ path
    else do
      bs <- BS.readFile path
      case SVGTree.parseSVG bs of
        Left err -> pure $ Left $ "SVG parse error: " ++ err
        Right doc -> pure $ Right $ SVGData doc

renderSVG :: NVG.Context -> SVGData -> Float -> Float -> IO ()
renderSVG ctx SVGData{..} _x _y = do
  NVG.beginPath ctx
  forM_ (SVG.elements svgDoc) (renderElement ctx)
  NVG.fillColor ctx (NVG.rgba 255 255 255 255)
  NVG.fill ctx

renderElement :: NVG.Context -> SVG.Element -> IO ()
renderElement ctx (SVG.Path path) = forM_ (SVG.pathCommands path) (renderCommand ctx)
renderElement _ _ = pure ()

renderCommand :: NVG.Context -> SVG.PathCommand -> IO ()
renderCommand ctx (SVG.MoveTo (SVG.Abs, px, py)) = NVG.moveTo ctx px py
renderCommand ctx (SVG.LineTo (SVG.Abs, px, py)) = NVG.lineTo ctx px py
renderCommand ctx SVG.Close = NVG.closePath ctx
renderCommand _ _ = pure ()

-- =============================================================================
-- VTY Rendering in NanoVG
-- =============================================================================

vtyColorToRGB :: VTY.Color -> (Word8, Word8, Word8)
vtyColorToRGB (VTY.ISOColor n) 
  | n < 0     = (0, 0, 0)
  | n < 8     = low n
  | n < 16    = high (n - 8)
  | otherwise = (0, 0, 0)
  where
    low 0 = (0,0,0)
    low 1 = (128,0,0)
    low 2 = (0,128,0)
    low 3 = (128,128,0)
    low 4 = (0,0,128)
    low 5 = (128,0,128)
    low 6 = (0,128,128)
    low 7 = (192,192,192)
    low _ = (0,0,0)
    high 0 = (128,128,128)
    high 1 = (255,0,0)
    high 2 = (0,255,0)
    high 3 = (255,255,0)
    high 4 = (0,0,255)
    high 5 = (255,0,255)
    high 6 = (0,255,255)
    high 7 = (255,255,255)
    high _ = (0,0,0)
vtyColorToRGB (VTY.Color240 code) 
  | code < 0    = (0, 0, 0)
  | code < 16   = vtyColorToRGB (VTY.ISOColor (fromIntegral code))
  | code < 232  = let offset = code - 16
                      r = (offset `div` 36) * 40 + if (offset `div` 36) > 0 then 55 else 0
                      g = ((offset `div` 6) `mod` 6) * 40 + if ((offset `div` 6) `mod` 6) > 0 then 55 else 0
                      b = (offset `mod` 6) * 40 + if (offset `mod` 6) > 0 then 55 else 0
                  in (fromIntegral r, fromIntegral g, fromIntegral b)
  | code < 256  = let gray = fromIntegral $ 8 + (code - 232) * 10 :: Word8
                  in (gray, gray, gray)
  | otherwise   = (0, 0, 0)

vtyAttrFore :: VTY.Attr -> NVG.Color
vtyAttrFore attr = case VTY.attrForeColor attr of
  VTY.KeepCurrent -> NVG.rgba 255 255 255 255
  VTY.Default -> NVG.rgba 255 255 255 255
  VTY.Bright -> NVG.rgba 255 255 255 255
  VTY.Fixed c -> let (r,g,b) = vtyColorToRGB c in NVG.rgba r g b 255

vtyAttrBack :: VTY.Attr -> NVG.Color
vtyAttrBack attr = case VTY.attrBackColor attr of
  VTY.KeepCurrent -> NVG.rgba 0 0 0 255
  VTY.Default -> NVG.rgba 0 0 0 255
  VTY.Bright -> NVG.rgba 0 0 0 255
  VTY.Fixed c -> let (r,g,b) = vtyColorToRGB c in NVG.rgba r g b 255

isBold :: VTY.Attr -> Bool
isBold attr = VTY.bold `elem` VTY.attrStyle attr

drawCell :: NVG.Context -> Float -> Float -> Float -> Float -> Char -> VTY.Attr -> IO ()
drawCell ctx px py cellW cellH c attr = do
  let bg = vtyAttrBack attr
      fg = vtyAttrFore attr
  NVG.fillColor ctx bg
  NVG.beginPath ctx
  NVG.rect ctx px py cellW cellH
  NVG.fill ctx
  NVG.fontFace ctx (if isBold attr then "bold" else "regular")
  NVG.fillColor ctx fg
  NVG.textAlign ctx (NVG.ALIGN_LEFT .|. NVG.ALIGN_TOP)
  BS8.useAsCString (BS8.pack [c]) $ \cstr -> NVG.text ctx px py cstr

renderVtyImage :: NVG.Context -> Float -> Float -> Float -> Float -> VTY.Attr -> VTY.Image -> IO ()
renderVtyImage ctx x y cellW cellH curAttr img = case VTY.imageContent img of
  VTY.EmptyImage -> pure ()
  VTY.Char attr' c -> drawCell ctx x y cellW cellH c (attr' `VTY.with` curAttr)
  VTY.String attr' s -> forM_ (zip [0..] s) $ \(i, c) -> 
    drawCell ctx (x + fromIntegral i * cellW) y cellW cellH c (attr' `VTY.with` curAttr)
  VTY.HorizText i -> renderVtyImage ctx x y cellW cellH curAttr i
  VTY.HorizCat a b -> do
    renderVtyImage ctx x y cellW cellH curAttr a
    renderVtyImage ctx (x + fromIntegral (VTY.imageWidth a) * cellW) y cellW cellH curAttr b
  VTY.VertCat a b -> do
    renderVtyImage ctx x y cellW cellH curAttr a
    renderVtyImage ctx x (y + fromIntegral (VTY.imageHeight a) * cellH) cellW cellH curAttr b
  VTY.Translate dx dy i -> renderVtyImage ctx (x + fromIntegral dx * cellW) (y + fromIntegral dy * cellH) cellW cellH curAttr i
  VTY.Crop _ _ i -> renderVtyImage ctx x y cellW cellH curAttr i
  VTY.Background c attr' -> do
    let attr'' = attr' `VTY.with` curAttr
        w = fromIntegral (VTY.imageWidth img) * cellW
        h = fromIntegral (VTY.imageHeight img) * cellH
    NVG.fillColor ctx (vtyAttrBack attr'')
    NVG.beginPath ctx
    NVG.rect ctx x y w h
    NVG.fill ctx
    when (c /= ' ') $ forM_ [0..VTY.imageHeight img - 1] $ \row ->
      forM_ [0..VTY.imageWidth img - 1] $ \col ->
        drawCell ctx (x + fromIntegral col * cellW) (y + fromIntegral row * cellH) cellW cellH c attr''
  _ -> pure ()

renderVtyPicture :: NVG.Context -> Float -> Float -> Float -> Float -> VTY.Picture -> IO ()
renderVtyPicture ctx x y cellW cellH pic = do
  case VTY.picBackground pic of
    VTY.BackgroundFill bg -> renderVtyImage ctx x y cellW cellH VTY.defAttr bg
  renderVtyImage ctx x y cellW cellH VTY.defAttr (VTY.picImage pic)

-- =============================================================================
-- Shader Compilation
-- =============================================================================

data UniformLocations = UniformLocations
  { ulTime       :: !GLint
  , ulAmp        :: !GLint
  , ulResolution :: !GLint
  , ulMouse      :: !GLint
  } deriving (Show)

data ModelUniforms = ModelUniforms
  { muModel       :: !GLint
  , muView        :: !GLint
  , muProjection  :: !GLint
  , muLightPos    :: !GLint
  , muViewPos     :: !GLint
  , muLightColor  :: !GLint
  , muObjectColor :: !GLint
  } deriving (Show)

getUniformLoc :: GLuint -> String -> IO GLint
getUniformLoc prog name = withCString name $ glGetUniformLocation prog

compileShader :: GLenum -> BS.ByteString -> IO GLuint
compileShader shaderType src = do
  shader <- glCreateShader shaderType
  -- Pass nullPtr for lengths: OpenGL then relies on null-termination.
  -- The previous code misinterpreted the byte-count as a pointer address (UB).
  BS.useAsCString src $ \cstr ->
    withArray [cstr] $ \ptr ->
      glShaderSource shader 1 ptr nullPtr
  glCompileShader shader

  status <- alloca $ \p -> glGetShaderiv shader GL_COMPILE_STATUS p >> peek p
  when (status == GL_FALSE) $ do
    errLog <- allocaArray 1024 $ \buf -> do
      glGetShaderInfoLog shader 1024 nullPtr buf
      peekCString buf
    putStrLn $ "Shader compilation FAILED:\n" ++ errLog

  pure shader

createShaderProgram :: BS.ByteString -> BS.ByteString -> IO GLuint
createShaderProgram vsSrc fsSrc = do
  vs <- compileShader GL_VERTEX_SHADER vsSrc
  fs <- compileShader GL_FRAGMENT_SHADER fsSrc
  prog <- glCreateProgram
  glAttachShader prog vs
  glAttachShader prog fs
  glLinkProgram prog

  linkStatus <- alloca $ \p -> glGetProgramiv prog GL_LINK_STATUS p >> peek p
  when (linkStatus == GL_FALSE) $ do
    logErr <- allocaArray 1024 $ \buf -> do
      glGetProgramInfoLog prog 1024 nullPtr buf
      peekCString buf
    putStrLn $ "Program link FAILED:\n" ++ logErr

  glDeleteShader vs
  glDeleteShader fs

  pure prog

createBackgroundProgram :: ShaderConfig -> Logger -> IO (GLuint, UniformLocations)
createBackgroundProgram ShaderConfig{..} log = do
  vsSrc <- case scVertex of
    Embedded s -> pure s
    FromFile p -> BS.readFile p

  fsSrc <- case scFragment of
    Embedded s -> pure s
    FromFile p -> BS.readFile p

  prog <- createShaderProgram vsSrc fsSrc

  timeLoc  <- getUniformLoc prog "uTime"
  ampLoc   <- getUniformLoc prog "uAmp"
  resLoc   <- getUniformLoc prog "uResolution"
  mouseLoc <- getUniformLoc prog "uMouse"

  when (timeLoc == -1)   $ log "WARNING: uniform uTime not found"
  when (ampLoc == -1)    $ log "WARNING: uniform uAmp not found"
  when (resLoc == -1)    $ log "WARNING: uniform uResolution not found"
  when (mouseLoc == -1)  $ log "WARNING: uniform uMouse not found"

  pure (prog, UniformLocations timeLoc ampLoc resLoc mouseLoc)

createNoteProgram :: IO GLuint
createNoteProgram = createShaderProgram noteVertexSrc noteFragmentSrc

createModelProgram :: IO (GLuint, ModelUniforms)
createModelProgram = do
  prog <- createShaderProgram modelVertexSrc modelFragmentSrc
  modelLoc <- getUniformLoc prog "model"
  viewLoc  <- getUniformLoc prog "view"
  projLoc  <- getUniformLoc prog "projection"
  lightPosLoc <- getUniformLoc prog "lightPos"
  viewPosLoc  <- getUniformLoc prog "viewPos"
  lightColorLoc <- getUniformLoc prog "lightColor"
  objectColorLoc <- getUniformLoc prog "objectColor"
  pure (prog, ModelUniforms modelLoc viewLoc projLoc lightPosLoc viewPosLoc lightColorLoc objectColorLoc)

-- =============================================================================
-- Geometry Setup
-- =============================================================================

setupQuadGeometry :: IO GLuint
setupQuadGeometry = do
  let vertices :: [GLfloat]
      vertices = [-1, -1,  1, -1,  1, 1,  -1, -1,  1, 1,  -1, 1]

  vao <- alloca $ \p -> glGenVertexArrays 1 p >> peek p
  vbo <- alloca $ \p -> glGenBuffers 1 p >> peek p

  glBindVertexArray vao
  glBindBuffer GL_ARRAY_BUFFER vbo
  withArray vertices $ \ptr ->
    glBufferData GL_ARRAY_BUFFER (fromIntegral $ length vertices * sizeOf (0 :: GLfloat))
                 (castPtr ptr) GL_STATIC_DRAW

  glEnableVertexAttribArray 0
  glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE 0 nullPtr

  glBindBuffer GL_ARRAY_BUFFER 0
  glBindVertexArray 0

  pure vao

setupNoteGeometry :: IO (GLuint, GLuint)
setupNoteGeometry = do
  vao <- alloca $ \p -> glGenVertexArrays 1 p >> peek p
  vbo <- alloca $ \p -> glGenBuffers 1 p >> peek p

  glBindVertexArray vao
  glBindBuffer GL_ARRAY_BUFFER vbo
  let maxSize = 1024 * 1024 :: GLsizei
  glBufferData GL_ARRAY_BUFFER maxSize nullPtr GL_DYNAMIC_DRAW

  let stride = 5 * sizeOf (0 :: GLfloat)
  glEnableVertexAttribArray 0
  glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE stride nullPtr
  glEnableVertexAttribArray 1
  glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE stride (nullPtr `plusPtr` (2 * sizeOf (0 :: GLfloat)))

  glBindBuffer GL_ARRAY_BUFFER 0
  glBindVertexArray 0

  pure (vao, vbo)

setupLaneLines :: Int -> IO GLuint
setupLaneLines numLanes = do
  let laneWidth = 2.0 / fromIntegral numLanes :: GLfloat
      vertices = concatMap (\i -> let x = -1.0 + laneWidth * fromIntegral i in
        [x, -1.0, 0.5, 0.5, 0.5, x, 1.0, 0.5, 0.5, 0.5]) [1..numLanes-1]

  vao <- alloca $ \p -> glGenVertexArrays 1 p >> peek p
  vbo <- alloca $ \p -> glGenBuffers 1 p >> peek p

  glBindVertexArray vao
  glBindBuffer GL_ARRAY_BUFFER vbo
  withArray vertices $ \ptr ->
    glBufferData GL_ARRAY_BUFFER (fromIntegral $ length vertices * sizeOf (0 :: GLfloat))
                 (castPtr ptr) GL_STATIC_DRAW

  let stride = 5 * sizeOf (0 :: GLfloat)
  glEnableVertexAttribArray 0
  glVertexAttribPointer 0 2 GL_FLOAT GL_FALSE stride nullPtr
  glEnableVertexAttribArray 1
  glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE stride (nullPtr `plusPtr` (2 * sizeOf (0 :: GLfloat)))

  glBindBuffer GL_ARRAY_BUFFER 0
  glBindVertexArray 0

  pure vao

-- =============================================================================
-- Application State
-- =============================================================================

data AppState = AppState
  { asWindow         :: !GLFW.Window
  , asBackgroundProg :: !(IORef GLuint)
  , asUniforms       :: !(IORef UniformLocations)
  , asQuadVAO        :: !GLuint
  , asNoteProgram    :: !GLuint
  , asNoteVAO        :: !GLuint
  , asNoteVBO        :: !GLuint
  , asLaneVAO        :: !GLuint
  , asModelProgram   :: !GLuint
  , asModelUniforms  :: !ModelUniforms
  , asModelVAO       :: !GLuint
  , asRenderState    :: !(TVar GLRenderState)   -- Changed from AudioState
  , asReactorState   :: !(Maybe (TVar ReactorState))  -- Optional backend connection
  , asTuiPicture     :: !(TVar VTY.Picture)
  , asMousePos       :: !(IORef (Double, Double))
  , asResolution     :: !(IORef (Int, Int))
  , asLastTime       :: !(IORef Double)
  , asFrameCount     :: !(IORef Int)
  , asLastFPSPrint   :: !(IORef UTCTime)
  , asLogger         :: !Logger
  , asShaderConfig   :: !(IORef ShaderConfig)
  , asNotes          :: !(IORef [GLNote])
  , asPreviewSeconds :: !Double
  , asNumLanes       :: !Int
  , asColors         :: ![(GLfloat, GLfloat, GLfloat)]
  , asToggleNotes    :: !(IORef Bool)
  , asToggle3D       :: !(IORef Bool)
  , asToggleSVG      :: !(IORef Bool)
  , asToggleTUI      :: !(IORef Bool)
  , asTracks         :: !(IORef [Int])
  , asMidiFile       :: !(IORef (Maybe FilePath))
  , asObjFile        :: !(IORef (Maybe FilePath))
  , asSvgFile        :: !(IORef (Maybe FilePath))
  , asNvgContext     :: !NVG.Context
  , asSvgData        :: !(IORef (Maybe SVGData))
  , asModel          :: !(IORef (Maybe Model))
  , asModelRotation  :: !(IORef Float)
  , asCellW          :: !Float
  , asCellH          :: !Float
  , asRunning        :: !(IORef Bool)
  , asIntroState     :: !(IORef IntroState)
  , asSkipIntro      :: !(IORef Bool)
  , asTriangle       :: !([String])
  , asTriDepth       :: !Int
  , asTimeLabel      :: !String
  , asAudioPlayer    :: !(IORef AudioPlayer)
  , asAudioPlayerCmd :: !(IORef (Maybe (String, [String])))  -- per-instance player cache
  }

-- =============================================================================
-- Shader Reload
-- =============================================================================

reloadShaders :: AppState -> IO ()
reloadShaders AppState{..} = do
  oldProg <- readIORef asBackgroundProg
  cfg <- readIORef asShaderConfig
  asLogger "Reloading background shaders (F5)..."

  (newProg, newUL) <- createBackgroundProgram cfg asLogger

  writeIORef asBackgroundProg newProg
  writeIORef asUniforms newUL

  glDeleteProgram oldProg

  asLogger "Background shaders reloaded."

-- =============================================================================
-- Callbacks
-- =============================================================================

keyCallback :: AppState -> GLFW.KeyCallback
keyCallback st _win key _scan action _mods = do
  -- Handle intro skip
  intro <- readIORef (asIntroState st)
  when (introPhase intro /= IntroComplete && 
        key == GLFW.Key'Space && 
        action == GLFW.KeyState'Pressed) $
    skipIntro st
  
  -- Handle normal keys
  when (introPhase intro == IntroComplete) $ do
    when (key == GLFW.Key'Escape && action == GLFW.KeyState'Pressed) $
      GLFW.setWindowShouldClose (asWindow st) True

    when (key == GLFW.Key'F5 && action == GLFW.KeyState'Pressed) $
      reloadShaders st

resizeCallback :: AppState -> GLFW.FramebufferSizeCallback
resizeCallback AppState{..} _win w h = do
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  writeIORef asResolution (fromIntegral w, fromIntegral h)

cursorPosCallback :: AppState -> GLFW.CursorPosCallback
cursorPosCallback AppState{..} _win x y = do
  (_, h) <- readIORef asResolution
  writeIORef asMousePos (x, fromIntegral h - y)

closeCallback :: GLFW.WindowCloseCallback
closeCallback win = GLFW.setWindowShouldClose win True

-- =============================================================================
-- Intro Rendering
-- =============================================================================

renderIntroPhase :: AppState -> Int -> Int -> IO ()
renderIntroPhase st@AppState{..} width height = do
  intro <- readIORef asIntroState
  skip  <- readIORef asSkipIntro
  
  let ctx = asNvgContext
  
  -- Clear with black
  NVG.beginPath ctx
  NVG.rect ctx 0 0 (fromIntegral width) (fromIntegral height)
  NVG.fillColor ctx (NVG.rgb 0 0 0)
  NVG.fill ctx
  
  -- Set global alpha for fade effects
  NVG.globalAlpha ctx (fadeAlpha intro)
  
  case introPhase intro of
    IntroPresents -> renderPresents st intro width height
    IntroReveal   -> renderReveal st intro width height
    IntroAstart   -> renderAstart st intro width height
    IntroFadeOut  -> renderAstart st intro width height
    IntroComplete -> pure ()
  
  -- Render "Press SPACE to skip" (always visible during intro)
  NVG.globalAlpha ctx 1.0
  renderSkipPrompt st width height

renderPresents :: AppState -> IntroState -> Int -> Int -> IO ()
renderPresents st intro w h = do
  let ctx = asNvgContext st
      centerX = fromIntegral w / 2
      centerY = fromIntegral h / 2
  
  -- Draw ASCII art with monospace font
  NVG.fontFace ctx "monospace"
  NVG.textAlign ctx (NVG.ALIGN_CENTER .|. NVG.ALIGN_MIDDLE)
  
  let lineHeight = 14.0
      startY = centerY - fromIntegral (length presentsLines) * lineHeight / 2
  
  forM_ (zip [0..] presentsLines) $ \(i, line) -> do
    let charsToReveal = max 0 (min (length line) (charCount intro - sum (map length (take i presentsLines))))
        revealedLine = take charsToReveal line
        y = startY + fromIntegral i * lineHeight
    
    -- Color gradient based on row
    let color = if i < 7
                then NVG.rgba 0 255 255 (floor (fadeAlpha intro * 255))
                else NVG.rgba 200 200 200 (floor (fadeAlpha intro * 255))
    NVG.fillColor ctx color
    NVG.fontSize ctx 10.0
    NVG.text ctx centerX y (BS8.pack revealedLine)
  
  -- Draw "seeding from system time..."
  NVG.fillColor ctx (NVG.rgba 100 100 100 (floor (fadeAlpha intro * 255)))
  NVG.fontSize ctx 14.0
  NVG.text ctx centerX (startY + fromIntegral (length presentsLines + 1) * lineHeight) 
           "  seeding from system time..."

renderReveal :: AppState -> IntroState -> Int -> Int -> IO ()
renderReveal st intro w h = do
  let ctx = asNvgContext st
      triangle = asTriangle st
      revealed = scanRow intro
      centerX = fromIntegral w / 2
      startY = fromIntegral h / 2 - fromIntegral (length triangle) * 8
  
  NVG.fontFace ctx "monospace"
  NVG.textAlign ctx (NVG.ALIGN_CENTER .|. NVG.ALIGN_MIDDLE)
  NVG.fontSize ctx 10.0
  
  forM_ (take revealed (zip [0..] triangle)) $ \(i, row) -> do
    let y = startY + fromIntegral i * 14.0
        -- Color based on row position
        pct = fromIntegral i / fromIntegral (max 1 (length triangle))
        color | pct < 0.33 = NVG.rgba 0 255 255 (floor (fadeAlpha intro * 255))
              | pct < 0.66 = NVG.rgba 0 100 255 (floor (fadeAlpha intro * 255))
              | otherwise  = NVG.rgba 255 0 255 (floor (fadeAlpha intro * 255))
    NVG.fillColor ctx color
    NVG.text ctx centerX y (BS8.pack row)
  
  -- Draw footer
  NVG.fillColor ctx (NVG.rgba 100 100 100 (floor (fadeAlpha intro * 255)))
  NVG.fontSize ctx 12.0
  let footerY = startY + fromIntegral (length triangle + 1) * 14.0
  NVG.text ctx centerX footerY (BS8.pack $ "  seed: " ++ asTimeLabel st ++ "  │  depth: " ++ show (asTriDepth st))

renderAstart :: AppState -> IntroState -> Int -> Int -> IO ()
renderAstart st intro w h = do
  let ctx = asNvgContext st
      centerX = fromIntegral w / 2
      centerY = fromIntegral h / 2
  
  -- Draw Astart banner
  NVG.fontFace ctx "monospace"
  NVG.textAlign ctx (NVG.ALIGN_CENTER .|. NVG.ALIGN_MIDDLE)
  NVG.fillColor ctx (NVG.rgba 255 255 0 (floor (fadeAlpha intro * 255)))
  NVG.fontSize ctx 10.0
  
  let astartY = centerY - fromIntegral (length astartLines + 5) * 7
  forM_ (zip [0..] astartLines) $ \(i, line) -> do
    let y = astartY + fromIntegral i * 14.0
    NVG.text ctx centerX y (BS8.pack line)
  
  -- Draw Sierpinski triangle below
  let triangle = asTriangle st
      triY = astartY + fromIntegral (length astartLines + 2) * 14.0
  
  NVG.fontSize ctx 10.0
  forM_ (zip [0..] triangle) $ \(i, row) -> do
    let y = triY + fromIntegral i * 12.0
        pct = fromIntegral i / fromIntegral (max 1 (length triangle))
        color | pct < 0.33 = NVG.rgba 0 255 255 (floor (fadeAlpha intro * 255))
              | pct < 0.66 = NVG.rgba 0 100 255 (floor (fadeAlpha intro * 255))
              | otherwise  = NVG.rgba 255 0 255 (floor (fadeAlpha intro * 255))
    NVG.fillColor ctx color
    NVG.text ctx centerX y (BS8.pack row)
  
  -- Draw footer
  NVG.fillColor ctx (NVG.rgba 100 100 100 (floor (fadeAlpha intro * 255)))
  NVG.fontSize ctx 12.0
  let footerY = triY + fromIntegral (length triangle + 1) * 12.0
  NVG.text ctx centerX footerY (BS8.pack $ "  seed: " ++ asTimeLabel st ++ "  │  depth: " ++ show (asTriDepth st))
  
  -- Draw hint
  NVG.fillColor ctx (NVG.rgba 255 255 255 (floor (fadeAlpha intro * 255)))
  NVG.text ctx centerX (footerY + 20) (BS8.pack "  [q / ESC]  quit")

renderSkipPrompt :: AppState -> Int -> Int -> IO ()
renderSkipPrompt st w h = do
  let ctx = asNvgContext st
  NVG.fontSize ctx 16.0
  NVG.fontFace ctx "monospace"
  NVG.textAlign ctx (NVG.ALIGN_CENTER .|. NVG.ALIGN_BOTTOM)
  NVG.fillColor ctx (NVG.rgba 150 150 150 255)
  NVG.text ctx (fromIntegral w / 2) (fromIntegral h - 20) (BS8.pack "Press SPACE to skip")

-- =============================================================================
-- Intro State Machine
-- =============================================================================

updateIntroState :: AppState -> IO Bool
updateIntroState st@AppState{..} = do
  intro <- readIORef asIntroState
  skip  <- readIORef asSkipIntro
  
  if skip
    then do
      -- Skip intro immediately
      writeIORef asIntroState (intro { introPhase = IntroComplete, fadeAlpha = 0.0 })
      return True
    else do
      let newTick = introTick intro + 1
      
      newIntro <- case introPhase intro of
        IntroPresents -> do
          let charsPerFrame = 2  -- Reveal 2 chars per frame
              newCharCount = charCount intro + charsPerFrame
              totalChars = sum (map length presentsLines)
          
          if newTick >= presentsDuration || newCharCount >= totalChars
            then return intro { introPhase = IntroReveal
                              , introTick = 0
                              , charCount = newCharCount }
            else return intro { introTick = newTick
                              , charCount = newCharCount }
        
        IntroReveal -> do
          let rowsPerFrame = 1  -- Reveal 1 row per frame
              totalRows = length asTriangle
              newScanRow = min totalRows (scanRow intro + rowsPerFrame)
          
          if newTick >= revealDuration || newScanRow >= totalRows
            then return intro { introPhase = IntroAstart
                              , introTick = 0
                              , scanRow = newScanRow }
            else return intro { introTick = newTick
                              , scanRow = newScanRow }
        
        IntroAstart -> do
          if newTick >= astartDuration
            then return intro { introPhase = IntroFadeOut
                              , introTick = 0 }
            else return intro { introTick = newTick }
        
        IntroFadeOut -> do
          let fadeProgress = fromIntegral newTick / fromIntegral fadeOutDuration
              newAlpha = max 0.0 (1.0 - fadeProgress)
          
          if newTick >= fadeOutDuration
            then return intro { introPhase = IntroComplete
                              , fadeAlpha = 0.0 }
            else return intro { introTick = newTick
                              , fadeAlpha = newAlpha }
        
        IntroComplete -> return intro
      
      writeIORef asIntroState newIntro
      return (introPhase newIntro == IntroComplete)

skipIntro :: AppState -> IO ()
skipIntro st@AppState{..} = do
  intro <- readIORef asIntroState
  unless (introPhase intro == IntroComplete) $ do
    writeIORef asSkipIntro True
    -- Fade out audio
    player <- readIORef asAudioPlayer
    fadeOutAndStopAudio player

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond act = cond >>= \b -> when b act

renderMenu :: AppState -> IO ()
renderMenu AppState{..} = do
  ImGui.beginMainMenuBar
  whenM (ImGui.beginMenu "File") $ do
    whenM (ImGui.menuItem "Load MIDI") $ asLogger "Load MIDI (implement dialog)"
    whenM (ImGui.menuItem "Load OBJ") $ asLogger "Load OBJ (implement dialog)"
    whenM (ImGui.menuItem "Load SVG") $ asLogger "Load SVG (implement dialog)"
    whenM (ImGui.menuItem "Exit") $ GLFW.setWindowShouldClose asWindow True
    ImGui.endMenu

  whenM (ImGui.beginMenu "View") $ do
    toggleNotes <- readIORef asToggleNotes
    whenM (ImGui.checkbox "Show Notes" toggleNotes) $ modifyIORef' asToggleNotes not
    toggle3D <- readIORef asToggle3D
    whenM (ImGui.checkbox "Show 3D Model" toggle3D) $ modifyIORef' asToggle3D not
    toggleSVG <- readIORef asToggleSVG
    whenM (ImGui.checkbox "Show SVG" toggleSVG) $ modifyIORef' asToggleSVG not
    toggleTUI <- readIORef asToggleTUI
    whenM (ImGui.checkbox "Show TUI Window" toggleTUI) $ modifyIORef' asToggleTUI not
    ImGui.endMenu

  whenM (ImGui.beginMenu "Tracks") $ do
    tracks <- readIORef asTracks
    forM_ [0..15] $ \i -> do
      let selected = i `elem` tracks
      whenM (ImGui.checkbox ("Track " ++ show i) selected) $ do
        if selected
          then modifyIORef' asTracks (filter (/=i))
          else modifyIORef' asTracks (i:)
        mbMidi <- readIORef asMidiFile
        case mbMidi of
          Nothing -> pure ()
          Just p -> do
            bs <- BS.readFile p
            case ZM.parseMidi bs of
              Left err -> asLogger $ "Reload MIDI error: " ++ show err
              Right mf -> do
                newNotes <- extractNotes asLogger mf =<< readIORef asTracks
                writeIORef asNotes newNotes
    ImGui.endMenu

  ImGui.endMainMenuBar

-- =============================================================================
-- Rendering
-- =============================================================================

perspective :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> [GLfloat]
perspective fovy aspect zNear zFar = let f = 1 / tan (fovy / 2 * pi / 180)
  in [f / aspect, 0, 0, 0,
      0, f, 0, 0,
      0, 0, (zFar + zNear) / (zNear - zFar), -1,
      0, 0, (2 * zFar * zNear) / (zNear - zFar), 0]

renderFrame :: AppState -> IO ()
renderFrame AppState{..} = do
  GLFW.pollEvents

  -- Get intro state
  intro <- readIORef asIntroState
  introComplete <- if introPhase intro == IntroComplete
                     then return True
                     else updateIntroState AppState{..}

  -- Get state from ReactorState if connected, otherwise use local GLRenderState
  renderState <- case asReactorState of
    Nothing -> readTVarIO asRenderState
    Just reactorVar -> do
      rs <- readTVarIO reactorVar
      pure $ fromReactorState rs

  let !audioTime = glrsTime renderState
      !audioAmp = glrsAmplitude renderState

  glClearColor 0.0 0.0 0.02 1.0
  glClear (GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT)

  if not introComplete
    then do
      -- Render intro
      (resW, resH) <- readIORef asResolution
      
      NVG.beginFrame asNvgContext (fromIntegral resW) (fromIntegral resH) 1.0
      renderIntroPhase AppState{..} resW resH
      NVG.endFrame asNvgContext
    else do
      -- Render normal visualization
      prog <- readIORef asBackgroundProg
      ul   <- readIORef asUniforms
      (resW, resH) <- readIORef asResolution
      (mx, my) <- readIORef asMousePos

      glUseProgram prog
      when (ulTime ul /= -1) $ glUniform1f (ulTime ul) (realToFrac audioTime)
      when (ulAmp ul /= -1) $ glUniform1f (ulAmp ul) audioAmp
      when (ulResolution ul /= -1) $ glUniform2f (ulResolution ul) (fromIntegral resW) (fromIntegral resH)
      when (ulMouse ul /= -1) $ glUniform2f (ulMouse ul) (realToFrac mx) (realToFrac my)

      glBindVertexArray asQuadVAO
      glDrawArrays GL_TRIANGLES 0 6
      glBindVertexArray 0

      -- Render detected note from backend
      renderDetectedNote renderState asNvgContext resW resH

      toggleNotes <- readIORef asToggleNotes
      when toggleNotes $ do
        notes <- readIORef asNotes
        when (not $ null notes) $ do
          glUseProgram asNoteProgram

          glBindVertexArray asLaneVAO
          glDrawArrays GL_LINES 0 (fromIntegral $ (asNumLanes - 1) * 2)
          glBindVertexArray 0

          let preview = asPreviewSeconds
              visible = filter (\n -> glnStart n > audioTime - 0.5 && glnStart n < audioTime + preview) notes
              noteWidth = 2.0 / fromIntegral asNumLanes * 0.8 :: GLfloat
              laneWidth = 2.0 / fromIntegral asNumLanes :: GLfloat

              buildVertices n = let lane = glnPitch n `mod` asNumLanes
                                    (r,g,b) = asColors !! min lane (length asColors - 1)
                                    xCenter = -1.0 + laneWidth * (fromIntegral lane + 0.5)
                                    left = xCenter - noteWidth / 2
                                    right = xCenter + noteWidth / 2
                                    distHead = glnStart n - audioTime
                                    distTail = distHead + max minNoteHeight (glnDur n)
                                    yHead = -1.0 + 2.0 * (realToFrac distHead / realToFrac preview) :: GLfloat
                                    yTail = -1.0 + 2.0 * (realToFrac distTail / realToFrac preview) :: GLfloat
                                    yH = max (-1.0) (min 1.0 yHead)
                                    yT = max (-1.0) (min 1.0 yTail)
                                in if yT <= yH || yH > 1.0 || yT < -1.0 then [] else
                                    [left, yH, r, g, b, right, yH, r, g, b, left, yT, r, g, b,
                                     left, yT, r, g, b, right, yH, r, g, b, right, yT, r, g, b]

              vertices = concatMap buildVertices visible

          unless (null vertices) $ do
            glBindBuffer GL_ARRAY_BUFFER asNoteVBO
            withArray vertices $ \ptr ->
              glBufferSubData GL_ARRAY_BUFFER 0 (fromIntegral $ length vertices * sizeOf (0 :: GLfloat)) (castPtr ptr)

            glBindVertexArray asNoteVAO
            -- Each vertex = [x, y, r, g, b] = 5 floats; divide accordingly.
            glDrawArrays GL_TRIANGLES 0 (fromIntegral $ length vertices `div` 5)
            glBindVertexArray 0

      toggle3D <- readIORef asToggle3D
      when toggle3D $ do
        mbModel <- readIORef asModel
        case mbModel of
          Nothing -> pure ()
          Just model -> do
            glEnable GL_DEPTH_TEST
            glUseProgram asModelProgram

            let mu = asModelUniforms
            rot <- readIORef asModelRotation
            modifyIORef' asModelRotation (+0.01)

            let modelMat = [cos rot, 0, sin rot, 0, 0, 1, 0, 0, -sin rot, 0, cos rot, 0, 0, 0, 0, 1] :: [GLfloat]
                viewMat = [1,0,0,0, 0,1,0,0, 0,0,1,0, 0,0,-3,1]
                projMat = perspective 45.0 (fromIntegral resW / fromIntegral resH) 0.1 100.0
                lightPos = [2.0, 3.0, -1.0] :: [GLfloat]
                viewPos = [0,0,3] :: [GLfloat]
                lightColor = [1,1,1] :: [GLfloat]
                objectColor = [1,0.5,0.31] :: [GLfloat]

            withArray modelMat $ \ptr -> glUniformMatrix4fv (muModel mu) 1 GL_FALSE ptr
            withArray viewMat $ \ptr -> glUniformMatrix4fv (muView mu) 1 GL_FALSE ptr
            withArray projMat $ \ptr -> glUniformMatrix4fv (muProjection mu) 1 GL_FALSE ptr
            withArray lightPos $ \ptr -> glUniform3fv (muLightPos mu) 1 ptr
            withArray viewPos $ \ptr -> glUniform3fv (muViewPos mu) 1 ptr
            withArray lightColor $ \ptr -> glUniform3fv (muLightColor mu) 1 ptr
            withArray objectColor $ \ptr -> glUniform3fv (muObjectColor mu) 1 ptr

            glBindVertexArray asModelVAO
            glDrawElements GL_TRIANGLES (fromIntegral $ length (modelIndices model)) GL_UNSIGNED_INT nullPtr
            glBindVertexArray 0
            glDisable GL_DEPTH_TEST

      NVG.beginFrame asNvgContext (fromIntegral resW) (fromIntegral resH) 1.0

      toggleSVG <- readIORef asToggleSVG
      when toggleSVG $ do
        mbSvg <- readIORef asSvgData
        case mbSvg of
          Nothing -> pure ()
          Just svg -> renderSVG asNvgContext svg 100 100

      toggleTUI <- readIORef asToggleTUI
      when toggleTUI $ do
        pic <- readTVarIO asTuiPicture
        let imgW = VTY.imageWidth (VTY.picImage pic)
            imgH = VTY.imageHeight (VTY.picImage pic)
        when (imgW > 0 && imgH > 0) $ do
          let winX = 50 :: Float
              winY = fromIntegral resH - 50 - asCellH * fromIntegral imgH
              winW = asCellW * fromIntegral imgW
              winH = asCellH * fromIntegral imgH
          NVG.strokeColor asNvgContext (NVG.rgba 255 255 255 128)
          NVG.strokeWidth asNvgContext 2.0
          NVG.beginPath asNvgContext
          NVG.rect asNvgContext winX winY winW winH
          NVG.stroke asNvgContext

          NVG.scissor asNvgContext winX winY winW winH
          renderVtyPicture asNvgContext (winX + 4) (winY + 4) asCellW asCellH pic
          NVG.resetScissor asNvgContext

      NVG.endFrame asNvgContext

      -- Only render ImGui menu after intro
      ImGuiGLFW.newFrame
      ImGui.newFrame
      renderMenu AppState{..}
      ImGui.render
      ImGuiGL3.renderDrawData =<< ImGui.getDrawData

  GLFW.swapBuffers asWindow

-- | Render the currently detected note (or a listening indicator) from backend state.
renderDetectedNote :: GLRenderState -> NVG.Context -> Int -> Int -> IO ()
renderDetectedNote state ctx resW resH = do
  let cx = fromIntegral resW / 2
      cy = fromIntegral resH / 2
  case glrsCurrentNote state of
    Nothing -> do
      -- Show a subtle "listening" indicator when no note is active.
      let conf  = glrsConfidence state
          alpha = floor (min 1.0 (realToFrac conf) * 180) :: Word8
      NVG.fontSize ctx 18.0
      NVG.fontFace ctx "regular"
      NVG.fillColor ctx (NVG.rgba 120 120 120 alpha)
      NVG.textAlign ctx (NVG.ALIGN_CENTER .|. NVG.ALIGN_MIDDLE)
      NVG.text ctx cx cy (BS8.pack "· · ·")
    Just (note, vel) -> do
      let noteName = noteNamesGL !! (note `mod` 12)
          octave   = note `div` 12 - 1
          text     = noteName ++ show octave
          velBar   = replicate (vel `div` 10) '█' ++ replicate (12 - vel `div` 10) '░'
          -- Color by confidence: green (high) → yellow (mid) → red (low)
          conf     = realToFrac (glrsConfidence state) :: Float
          (nr,ng,nb) | conf >= 0.75 = (0,   255, 128)
                     | conf >= 0.40 = (255, 200, 0  )
                     | otherwise    = (255, 60,  60 )
      NVG.fontSize ctx 48.0
      NVG.fontFace ctx "bold"
      NVG.fillColor ctx (NVG.rgba nr ng nb 255)
      NVG.textAlign ctx (NVG.ALIGN_CENTER .|. NVG.ALIGN_MIDDLE)
      NVG.text ctx cx cy (BS8.pack text)
      NVG.fontSize ctx 24.0
      NVG.fillColor ctx (NVG.rgba nr ng nb 200)
      NVG.text ctx cx (cy + 40) (BS8.pack $ "vel: " ++ velBar)
      -- BPM in top-right
      NVG.fontSize ctx 18.0
      NVG.fillColor ctx (NVG.rgba 255 255 255 200)
      NVG.textAlign ctx (NVG.ALIGN_RIGHT .|. NVG.ALIGN_TOP)
      NVG.text ctx (fromIntegral resW - 20) 20
               (BS8.pack $ "BPM: " ++ show (round (glrsBPM state) :: Int))
  where
    noteNamesGL = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]

-- =============================================================================
-- Main Loop
-- =============================================================================

mainLoop :: AppState -> IO ()
mainLoop st@AppState{..} = do
  shouldClose <- GLFW.windowShouldClose asWindow
  running <- readIORef asRunning
  unless (shouldClose || not running) $ do
    renderFrame st

    now  <- GLFW.getTime
    last <- readIORef asLastTime
    let dt = now - last
    -- Always advance the timer so per-frame dt is accurate.
    writeIORef asLastTime now
    modifyIORef' asFrameCount (+1)

    -- Print FPS at most once per second (accumulate over ~0.25 s windows).
    when (dt > 0.25) $ do
      frames <- readIORef asFrameCount
      let fps = fromIntegral frames / dt :: Double
      modifyIORef' asFrameCount (const 0)

      lastPrint <- readIORef asLastFPSPrint
      nowWall <- getCurrentTime
      when (diffUTCTime nowWall lastPrint > 1.0) $ do
        asLogger $ "FPS: " ++ show (round fps :: Int)
        writeIORef asLastFPSPrint nowWall

    mainLoop st

  -- Cleanup on exit
  when shouldClose $
    asLogger "Shutting down OpenGL..."

-- =============================================================================
-- Entry Points
-- =============================================================================

-- | Run OpenGL visualizer connected to backend state (for multi-threaded use)
-- This should be spawned with forkOS for GL context isolation
runOpenGLVisualizer :: OpenGLConfig -> TVar ReactorState -> IO ()
runOpenGLVisualizer cfg reactorVar = do
  logger <- mkLogger
  logger "Starting OpenGL visualizer (connected to backend)..."

  -- Initialize GLFW (must be on same thread as OpenGL context)
  GLFW.setErrorCallback $ Just (\_ desc -> logger $ "GLFW error: " ++ desc)
  ok <- GLFW.init
  unless ok $ do
    logger "Failed to initialize GLFW"
    exitFailure

  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMajor 4
  GLFW.windowHint $ GLFW.WindowHint'ContextVersionMinor 1
  GLFW.windowHint $ GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core
  GLFW.windowHint $ GLFW.WindowHint'OpenGLForwardCompat True
  GLFW.windowHint $ GLFW.WindowHint'Resizable True
  GLFW.windowHint $ GLFW.WindowHint'Samples 4

  mbWin <- GLFW.createWindow (oglWidth cfg) (oglHeight cfg) (oglTitle cfg) Nothing Nothing
  win <- case mbWin of
    Just w  -> pure w
    Nothing -> do
      logger "Failed to create GLFW window"
      GLFW.terminate
      exitFailure

  GLFW.makeContextCurrent (Just win)
  GLFW.swapInterval (if oglNoVSync cfg then 0 else 1)

  (w, h) <- GLFW.getFramebufferSize win
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  glEnable GL_MULTISAMPLE
  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

  (bgProg, bgUL) <- createBackgroundProgram (oglShaderConfig cfg) logger
  noteProg <- createNoteProgram
  (modelProg, modelUL) <- createModelProgram

  quadVAO <- setupQuadGeometry
  (noteVAO, noteVBO) <- setupNoteGeometry
  laneVAO <- setupLaneLines defaultNumLanes

  mbModel <- case oglObjFile cfg of
    Nothing -> pure Nothing
    Just p -> do
      result <- loadOBJ p
      case result of
        Left err -> do
          logger $ "OBJ load error: " ++ err
          pure Nothing
        Right model -> pure (Just model)
  modelVAO <- case mbModel of
    Nothing -> pure 0
    Just m -> do
      (vao, _, _, _) <- setupModel m
      pure vao

  mbSvg <- case oglSvgFile cfg of
    Nothing -> pure Nothing
    Just p -> do
      result <- loadSVG p
      case result of
        Left err -> do
          logger $ "SVG load error: " ++ err
          pure Nothing
        Right svg -> pure (Just svg)

  notes <- case oglMidiFile cfg of
    Nothing -> pure []
    Just p -> do
      bs <- BS.readFile p
      case ZM.parseMidi bs of
        Left err -> logger ("MIDI parse error: " ++ show err) >> pure []
        Right mf -> extractNotes logger mf (oglTracks cfg)

  logger $ "Extracted " ++ show (length notes) ++ " notes from MIDI"

  nvgCtx <- NVG.createGL3 (NVG.Antialias .|. NVG.StencilStrokes)
  loadFonts nvgCtx (oglFontRegular cfg) (oglFontBold cfg)
  -- Also create monospace font for intro
  mbMonoPath <- findFont "DejaVuSansMono.ttf"
  case mbMonoPath of
    Nothing -> putStrLn "Warning: Could not find monospace font for intro"
    Just path -> do
      result <- NVG.createFont nvgCtx "monospace" path
      case result of
        NVG.FontHandle _ -> pure ()
        _ -> putStrLn $ "Warning: Failed to load monospace font from: " ++ path
  NVG.fontSize nvgCtx 12.0
  NVG.fontFace nvgCtx "regular"
  (_, _, lineH) <- NVG.textMetrics nvgCtx
  (_, cellW) <- NVG.textBounds nvgCtx 0 0 "W" nullPtr
  let cellH = lineH

  ImGui.createContext
  ImGuiGL3.init True
  ImGuiGLFW.initForOpenGL win True

  -- Create local render state TVar
  renderTV <- newTVarIO $ GLRenderState 0.0 0.5 Nothing [] 0.0 [] 120.0

  -- Initialize intro state and triangle
  now <- getCurrentTime
  let (depth, fillChar, timeLabel) = timeToDepthAndChar now
      triangle = renderSierpinski depth fillChar
      initIntroState = IntroState
        { introPhase = IntroPresents
        , introTick = 0
        , charCount = 0
        , scanRow = 0
        , fadeAlpha = 1.0
        }

  bgProgRef <- newIORef bgProg
  ulRef <- newIORef bgUL
  mouseRef <- newIORef (0, 0)
  resRef <- newIORef (fromIntegral w, fromIntegral h)
  lastTimeRef <- newIORef =<< GLFW.getTime
  frameRef <- newIORef 0
  fpsPrintRef <- newIORef =<< getCurrentTime
  shaderCfgRef <- newIORef (oglShaderConfig cfg)
  notesRef <- newIORef notes
  toggleNotesRef <- newIORef True
  toggle3DRef <- newIORef (isJust (oglObjFile cfg))
  toggleSVGRef <- newIORef (isJust (oglSvgFile cfg))
  toggleTUIRef <- newIORef True
  tracksRef <- newIORef (oglTracks cfg)
  midiFileRef <- newIORef (oglMidiFile cfg)
  objFileRef <- newIORef (oglObjFile cfg)
  svgFileRef <- newIORef (oglSvgFile cfg)
  svgDataRef <- newIORef mbSvg
  modelRef <- newIORef mbModel
  rotationRef <- newIORef 0.0
  runningRef <- newIORef True
  tuiTV <- newTVarIO VTY.emptyPicture
  introStateRef <- newIORef initIntroState
  skipIntroRef <- newIORef False
  audioPlayerRef    <- newIORef NoAudio
  audioPlayerCmdRef <- newIORef Nothing

  let colors = defaultLaneColors
      appState = AppState
        { asWindow = win
        , asBackgroundProg = bgProgRef
        , asUniforms = ulRef
        , asQuadVAO = quadVAO
        , asNoteProgram = noteProg
        , asNoteVAO = noteVAO
        , asNoteVBO = noteVBO
        , asLaneVAO = laneVAO
        , asModelProgram = modelProg
        , asModelUniforms = modelUL
        , asModelVAO = modelVAO
        , asRenderState = renderTV
        , asReactorState = Just reactorVar  -- Connect to backend!
        , asTuiPicture = tuiTV
        , asMousePos = mouseRef
        , asResolution = resRef
        , asLastTime = lastTimeRef
        , asFrameCount = frameRef
        , asLastFPSPrint = fpsPrintRef
        , asLogger = logger
        , asShaderConfig = shaderCfgRef
        , asNotes = notesRef
        , asPreviewSeconds = defaultPreviewSeconds
        , asNumLanes = defaultNumLanes
        , asColors = colors
        , asToggleNotes = toggleNotesRef
        , asToggle3D = toggle3DRef
        , asToggleSVG = toggleSVGRef
        , asToggleTUI = toggleTUIRef
        , asTracks = tracksRef
        , asMidiFile = midiFileRef
        , asObjFile = objFileRef
        , asSvgFile = svgFileRef
        , asNvgContext = nvgCtx
        , asSvgData = svgDataRef
        , asModel = modelRef
        , asModelRotation = rotationRef
        , asCellW = cellW
        , asCellH = cellH
        , asRunning = runningRef
        , asIntroState = introStateRef
        , asSkipIntro = skipIntroRef
        , asTriangle = triangle
        , asTriDepth = depth
        , asTimeLabel = timeLabel
        , asAudioPlayer    = audioPlayerRef
        , asAudioPlayerCmd = audioPlayerCmdRef
        }

  GLFW.setKeyCallback win $ Just (keyCallback appState)
  GLFW.setFramebufferSizeCallback win $ Just (resizeCallback appState)
  GLFW.setCursorPosCallback win $ Just (cursorPosCallback appState)
  GLFW.setWindowCloseCallback win $ Just closeCallback

  -- Handle skip intro from config
  when (oglSkipIntro cfg) $ do
    writeIORef introStateRef (initIntroState { introPhase = IntroComplete, fadeAlpha = 0.0 })
    writeIORef skipIntroRef True
    logger "Intro skipped (config)"

  -- Start intro audio if not skipping
  unless (oglSkipIntro cfg) $ do
    audioPlayer <- case oglMidiFile cfg of
      -- Use intro.mp3 from data directory via getDataFileName
      Nothing -> do
        introPath <- getDataFileName "assets/intro.mp3"
        exists <- doesFileExist introPath
        if exists 
          then spawnIntroAudio audioPlayerCmdRef introPath
          else do
            putStrLn "[OpenGL] WARNING: intro.mp3 not found. Continuing without intro audio."
            return NoAudio
      Just _ -> return NoAudio  -- Don't play intro if MIDI file specified
    writeIORef audioPlayerRef audioPlayer

  logger "All systems initialized. Entering render loop."
  logger "Press F5 to live-reload shaders | SPACE to skip intro | ESC to quit"

  mainLoop appState

  -- Cleanup
  stopAudioPlayer =<< readIORef audioPlayerRef
  ImGuiGL3.shutdown
  ImGuiGLFW.shutdown
  ImGui.destroyContext =<< ImGui.getCurrentContext
  NVG.deleteGL3 nvgCtx
  glDeleteProgram =<< readIORef bgProgRef
  glDeleteProgram noteProg
  glDeleteProgram modelProg
  GLFW.destroyWindow win
  GLFW.terminate
  logger "Clean shutdown complete."

-- | Run OpenGL visualizer standalone (without backend connection)
runOpenGLStandalone :: OpenGLConfig -> IO ()
runOpenGLStandalone cfg = do
  -- Create a dummy reactor state using the proper constructor
  let dummyConfig = Config.defaultConfig
  reactorVar <- newTVarIO (emptyReactorState dummyConfig)
  runOpenGLVisualizer cfg reactorVar