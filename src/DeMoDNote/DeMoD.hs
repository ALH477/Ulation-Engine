{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

-- DeMoD.hs — Production build
-- "DeMoD LLC presents" typewriter → time-seeded Sierpinski scan → Astart TUI
-- Audio: plays intro.mp3 by default via mpg123 (Linux) or afplay (macOS)
--
-- Usage:
--   cabal run demod              # plays intro.mp3 from data-files
--   cabal run demod -- --no-audio   # skip audio
--   cabal run demod -- --audio custom.mp3  # use custom audio
--
-- Dependencies: brick, vty, time, process
-- Build: cabal build && cabal run demod

module Main where

import Brick
import Brick.BChan                (newBChan, writeBChan)
import Brick.Widgets.Center       (center)
import qualified Graphics.Vty          as V
import Graphics.Vty.CrossPlatform (mkVty)
import Control.Concurrent         (forkIO, threadDelay)
import Control.Monad              (forever, void)
import Control.Exception          (SomeException, try)
import Data.Time                  (getCurrentTime)
import System.Process             (createProcess, proc, std_out, std_err, StdStream(..),
                                   ProcessHandle, terminateProcess, waitForProcess)
import System.Environment         (getArgs)
import System.IO                  (hPutStrLn, stderr)
import Paths_DeMoD_Note           (getDataFileName)
import DeMoDNote.Intro

-- ────────────────────────────────────────────────────────────────
-- CLI args
-- ────────────────────────────────────────────────────────────────

data AudioConfig = AudioDefault | AudioFile FilePath | AudioOff deriving Show

-- Renamed from 'Config' to avoid shadowing DeMoDNote.Config.Config
data CLIConfig = CLIConfig { cfgAudio :: AudioConfig } deriving Show

parseArgs :: [String] -> CLIConfig
parseArgs args = CLIConfig { cfgAudio = go args }
  where
    go ("--no-audio" : _)      = AudioOff
    go ("--audio" : path : _)  = AudioFile path
    go (_ : rest)              = go rest
    go []                      = AudioDefault  -- play intro.mp3 by default

-- ────────────────────────────────────────────────────────────────
-- Events & State
-- ────────────────────────────────────────────────────────────────

data Tick = Tick

data Phase
  = PhasePresents  -- typewrite "DeMoD LLC presents"
  | PhaseReveal    -- scan-line reveal of Sierpinski
  | PhaseAstart    -- final stable display with "Astart" banner
  deriving (Eq, Show)

data St = St
  { phase      :: Phase
  , tick       :: Int
  , charCount  :: Int     -- chars typed so far in Presents phase
  , scanRow    :: Int     -- rows revealed in Reveal phase
  , triangle   :: [String]  -- pre-rendered Sierpinski rows
  , triDepth   :: Int     -- depth derived from time
  , timeLabel  :: String  -- HH:MM:SS used as seed label
  }

-- ────────────────────────────────────────────────────────────────
-- Timing
-- ────────────────────────────────────────────────────────────────

-- 30 fps tick loop
fpsDelay :: Int
fpsDelay = 33_000

-- ────────────────────────────────────────────────────────────────
-- Event handling
-- ────────────────────────────────────────────────────────────────

appEvent :: BrickEvent () Tick -> EventM () St ()
appEvent (AppEvent Tick) = do
  st <- get
  let t' = tick st + 1
  case phase st of

    PhasePresents -> do
      let c' = min presentsLength (charCount st + 1)
          nextPhase = if c' >= presentsLength then PhaseReveal else PhasePresents
      put st { tick = t', charCount = c', phase = nextPhase }

    PhaseReveal -> do
      let rows = length (triangle st)
          r'   = min rows (scanRow st + 1)
          nextPhase = if r' >= rows then PhaseAstart else PhaseReveal
      put st { tick = t', scanRow = r', phase = nextPhase }

    PhaseAstart -> put st { tick = t' }

appEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
appEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
appEvent _ = pure ()

-- ────────────────────────────────────────────────────────────────
-- Rendering
-- ────────────────────────────────────────────────────────────────

drawUi :: St -> [Widget ()]
drawUi st = [ui]
  where
    ui = case phase st of
      PhasePresents -> center $ vBox
        [ drawPresents (charCount st)
        , str ""
        , withAttr (aName "dim") $ str "  seeding from system time..."
        ]

      PhaseReveal -> center $ vBox
        [ drawTriangle st (scanRow st)
        , str ""
        , footer st
        ]

      PhaseAstart -> center $ vBox
        [ drawAstart
        , str ""
        , drawTriangle st (length $ triangle st)
        , str ""
        , footer st
        , str ""
        , withAttr (aName "hint") $ str "  [q / ESC]  quit"
        ]

drawPresents :: Int -> Widget ()
drawPresents n =
  vBox
    [ withAttr (aName "presents") $ str (padRight presentsWidth r)
    | r <- lines (take n presentsText)
    ]
  where
    padRight w s = s ++ replicate (max 0 (w - length s)) ' '

drawTriangle :: St -> Int -> Widget ()
drawTriangle st revealed =
  vBox
    [ withAttr (rowAttr i (length tri)) (str row)
    | (i, row) <- zip [0..] (take revealed tri)
    ]
  where
    tri = triangle st

rowAttr :: Int -> Int -> AttrName
rowAttr i total
  | pct < 0.33 = aName "tri1"
  | pct < 0.66 = aName "tri2"
  | otherwise  = aName "tri3"
  where pct = fromIntegral i / fromIntegral (max 1 total) :: Double

drawAstart :: Widget ()
drawAstart = vBox [ withAttr (aName "astart") (str l) | l <- astartLines ]

footer :: St -> Widget ()
footer st = withAttr (aName "dim") $
  str ("  seed: " ++ timeLabel st ++ "  │  depth: " ++ show (triDepth st))

aName :: String -> AttrName
aName = attrName

-- ────────────────────────────────────────────────────────────────
-- Attributes
-- ────────────────────────────────────────────────────────────────

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (aName "presents", bold V.cyan)
  , (aName "tri1",     bold V.cyan)
  , (aName "tri2",     bold V.blue)
  , (aName "tri3",     V.defAttr `V.withForeColor` V.magenta)
  , (aName "astart",   bold V.yellow)
  , (aName "dim",      V.defAttr `V.withForeColor` V.brightBlack)
  , (aName "hint",     V.defAttr `V.withForeColor` V.white)
  ]
  where bold c = V.defAttr `V.withForeColor` c `V.withStyle` V.bold

-- ────────────────────────────────────────────────────────────────
-- App record
-- ────────────────────────────────────────────────────────────────

app :: App St Tick ()
app = App
  { appDraw         = drawUi
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = appEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const theMap
  }

-- ────────────────────────────────────────────────────────────────
-- Audio playback
-- Cross-platform: tries mpg123 (Linux) then afplay (macOS) then mplayer
-- Runs in a background thread; ProcessHandle stored for clean shutdown
-- ────────────────────────────────────────────────────────────────

data AudioPlayer = NoAudio | Player ProcessHandle

-- Try each player binary in order, return first that spawns successfully
spawnAudio :: FilePath -> IO AudioPlayer
spawnAudio path = tryPlayers candidates
  where
    candidates =
      [ ("mpg123",  ["-q",              path])  -- Linux standard
      , ("afplay",  [                   path])  -- macOS built-in
      , ("mplayer", ["-really-quiet",   path])  -- fallback
      ]
    tryPlayers [] = do
      hPutStrLn stderr
        "[demod] WARNING: no audio player found (mpg123/afplay/mplayer). Continuing without audio."
      return NoAudio
    tryPlayers ((cmd, args) : rest) = do
      result <- try $ createProcess
        (proc cmd args) { std_out = CreatePipe, std_err = CreatePipe }
      case result of
        Left  (_ :: SomeException) -> tryPlayers rest
        Right (_, _, _, ph)        -> return (Player ph)

stopAudio :: AudioPlayer -> IO ()
stopAudio NoAudio     = return ()
stopAudio (Player ph) = void . try @SomeException $
  terminateProcess ph >> waitForProcess ph

-- ────────────────────────────────────────────────────────────────
-- Main
-- ────────────────────────────────────────────────────────────────

main :: IO ()
main = do
  args <- getArgs
  let cfg = parseArgs args

  -- Time seed
  now <- getCurrentTime
  let (depth, fillChar, label) = timeToDepthAndChar now
      tri    = renderSierpinski depth fillChar
      initSt = St
        { phase     = PhasePresents
        , tick      = 0
        , charCount = 0
        , scanRow   = 0
        , triangle  = tri
        , triDepth  = depth
        , timeLabel = label
        }

  -- Tick channel
  chan <- newBChan 10
  void $ forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay fpsDelay

  -- Audio — background, non-blocking
  -- Default: play intro.mp3 from data-files
  -- --no-audio: skip audio
  -- --audio path: use custom file
  audio <- case cfgAudio cfg of
    AudioOff -> return NoAudio
    AudioFile path -> spawnAudio path
    AudioDefault -> do
      introPath <- getDataFileName "assets/intro.mp3"
      spawnAudio introPath

  -- Brick TUI
  let buildVty = mkVty V.defaultConfig
  vty     <- buildVty
  finalSt <- customMain vty buildVty (Just chan) app initSt

  -- Cleanup audio process
  stopAudio audio

  -- Handoff hook — replace putStrLn with your real TUI entry point
  putStrLn $ "DeMoD LLC — Astart ready. (seed=" ++ timeLabel finalSt ++ ")"
