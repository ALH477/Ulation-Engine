{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
module DeMoDNote.TUI where

import Brick hiding (clamp)
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Graphics.Vty
import Graphics.Vty.CrossPlatform (mkVty)
import qualified Graphics.Vty.Config as VtyConfig
import Control.Concurrent (Chan, forkIO, readChan, threadDelay)
import Control.Concurrent.STM (atomically, readTVar, TVar)
import Control.Exception (SomeException, catch)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (modify')
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Process (spawnCommand)
import Data.List (intercalate, intersperse)
import Data.Time.Clock
import Data.Time.Clock.POSIX (getPOSIXTime)
import DeMoDNote.Types
import DeMoDNote.Config hiding (defaultConfig)
import DeMoDNote.Backend (DetectionEvent(..))
import DeMoDNote.Util (safeIndex, safeTail, clamp, chunksOf)

-- ─────────────────────────────────────────────────────────────────────────────
-- Configuration constants
-- ─────────────────────────────────────────────────────────────────────────────

-- | Tick thread interval.  Drives animations at ~3.6 Hz.
tickIntervalUs :: Int
tickIntervalUs = 280_000

-- | BChan capacity.  Large enough to absorb a burst; small enough that
--   back-pressure is felt before memory balloons.
bChanCapacity :: Int
bChanCapacity = 100

-- | Number of ticks before a transient status message reverts to idle.
--   At 3.6 Hz, 12 ticks ≈ 3.3 seconds.
statusExpiryTicks :: Int
statusExpiryTicks = 12

-- | Maximum tap-tempo samples retained.
maxTapSamples :: Int
maxTapSamples = 8

-- | Waveform sample buffer length fed to the oscilloscope.
waveformSamples :: Int
waveformSamples = 64

-- | Maximum note-history entries shown in the history row.
maxNoteHistory :: Int
maxNoteHistory = 10

-- ─────────────────────────────────────────────────────────────────────────────
-- Utilities  (safeIndex, safeTail, clamp, chunksOf imported from DeMoDNote.Util)
-- ─────────────────────────────────────────────────────────────────────────────

-- "BPM" → "B P M"  (retro digital-display aesthetic)
spaced :: String -> String
spaced = intersperse ' '

-- Fixed one-decimal formatter: correct for negatives and rounding carry.
showFixed1 :: Double -> String
showFixed1 x
    | x < 0    = "-" ++ showFixed1 (abs x)
    | otherwise =
        let i        = floor x :: Int
            f        = round ((x - fromIntegral i) * 10.0) :: Int
            (i', f') = if f >= 10 then (i + 1, 0) else (i, f)
        in  show i' ++ "." ++ show f'

-- ─────────────────────────────────────────────────────────────────────────────
-- Events
-- ─────────────────────────────────────────────────────────────────────────────

data TUIEvent
    = TUIStatusMsg  String
    | TUIDetection  DetectionEvent
    | TUITick           -- fired ~4 Hz by the animation thread

-- ─────────────────────────────────────────────────────────────────────────────
-- Color palette system
-- ─────────────────────────────────────────────────────────────────────────────

data ColorPalette
    = Synthwave   -- ① neon cyan / magenta / green (default)
    | Sunset      -- ② warm red / orange / yellow
    | Matrix      -- ③ monochrome green phosphor
    | Ice         -- ④ cool blue / cyan / white
    | Amber       -- ⑤ classic amber terminal
    deriving (Eq, Enum, Bounded)

allPalettes :: [ColorPalette]
allPalettes = [minBound .. maxBound]

-- Single source of truth: (constructor, display name, keyboard shortcut).
-- Adding a palette here is the only edit required — name/key/parse all derive.
paletteRegistry :: [(ColorPalette, String, Char)]
paletteRegistry =
    [ (Synthwave, "Synthwave", '1')
    , (Sunset,    "Sunset",    '2')
    , (Matrix,    "Matrix",    '3')
    , (Ice,       "Ice",       '4')
    , (Amber,     "Amber",     '5')
    ]

paletteName :: ColorPalette -> String
paletteName p =
    case [ n | (p', n, _) <- paletteRegistry, p' == p ] of
        (n:_) -> n
        []    -> error $ "paletteName: palette not in registry (add it to paletteRegistry)"

paletteKey :: ColorPalette -> Char
paletteKey p =
    case [ k | (p', _, k) <- paletteRegistry, p' == p ] of
        (k:_) -> k
        []    -> error $ "paletteKey: palette not in registry (add it to paletteRegistry)"

paletteFromChar :: Char -> Maybe ColorPalette
paletteFromChar c = case [ p | (p, _, k) <- paletteRegistry, k == c ] of
    (p:_) -> Just p
    []    -> Nothing

-- Dynamic attr map: called on every render with the current state.
paletteMap :: ColorPalette -> AttrMap
paletteMap p = attrMap defAttr $
    [ (helpAttr,             fg (ISOColor 8))
    , (progressIncompleteAttr, bg (ISOColor 8))
    ] ++ paletteAttrs p

paletteAttrs :: ColorPalette -> [(AttrName, Attr)]
paletteAttrs Synthwave =
    [ (titleAttr,       withStyle (fg brightCyan)    bold)
    , (valueAttr,       withStyle (fg brightCyan)    bold)
    , (dimAttr,         fg (ISOColor 8))
    , (bpmAttr,         withStyle (fg brightCyan)    bold)
    , (velAttr,         fg cyan)
    , (waveAttr,        fg brightGreen)
    , (highConfAttr,    fg brightGreen)
    , (midConfAttr,     fg yellow)
    , (lowConfAttr,     fg red)
    , (heroNoteAttr,    withStyle (fg brightMagenta) bold)
    , (heroOctAttr,     fg (ISOColor 8))
    , (tuningGoodAttr,  withStyle (fg brightGreen)   bold)
    , (tuningBadAttr,   withStyle (fg red)           bold)
    , (jackGoodAttr,    fg brightGreen)
    , (jackBadAttr,     fg red)
    , (jackWarnAttr,    fg yellow)
    , (accentAttr,      fg cyan)
    , (statusAttr,      fg (ISOColor 7))
    , (splashTitleAttr, withStyle (fg brightMagenta) bold)
    , (splashSubAttr,   fg cyan)
    , (splashDivAttr,   fg (ISOColor 8))
    , (splashBodyAttr,  fg (ISOColor 7))
    , (progressCompleteAttr, bg brightGreen)
    ]
paletteAttrs Sunset =
    [ (titleAttr,       withStyle (fg red)           bold)
    , (valueAttr,       withStyle (fg brightYellow)  bold)
    , (dimAttr,         fg (ISOColor 8))
    , (bpmAttr,         withStyle (fg brightYellow)  bold)
    , (velAttr,         fg yellow)
    , (waveAttr,        fg yellow)
    , (highConfAttr,    fg yellow)
    , (midConfAttr,     fg red)
    , (lowConfAttr,     fg magenta)
    , (heroNoteAttr,    withStyle (fg brightYellow)  bold)
    , (heroOctAttr,     fg (ISOColor 8))
    , (tuningGoodAttr,  withStyle (fg yellow)        bold)
    , (tuningBadAttr,   withStyle (fg magenta)       bold)
    , (jackGoodAttr,    fg yellow)
    , (jackBadAttr,     fg magenta)
    , (jackWarnAttr,    fg red)
    , (accentAttr,      fg red)
    , (statusAttr,      fg (ISOColor 7))
    , (splashTitleAttr, withStyle (fg brightYellow)  bold)
    , (splashSubAttr,   fg red)
    , (splashDivAttr,   fg (ISOColor 8))
    , (splashBodyAttr,  fg (ISOColor 7))
    , (progressCompleteAttr, bg yellow)
    ]
paletteAttrs Matrix =
    [ (titleAttr,       withStyle (fg brightGreen)   bold)
    , (valueAttr,       withStyle (fg brightGreen)   bold)
    , (dimAttr,         fg green)
    , (bpmAttr,         withStyle (fg brightGreen)   bold)
    , (velAttr,         fg green)
    , (waveAttr,        fg brightGreen)
    , (highConfAttr,    fg brightGreen)
    , (midConfAttr,     fg green)
    , (lowConfAttr,     fg (ISOColor 2))
    , (heroNoteAttr,    withStyle (fg brightGreen)   bold)
    , (heroOctAttr,     fg green)
    , (tuningGoodAttr,  withStyle (fg brightGreen)   bold)
    , (tuningBadAttr,   withStyle (fg yellow)        bold)
    , (jackGoodAttr,    fg brightGreen)
    , (jackBadAttr,     withStyle (fg yellow)       bold)
    , (jackWarnAttr,    fg green)
    , (accentAttr,      fg green)
    , (statusAttr,      fg green)
    , (splashTitleAttr, withStyle (fg brightGreen)   bold)
    , (splashSubAttr,   fg green)
    , (splashDivAttr,   fg (ISOColor 2))
    , (splashBodyAttr,  fg green)
    , (progressCompleteAttr, bg brightGreen)
    ]
paletteAttrs Ice =
    [ (titleAttr,       withStyle (fg brightCyan)    bold)
    , (valueAttr,       withStyle (fg brightCyan)    bold)
    , (dimAttr,         fg (ISOColor 8))
    , (bpmAttr,         withStyle (fg brightCyan)    bold)
    , (velAttr,         fg cyan)
    , (waveAttr,        fg cyan)
    , (highConfAttr,    fg brightCyan)
    , (midConfAttr,     fg blue)
    , (lowConfAttr,     fg (ISOColor 4))
    , (heroNoteAttr,    withStyle (fg brightWhite)   bold)
    , (heroOctAttr,     fg cyan)
    , (tuningGoodAttr,  withStyle (fg brightCyan)    bold)
    , (tuningBadAttr,   withStyle (fg blue)          bold)
    , (jackGoodAttr,    fg brightCyan)
    , (jackBadAttr,     fg blue)
    , (jackWarnAttr,    fg cyan)
    , (accentAttr,      fg brightBlue)
    , (statusAttr,      fg (ISOColor 7))
    , (splashTitleAttr, withStyle (fg brightWhite)   bold)
    , (splashSubAttr,   fg brightCyan)
    , (splashDivAttr,   fg (ISOColor 8))
    , (splashBodyAttr,  fg (ISOColor 7))
    , (progressCompleteAttr, bg brightCyan)
    ]
paletteAttrs Amber =
    [ (titleAttr,       withStyle (fg yellow)        bold)
    , (valueAttr,       withStyle (fg brightYellow)  bold)
    , (dimAttr,         fg (ISOColor 8))
    , (bpmAttr,         withStyle (fg brightYellow)  bold)
    , (velAttr,         fg yellow)
    , (waveAttr,        fg yellow)
    , (highConfAttr,    fg brightYellow)
    , (midConfAttr,     fg yellow)
    , (lowConfAttr,     fg (ISOColor 3))
    , (heroNoteAttr,    withStyle (fg brightYellow)  bold)
    , (heroOctAttr,     fg yellow)
    , (tuningGoodAttr,  withStyle (fg brightYellow)  bold)
    , (tuningBadAttr,   withStyle (fg (ISOColor 3))  bold)
    , (jackGoodAttr,    fg brightYellow)
    , (jackBadAttr,     fg (ISOColor 3))
    , (jackWarnAttr,    fg yellow)
    , (accentAttr,      fg yellow)
    , (statusAttr,      fg (ISOColor 7))
    , (splashTitleAttr, withStyle (fg brightYellow)  bold)
    , (splashSubAttr,   fg yellow)
    , (splashDivAttr,   fg (ISOColor 8))
    , (splashBodyAttr,  fg (ISOColor 7))
    , (progressCompleteAttr, bg brightYellow)
    ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Tips & JACK guide (hardcoded; one tip selected pseudo-randomly at startup)
-- ─────────────────────────────────────────────────────────────────────────────

tips :: [String]
tips =
    [ "Sustain each note for 2+ beats to maximise detection confidence."
    , "Set input gain so peaks sit around -12 dBFS: not too loud, not too quiet."
    , "Open the chromatic tuner with [t] before a session to check intonation."
    , "Four or more tap-tempo taps gives the most accurate BPM estimate."
    , "High confidence (● strong) means the pitch algorithm has converged."
    , "JACK latency under 5 ms is excellent for real-time harmonic analysis."
    , "Pentatonic scales minimise 'wrong note' detections during improvisation."
    , "The oscilloscope shows raw input — watch for clipping at the top rail."
    , "Arpeggio patterns are quantised to the active scale for musical output."
    , "Try the Blues scale in C for an authentic twelve-bar feel."
    , "Connect your instrument directly before JACK routing for lowest latency."
    , "Hold a drone and cycle scales with [s] to hear how modes colour pitch."
    , "The waveform spark-line above the dot-plot shows the amplitude envelope."
    , "Press [r] to clear tap tempo and re-lock BPM to a new section."
    ]

jackGuide :: [String]
jackGuide =
    [ "① Start jackd or QjackCtl before launching DeMoDNote."
    , "② Select your audio interface as the JACK driver."
    , "③ Set buffer size to 64–256 samples for low latency."
    , "④ Connect your instrument input → DeMoDNote capture port."
    , "⑤ Press ENTER when JACK is running and routing is confirmed."
    ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Domain type aliases (local NoteEvent for TUI)
-- ─────────────────────────────────────────────────────────────────────────────

type NoteEvent = (MIDINote, Velocity)

-- ─────────────────────────────────────────────────────────────────────────────
-- TUI mode
-- ─────────────────────────────────────────────────────────────────────────────

data TUIMode
    = StartMode      -- animated splash / options screen
    | MainMode       -- main monitoring interface
    | SoundFontMode  -- soundfont browser with donation prompt
    deriving (Eq)

-- ─────────────────────────────────────────────────────────────────────────────
-- State
-- ─────────────────────────────────────────────────────────────────────────────

data TUIState = TUIState
    { tuiConfig        :: Config
    -- ── mode & animation ───────────────────────────────────────────────────
    , tuiMode          :: TUIMode
    , tuiTick          :: Int          -- incremented ~4 Hz for animations
    , tuiPalette       :: ColorPalette
    , tuiTip           :: String       -- selected at startup
    -- ── audio / detection ─────────────────────────────────────────────────
    , tuiBPM           :: Double
    , tuiTapTimes      :: [UTCTime]
    , tuiLastNote      :: Maybe NoteEvent   -- (midiNote, velocity 0–127)
    , tuiNoteHistory   :: [NoteEvent]
    , tuiConfidence    :: Double            -- 0.0 – 1.0
    , tuiLatency       :: Double            -- milliseconds
    , tuiWaveform      :: [Double]          -- 64 samples, -1.0 – 1.0
    -- ── presets ───────────────────────────────────────────────────────────
    , tuiScaleIndex    :: Int
    , tuiArpeggioIndex :: Int
    -- ── misc ──────────────────────────────────────────────────────────────
    , tuiRunning        :: Bool
    , tuiStatusMessage  :: String
    , tuiStatusExpiry   :: Maybe Int        -- tick at which status reverts to idle
    , tuiTuningMode     :: Bool
    , tuiTuningNote     :: Maybe Int
    , tuiTuningCents    :: Double           -- -50.0 – 50.0
    , tuiTuningInTune   :: Bool
    , tuiJackStatus     :: JackStatus
    -- ── feedback ──────────────────────────────────────────────────────────
    , tuiNoteFlash      :: Int              -- ticks remaining for hero border flash
    , tuiPeakVelocity   :: Int             -- peak velocity hold for VU meter
    }

-- NOTE: All scales are currently rooted in C.  When root-key selection is added,
-- these should be generated dynamically (e.g. map (++ " " ++ rootName) modeNames).
availableScales :: [String]
availableScales =
    [ "C Major", "C Minor", "C Dorian", "C Phrygian", "C Lydian"
    , "C Mixolydian", "C Locrian", "C Pentatonic Maj", "C Pentatonic Min"
    , "C Blues", "C Chromatic"
    ]

availableArpeggios :: [String]
availableArpeggios =
    [ "None", "Triad Up", "Triad Down", "Triad Up/Down"
    , "7th Up", "7th Down", "7th Up/Down", "Octave Up"
    ]

currentScale :: TUIState -> String
currentScale s = safeIndex availableScales (tuiScaleIndex s) "Unknown"

currentArpeggio :: TUIState -> String
currentArpeggio s = safeIndex availableArpeggios (tuiArpeggioIndex s) "Unknown"

initialTUIState :: Config -> String -> TUIState
initialTUIState cfg tip = TUIState
    { tuiConfig        = cfg
    , tuiMode          = StartMode
    , tuiTick          = 0
    , tuiPalette       = Synthwave
    , tuiTip           = tip
    , tuiBPM           = 120.0
    , tuiTapTimes      = []
    , tuiLastNote      = Nothing
    , tuiNoteHistory   = []
    , tuiConfidence    = 0.0
    , tuiLatency       = 0.0
    , tuiWaveform      = replicate waveformSamples 0.0
    , tuiScaleIndex    = 0
    , tuiArpeggioIndex = 0
    , tuiRunning       = True
    , tuiStatusMessage = "READY  ─  PRESS ENTER TO START"
    , tuiStatusExpiry  = Nothing
    , tuiTuningMode    = False
    , tuiTuningNote    = Nothing
    , tuiTuningCents   = 0.0
    , tuiTuningInTune  = False
    , tuiJackStatus    = JackDisconnected
    , tuiNoteFlash     = 0
    , tuiPeakVelocity  = 0
    }

-- ─────────────────────────────────────────────────────────────────────────────
-- MIDI helpers
-- ─────────────────────────────────────────────────────────────────────────────

noteNames :: [String]
noteNames = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"]

midiToName :: Int -> String
midiToName n = safeIndex noteNames (n `mod` 12) "C" ++ show ((n `div` 12) - 1)

midiNotePart :: Int -> String
midiNotePart n = safeIndex noteNames (n `mod` 12) "C"

midiOctavePart :: Int -> String
midiOctavePart n = show ((n `div` 12) - 1)

-- VU-meter with peak-hold marker │
vuBarWithPeak :: Int -> Int -> Int -> String
vuBarWithPeak w v peak =
    let toPos x = clamp 0 (w - 1)
            (round (fromIntegral x / 127.0 * fromIntegral (w - 1) :: Double) :: Int)
        vPos    = toPos v
        pPos    = toPos peak
        cell i
            | i <= vPos = '█'
            | i == pPos = '│'
            | otherwise = '░'
    in  "▐" ++ map cell [0 .. w - 1] ++ "▌"

-- ─────────────────────────────────────────────────────────────────────────────
-- Waveform renderers
-- ─────────────────────────────────────────────────────────────────────────────

-- Amplitude envelope spark-line: uses |x| so positive and negative excursions
-- both read as signal energy.  Sign is shown in the dot-plot below.
sparkLine :: [Double] -> String
sparkLine samples =
    let chars  = " ▁▂▃▄▅▆▇█"
        toIdx x = clamp 0 8 (round (abs x * 8.0) :: Int)
    in  map (\x -> safeIndex chars (toIdx x) ' ') samples

-- True when any sample touches the rail — signals overload to the user.
waveformIsClipping :: [Double] -> Bool
waveformIsClipping = any (\x -> abs x >= 0.95)

-- Multi-row dot-plot oscilloscope — CRT phosphor aesthetic.
waveformRows :: [Double] -> Int -> [String]
waveformRows samples height =
    let w      = min 64 (length samples)
        samps  = take w samples
        toRow x = clamp 0 (height - 1)
                     (round ((x + 1.0) / 2.0 * fromIntegral (height - 1)) :: Int)
        rowOf  = map toRow samps
        mkLine r = map (\rv -> if rv == r then '●' else '·') rowOf
    in  [ mkLine r | r <- [height - 1, height - 2 .. 0] ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Signal quality
-- ─────────────────────────────────────────────────────────────────────────────

confidenceBar :: Double -> String
confidenceBar c =
    let w      = 20
        filled = clamp 0 w (round (clamp 0.0 1.0 c * fromIntegral w) :: Int)
    in  replicate filled '█' ++ replicate (w - filled) '░'

confidenceAttr :: Double -> AttrName
confidenceAttr c
    | c >= 0.75 = highConfAttr
    | c >= 0.40 = midConfAttr
    | otherwise = lowConfAttr

latencyInfo :: Double -> (String, String, AttrName)
latencyInfo lat
    | lat == 0.0 = ("○", "no data",   dimAttr)
    | lat <  5.0 = ("●", "excellent", highConfAttr)
    | lat < 15.0 = ("●", "good",      midConfAttr)
    | lat < 30.0 = ("◐", "moderate",  midConfAttr)
    | otherwise  = ("○", "high",      lowConfAttr)

-- ─────────────────────────────────────────────────────────────────────────────
-- Tuning
-- ─────────────────────────────────────────────────────────────────────────────

tuningInTuneThreshold :: Double
tuningInTuneThreshold = 5.0

-- 41-char bar; 1 char ≈ 2.5 ¢; range ±50 ¢.
-- ◀ fills leftward (flat); ▶ fills rightward (sharp).
centsBar :: Double -> String
centsBar cents =
    let total  = 41
        centre = total `div` 2
        offset = clamp (-centre) centre (round (cents / 2.5) :: Int)
        pos    = centre + offset
        lChar  = if offset < 0 then '◀' else '░'
        rChar  = if offset > 0 then '▶' else '░'
    in  replicate pos lChar ++ "┃" ++ replicate (total - 1 - pos) rChar

-- ─────────────────────────────────────────────────────────────────────────────
-- Border styles
-- ─────────────────────────────────────────────────────────────────────────────

-- Heavy double-line outer frame ╔═╗ ║ ╠═╣ ╚═╝ — the retro chrome.
retroBorder :: BorderStyle
retroBorder = BorderStyle
    { bsCornerTL      = '╔'
    , bsCornerTR      = '╗'
    , bsCornerBL      = '╚'
    , bsCornerBR      = '╝'
    , bsIntersectFull = '╬'
    , bsIntersectL    = '╠'
    , bsIntersectR    = '╣'
    , bsIntersectT    = '╦'
    , bsIntersectB    = '╩'
    , bsHorizontal    = '═'
    , bsVertical      = '║'
    }

-- Light rounded inner panels ╭─╮ │ ╰─╯ — contrast with outer double-line.
innerBorder :: BorderStyle
innerBorder = unicodeRounded

-- ─────────────────────────────────────────────────────────────────────────────
-- App  (attr map is dynamic: palette changes without restart)
-- ─────────────────────────────────────────────────────────────────────────────

tuiApp :: App TUIState TUIEvent ()
tuiApp = App
    { appDraw         = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent  = handleEvent
    , appStartEvent   = return ()
    , appAttrMap      = \s -> paletteMap (tuiPalette s)
    }

-- ─────────────────────────────────────────────────────────────────────────────
-- Top-level draw dispatcher
-- ─────────────────────────────────────────────────────────────────────────────

drawUI :: TUIState -> [Widget ()]
drawUI state = case tuiMode state of
    StartMode     -> [drawStartMenu state]
    MainMode      -> [drawMainUI   state]
    SoundFontMode -> [drawSoundFontUI state]

-- ─────────────────────────────────────────────────────────────────────────────
-- START MENU
-- ─────────────────────────────────────────────────────────────────────────────
--
--  ╔══════════ ◈  D·E·M·O·D·N·O·T·E  ◈ ══════════╗
--  ║                                               ║
--  ║   ╭──────────────────────────────────────╮   ║
--  ║   │  ★  D · E · M · O · D · N · O · T · E │  ║
--  ║   │     Detection · Modulation · Notation  │  ║
--  ║   ╰──────────────────────────────────────╯   ║
--  ║                                               ║
--  ║  ── T I P ──────────────────────────────      ║
--  ║  Sustain each note for 2+ beats…             ║
--  ║                                               ║
--  ║  ── J A C K  S E T U P ────────────────       ║
--  ║  ① Start jackd or QjackCtl first…            ║
--  ║  ② Select audio interface as JACK driver…    ║
--  ║  ③ Buffer size 64–256 samples…               ║
--  ║  ④ Connect instrument → capture port…        ║
--  ║  ⑤ Press ENTER when routing is confirmed…    ║
--  ║                                               ║
--  ║  ── C O L O R  T H E M E ──────────────       ║
--  ║  ► [1] Synthwave  [2] Sunset  [3] Matrix      ║
--  ║    [4] Ice        [5] Amber                   ║
--  ║                                               ║
--  ║         ▶  P R E S S  E N T E R  ◀            ║
--  ║                                               ║
--  ╚═══════════════════════════════════════════════╝

drawStartMenu :: TUIState -> Widget ()
drawStartMenu state =
    withBorderStyle retroBorder $
    borderWithLabel menuTitleW $
    padAll 2 $
    vBox
        [ logoBlock state
        , str " "
        , sectionHeader "T I P"
        , padLeft (Pad 2) $ withAttr splashBodyAttr (str (tuiTip state))
        , str " "
        , sectionHeader "J A C K  S E T U P"
        , vBox [ padLeft (Pad 2) $ withAttr splashBodyAttr (str l) | l <- jackGuide ]
        , str " "
        , sectionHeader "C O L O R  T H E M E"
        , str " "
        , paletteSelector state
        , str " "
        , hCenter (jackStatusWidget state)
        , str " "
        , hCenter (pressEnterWidget state)
        , str " "
        ]
  where
    menuTitleW = withAttr titleAttr $
                 str (" ◈  " ++ intersperse '·' "DeMoD-NOTE" ++ "  ◈ ")

-- ── Animated logo block ───────────────────────────────────────────────────────

logoBlock :: TUIState -> Widget ()
logoBlock state =
    withBorderStyle innerBorder $
    borderWithLabel logoLabel $
    padAll 1 $
    vBox
        [ hCenter $ withAttr splashTitleAttr $
            str (orn ++ "  " ++ intersperse ' ' "DeMoD-NOTE" ++ "  " ++ orn)
        , hCenter $ withAttr splashSubAttr   $
            str "Detection  ·  Modulation  ·  Notation  Engine"
        ]
  where
    -- Ornament cycles through 4 glyphs, giving a slow shimmer.
    ornaments = ["◈", "◇", "◆", "◇"] :: [String]
    orn       = safeIndex ornaments (tuiTick state `mod` 4) "◈"
    logoLabel = withAttr dimAttr $ str " v1.0 "

-- ── Section divider header ────────────────────────────────────────────────────

sectionHeader :: String -> Widget ()
sectionHeader label =
    padBottom (Pad 1) $
    withAttr splashDivAttr $
    hBox
        [ str ("  ─── " ++ label ++ " ")
        , fill '─'
        ]

-- ── Palette selector ──────────────────────────────────────────────────────────

paletteSelector :: TUIState -> Widget ()
paletteSelector state =
    let chunkSize = 4
        rows = chunksOf chunkSize allPalettes
        renderRow row = padLeft (Pad 2) $ hBox $ intersperse (str "  ") $
            map (paletteOption state) row
    in vBox $ map renderRow rows

paletteOption :: TUIState -> ColorPalette -> Widget ()
paletteOption state p =
    let selected = tuiPalette state == p
        pfx      = if selected then "► " else "  "
        atr      = if selected then accentAttr else dimAttr
    in  withAttr atr $
        str (pfx ++ "[" ++ [paletteKey p] ++ "] " ++ paletteName p)

-- ── JACK status indicator on startup screen ──────────────────────────────────

jackStatusWidget :: TUIState -> Widget ()
jackStatusWidget state =
    let (statusText, jackAtr) = case tuiJackStatus state of
            JackConnected    -> ("● JACK CONNECTED  ─  PRESS ENTER", jackGoodAttr)
            JackDisconnected -> ("○ WAITING FOR JACK...", jackBadAttr)
            JackReconnecting -> ("◐ RECONNECTING...", jackWarnAttr)
            JackError msg    -> ("○ JACK ERROR: " ++ take 20 msg, jackBadAttr)
        blinkOn = tuiTick state `mod` 2 == 0
        atr     = if tuiJackStatus state == JackConnected
                  then if blinkOn then accentAttr else jackAtr
                  else jackAtr
    in  withAttr atr $ str statusText

pressEnterWidget :: TUIState -> Widget ()
pressEnterWidget state =
    let blinkOn = tuiTick state `mod` 2 == 0
        (atr, msg) = case tuiJackStatus state of
            JackConnected    ->
                ( if blinkOn then accentAttr else jackGoodAttr
                , "▶   P R E S S   E N T E R   ◀" )
            JackReconnecting ->
                ( if blinkOn then jackWarnAttr else dimAttr
                , "◐   R E C O N N E C T I N G …" )
            JackError errMsg   ->
                ( if blinkOn then jackBadAttr else dimAttr
                , "○   J A C K   E R R O R   ─   " ++ take 20 errMsg )
            JackDisconnected ->
                ( dimAttr
                , "      W A I T I N G   F O R   J A C K      " )
    in  withAttr atr $ str msg

-- ─────────────────────────────────────────────────────────────────────────────
-- MAIN UI
-- ─────────────────────────────────────────────────────────────────────────────
--
--  ╔═════════ ◈  D·E·M·O·D·N·O·T·E  ◈ ═════════╗
--  ║  J A C K  ║  T A P  B P M  ║  S I G N A L ║
--  ╠═══════════╩═════════════════╩══════════════╣
--  ║  ╭──────────── L A S T  N O T E ─────────╮ ║
--  ║  │  G#  4  ·  midi 68  ·  vel ▐████░▌  84│ ║
--  ║  ╰──────────────────────────────────────╯ ║
--  ╠══════════════════════════════════════════╣
--  ║  ╭─ W A V E F O R M  ▶ RUNNING ─────────╮ ║
--  ║  │  ▁▂▃▄▅▆▇▇▆▅▄▃▂▁▁▂▃▄                 │ ║
--  ║  │  ·●···●·●··············●···●··●·······│ ║
--  ║  ╰──────────────────────────────────────╯ ║
--  ╠══════════════════════════════════════════╣
--  ║  S C A L E   C Major  [1/11]  [s]›  [S]‹  ║
--  ║  A R P E G   None     [1/ 8]  [a]›  [A]‹  ║
--  ╠══════════════════════════════════════════╣
--  ║  H I S T   G#4·84  A4·72  F3·50  C5·99   ║
--  ╠══════════════════════════════════════════╣
--  ║  ◈  READY — JACK CONNECTED                ║
--  ║  [SPC]tap · [s/S]scale · [c]theme · [q]  ║
--  ╚══════════════════════════════════════════╝

drawMainUI :: TUIState -> Widget ()
drawMainUI state =
    withBorderStyle retroBorder $
    borderWithLabel (withAttr titleAttr $ str mainTitle) $
    vBox $
        [ topRow     state
        , hBorder
        , heroPanel  state
        , hBorder
        , if tuiTuningMode state
            then tuningPanel   state
            else waveformPanel state
        , hBorder
        , presetRow  state
        , hBorder
        , historyRow state
        , hBorder
        , statusRow  state
        , mainHelpBar
        ]
        -- Append a JACK warning banner below the help bar when not connected.
        ++ jackWarningBanner state
  where
    badge
        | tuiTuningMode state = " [TUNER]"
        | not (tuiRunning state) = " [PAUSED]"
        | otherwise = ""
    mainTitle = " ◈  " ++ intersperse '·' "DeMoD-NOTE" ++ badge ++ "  ◈ "

-- Full-width warning banner rendered only when JACK is unhealthy.
jackWarningBanner :: TUIState -> [Widget ()]
jackWarningBanner state = case tuiJackStatus state of
    JackConnected -> []
    JackReconnecting ->
        let blinkOn = tuiTick state `mod` 2 == 0
            atr     = if blinkOn then jackWarnAttr else dimAttr
        in  [ withAttr atr $ hCenter $ str "◐  JACK RECONNECTING — AUDIO PAUSED  ◐" ]
    JackDisconnected ->
        let blinkOn = tuiTick state `mod` 2 == 0
            atr     = if blinkOn then jackBadAttr else dimAttr
        in  [ withAttr atr $ hCenter $ str "○  JACK DISCONNECTED — CHECK ROUTING  ○" ]
    JackError msg ->
        [ withAttr jackBadAttr $ hCenter $
            str ("○  JACK ERROR: " ++ take 40 msg ++ "  ○") ]

-- ── Top row: J A C K  │  T A P  B P M  │  S I G N A L ───────────────────────

topRow :: TUIState -> Widget ()
topRow state = hBox
    [ jackPanel   state
    , vBorder
    , bpmPanel    state
    , vBorder
    , signalPanel state
    ]

jackPanel :: TUIState -> Widget ()
jackPanel state = padAll 1 $ hLimit 22 $
    vBox
        [ withAttr dimAttr (str (spaced "JACK"))
        , hBox [ withAttr dotAtr (str dot), str " ", withAttr ja (str lbl) ]
        ]
  where
    status  = tuiJackStatus state
    blinkOn = tuiTick state `mod` 2 == 0
    ja      = jackAttr status
    -- Blink the dot to draw attention when JACK is unhealthy
    dotAtr  = case status of
        JackConnected -> jackGoodAttr
        _             -> if blinkOn then ja else dimAttr
    (dot, lbl) = jackDisplay status

jackDisplay :: JackStatus -> (String, String)
jackDisplay JackConnected    = ("●", "CONNECTED")
jackDisplay JackDisconnected = ("○", "DISCONNECTED")
jackDisplay JackReconnecting = ("◐", "RECONNECTING")
jackDisplay (JackError msg)  = ("○", "ERR: " ++ take 9 msg)

jackAttr :: JackStatus -> AttrName
jackAttr JackConnected    = jackGoodAttr
jackAttr JackDisconnected = jackBadAttr
jackAttr JackReconnecting = jackWarnAttr
jackAttr (JackError _)    = jackBadAttr

bpmPanel :: TUIState -> Widget ()
bpmPanel state = padAll 1 $ hLimit 24 $
    vBox
        [ withAttr dimAttr (str (spaced "TAP BPM"))
        , withAttr bpmAttr (str (spaced bpmStr))
        , hBox
            [ withAttr highConfAttr (str tapsOn)
            , withAttr dimAttr      (str tapsOff)
            , withAttr dimAttr      (str "  ")
            , withAttr highConfAttr (str qualityRing)
            , withAttr dimAttr      (str "  [SPC]")
            ]
        ]
  where
    bpmStr      = show (round (tuiBPM state) :: Int)
    tapCount    = length (tuiTapTimes state)
    tapsOn      = replicate tapCount '●'
    tapsOff     = replicate (max 0 (maxTapSamples - tapCount)) '○'
    qualityRing = tapQualityRing (tuiTapTimes state)

signalPanel :: TUIState -> Widget ()
signalPanel state = padAll 1 $
    vBox
        [ withAttr dimAttr (str (spaced "SIGNAL"))
        , hBox
            [ withAttr dimAttr            (str "CONF  ")
            , withAttr (confidenceAttr c) (str (confidenceBar c))
            , withAttr (confidenceAttr c) (str (" " ++ show pct ++ "%  " ++ confGlyph c))
            ]
        , hBox
            [ withAttr dimAttr   (str "LAT   ")
            , withAttr latAttr   (str latDot)
            , str " "
            , withAttr valueAttr (str (showFixed1 lat ++ " ms"))
            , withAttr dimAttr   (str ("  " ++ latLbl))
            ]
        ]
  where
    c                         = tuiConfidence state
    pct                       = round (c * 100) :: Int
    lat                       = tuiLatency state
    (latDot, latLbl, latAttr) = latencyInfo lat
    confGlyph v
        | v >= 0.75 = "● strong"
        | v >= 0.40 = "◐ moderate"
        | otherwise = "○ weak"

-- ── Hero panel ────────────────────────────────────────────────────────────────

heroPanel :: TUIState -> Widget ()
heroPanel state =
    withBorderStyle innerBorder $
    borderWithLabel heroLabel $
    padAll 1 $ padLeftRight 2 $
    case tuiLastNote state of
        Nothing ->
            let waitGlyphs = ["○", "◌", "○", "◎"] :: [String]
                glyph      = safeIndex waitGlyphs (tuiTick state `mod` 4) "○"
            in  hBox [ withAttr dimAttr (str (glyph ++ "  ─  awaiting input")) ]
        Just (n, v) ->
            hBox
                [ withAttr heroNoteAttr (str (midiNotePart n))
                , withAttr heroOctAttr  (str (midiOctavePart n))
                , withAttr dimAttr      (str "   ·   midi ")
                , withAttr valueAttr    (str (show n))
                , withAttr dimAttr      (str "   ·   vel ")
                , withAttr velAttr      (str (vuBarWithPeak 12 v (tuiPeakVelocity state)))
                , withAttr dimAttr      (str ("  " ++ show v))
                ]
  where
    -- Border label flashes accent on note arrival, dims after 3 ticks.
    labelAtr = if tuiNoteFlash state > 0 then accentAttr else dimAttr
    heroLabel = withAttr labelAtr $ str " L A S T  N O T E "

-- ── Waveform panel ───────────────────────────────────────────────────────────

waveformPanel :: TUIState -> Widget ()
waveformPanel state =
    withBorderStyle innerBorder $
    borderWithLabel waveLabel $
    padTopBottom 1 $ padLeftRight 2 $
    vBox
        [ withAttr waveAttr (str (sparkLine wave))
        , str " "
        , vBox (map (withAttr waveAttr . str) (waveformRows wave 5))
        ]
  where
    wave    = tuiWaveform state
    runAttr = if tuiRunning state then highConfAttr else midConfAttr
    runStr  = if tuiRunning state then "▶ RUNNING" else "⏸ PAUSED  [p] resume"
    clipW   = if waveformIsClipping wave
                then hBox [ withAttr dimAttr (str "  "), withAttr lowConfAttr (str "⚡ CLIP") ]
                else emptyWidget
    waveLabel = hBox
        [ withAttr dimAttr (str " W A V E F O R M  ")
        , withAttr runAttr (str runStr)
        , clipW
        , withAttr dimAttr (str " ")
        ]

-- ── Tuning panel ─────────────────────────────────────────────────────────────

tuningPanel :: TUIState -> Widget ()
tuningPanel state =
    withBorderStyle innerBorder $
    borderWithLabel
        (withAttr dimAttr $ str " C H R O M A T I C  T U N E R  ─  [t] exit ") $
    padAll 1 $ padLeftRight 2 $
    vBox [ noteRow, str " ", centsRow, str " ", statusWidget ]
  where
    ta    = if tuiTuningInTune state then tuningGoodAttr else tuningBadAttr
    cents = tuiTuningCents state

    noteRow = case tuiTuningNote state of
        Nothing -> withAttr dimAttr (str "○  play a note to begin tuning")
        Just n  -> hBox
            [ withAttr dimAttr      (str "N O T E  ")
            , withAttr heroNoteAttr (str (midiToName n))
            ]

    cSign   = if cents >= 0 then "+" else ""
    centsRow = vBox
        [ withAttr ta (str (centsBar cents))
        , hBox
            [ withAttr dimAttr (str "  ◄ flat")
            , withAttr ta      (str ("   " ++ cSign ++ show (round cents :: Int) ++ " ¢   "))
            , withAttr dimAttr (str "sharp ►")
            ]
        ]

    (dot, msg) = tuneStatus (tuiTuningInTune state) cents
    statusWidget = hBox [ withAttr ta (str dot), withAttr ta (str msg) ]

tuneStatus :: Bool -> Double -> (String, String)
tuneStatus True  _     = ("★", "  I N  T U N E")
tuneStatus False cents
    | abs cents <= 15.0 = ("◐", "  CLOSE  ─  adjust slightly")
    | cents < 0         = ("○", "  FLAT   ─  tune up ↑")
    | otherwise         = ("○", "  SHARP  ─  tune down ↓")

-- ── Preset row ───────────────────────────────────────────────────────────────

presetRow :: TUIState -> Widget ()
presetRow state = hBox [ scalePanel state, vBorder, arpeggioPanel state ]

scalePanel :: TUIState -> Widget ()
scalePanel state = padAll 1 $
    hBox
        [ withAttr dimAttr   (str (spaced "SCALE" ++ "  "))
        , withAttr valueAttr (str (currentScale state))
        , withAttr dimAttr   (str ("  [" ++ show (tuiScaleIndex state + 1)
            ++ "/" ++ show (length availableScales) ++ "]"))
        , withAttr dimAttr   (str "   [s]›  [S]‹")
        ]

arpeggioPanel :: TUIState -> Widget ()
arpeggioPanel state = padAll 1 $
    hBox
        [ withAttr dimAttr   (str (spaced "ARPEG" ++ "  "))
        , withAttr valueAttr (str (currentArpeggio state))
        , withAttr dimAttr   (str ("  [" ++ show (tuiArpeggioIndex state + 1)
            ++ "/" ++ show (length availableArpeggios) ++ "]"))
        , withAttr dimAttr   (str "   [a]›  [A]‹")
        ]

-- ── History row ──────────────────────────────────────────────────────────────

historyRow :: TUIState -> Widget ()
historyRow state = padAll 1 $
    hBox
        [ withAttr dimAttr (str (spaced "HIST" ++ "  "))
        , histContent
        ]
  where
    hist = tuiNoteHistory state
    histContent
        | null hist = withAttr dimAttr (str "─  no notes yet  ─")
        | otherwise = hBox $ intersperse sep $ map noteChip (take maxNoteHistory hist)
    sep = withAttr dimAttr (str "  ╌  ")
    -- Sharp notes (containing '#') rendered in accent; naturals in value.
    noteChip (n, v) =
        let name = midiToName n
            atr  = if '#' `elem` name then heroNoteAttr else valueAttr
        in  hBox [ withAttr atr (str name)
                 , withAttr dimAttr (str ("·" ++ show v))
                 ]

-- ── Status + help bars ───────────────────────────────────────────────────────

statusRow :: TUIState -> Widget ()
statusRow state = padLeftRight 1 $
    hBox
        [ withAttr accentAttr (str "◈  ")
        , withAttr statusAttr (str (tuiStatusMessage state))
        ]

mainHelpBar :: Widget ()
mainHelpBar = withAttr helpAttr $ padLeftRight 1 $
    str $ intercalate "  ·  "
        [ "[SPC] tap", "[s/S] scale", "[a/A] arpeg"
        , "[t] tuner", "[f] font", "[c] theme", "[p] pause", "[r] reset", "[q] quit"
        ]

-- ─────────────────────────────────────────────────────────────────────────────
-- Event handling
-- ─────────────────────────────────────────────────────────────────────────────

handleEvent :: BrickEvent () TUIEvent -> EventM () TUIState ()

-- ── Animation tick (both modes) ───────────────────────────────────────────────
handleEvent (AppEvent TUITick) = modify' $ \s ->
    let tick'   = tuiTick s + 1
        flash'  = max 0 (tuiNoteFlash s - 1)
        -- Peak velocity decays by 1 every 3 ticks (~0.8 sec full decay ~3 min).
        peak'   = if tick' `mod` 3 == 0
                    then max 0 (tuiPeakVelocity s - 1)
                    else tuiPeakVelocity s
        -- Status reverts to a context-appropriate idle message when expired.
        (msg', expiry') = case tuiStatusExpiry s of
            Just e | tick' >= e ->
                let idle = idleStatus s
                in  (idle, Nothing)
            other -> (tuiStatusMessage s, other)
    in  s { tuiTick          = tick'
          , tuiNoteFlash     = flash'
          , tuiPeakVelocity  = peak'
          , tuiStatusMessage = msg'
          , tuiStatusExpiry  = expiry'
          }

handleEvent ev = do
    mode <- gets tuiMode
    case mode of
        StartMode     -> handleStartEvent ev
        MainMode      -> handleMainEvent  ev
        SoundFontMode -> handleSoundFontEvent ev

-- Idle status message reflects current session context.
idleStatus :: TUIState -> String
idleStatus s = case tuiJackStatus s of
    JackDisconnected -> "WAITING FOR JACK  ─  CHECK ROUTING"
    JackReconnecting -> "RECONNECTING TO JACK…"
    JackError msg    -> "JACK ERROR  ─  " ++ take 40 msg
    JackConnected
        | tuiTuningMode s -> "TUNER ACTIVE  ─  [t] to exit"
        | not (tuiRunning s) -> "PAUSED  ─  [p] to resume"
        | otherwise ->
            let sc = safeIndex availableScales (tuiScaleIndex s) "?"
            in  "LISTENING  ─  " ++ sc

handleStartEvent :: BrickEvent () TUIEvent -> EventM () TUIState ()
handleStartEvent (VtyEvent e) = case e of
    EvKey (KChar 'q') [] -> halt
    EvKey KEsc         [] -> halt

    -- ENTER: transition to main UI only when JACK is fully connected.
    -- JackReconnecting is not sufficient — audio routing must be confirmed.
    EvKey KEnter [] -> do
        jack <- gets tuiJackStatus
        case jack of
            JackConnected    -> modify' $ \s ->
                s { tuiMode = MainMode
                  , tuiStatusMessage = "JACK CONNECTED  ─  AWAITING INPUT"
                  , tuiStatusExpiry  = Nothing   -- persistent until next action
                  }
            JackReconnecting -> modify' $ setStatus "RECONNECTING  ─  WAIT FOR JACK…"
            JackDisconnected -> modify' $ setStatus "JACK NOT CONNECTED  ─  CHECK ROUTING"
            JackError msg    -> modify' $ setStatus ("JACK ERROR  ─  " ++ take 40 msg)

    -- 1–5: select palette (live preview — attr map is dynamic)
    EvKey (KChar c) [] | Just p <- paletteFromChar c ->
        modify' $ \s ->
            s { tuiPalette = p
              , tuiStatusMessage = "THEME: " ++ paletteName p
              }

    _ -> return ()

-- Handle detection events in StartMode to update JACK status
handleStartEvent (AppEvent (TUIDetection det)) = modify' $ \s ->
    s { tuiJackStatus = deJackStatus det }

handleStartEvent _ = return ()

handleMainEvent :: BrickEvent () TUIEvent -> EventM () TUIState ()
handleMainEvent (VtyEvent e) = case e of
    EvKey (KChar 'q') [] -> halt
    EvKey KEsc         [] -> halt

    EvKey (KChar ' ') [] -> do
        now <- liftIO getCurrentTime
        modify' $ \s ->
            let taps   = take maxTapSamples (now : tuiTapTimes s)
                newBPM = computeBPM taps
            in  setStatus ("TAP " ++ show (length taps)
                                  ++ " / 8  ─  " ++ show (round newBPM :: Int) ++ " BPM")
                    (s { tuiTapTimes = taps, tuiBPM = newBPM })

    EvKey (KChar 's') [] -> cycleScale    1
    EvKey (KChar 'S') [] -> cycleScale  (-1)
    EvKey (KChar 'a') [] -> cycleArpeggio    1
    EvKey (KChar 'A') [] -> cycleArpeggio  (-1)

    -- Cycle color palette (in-session)
    EvKey (KChar 'c') [] -> modify' $ \s ->
        let palettes = allPalettes
            idx      = length (takeWhile (/= tuiPalette s) palettes)
            next     = safeIndex palettes ((idx + 1) `mod` length palettes) Synthwave
        in  setStatus ("THEME: " ++ paletteName next) (s { tuiPalette = next })

    -- Or pick palette directly by number
    EvKey (KChar c) [] | Just p <- paletteFromChar c ->
        modify' $ \s -> setStatus ("THEME: " ++ paletteName p) (s { tuiPalette = p })

    EvKey (KChar 'p') [] -> modify' $ \s ->
        let r = not (tuiRunning s)
        in  setStatus (if r then "RUNNING" else "PAUSED") (s { tuiRunning = r })

    EvKey (KChar 'r') [] -> modify' $ \s ->
        setStatus "TAP TEMPO RESET  ─  120 BPM"
            (s { tuiTapTimes    = []
               , tuiBPM         = 120.0
               , tuiPeakVelocity = 0
               , tuiLastNote    = Nothing   -- clear stale hero panel
               , tuiNoteHistory = []        -- clear note trail
               })

    EvKey (KChar 't') [] -> modify' $ \s ->
        let m = not (tuiTuningMode s)
        in  setStatus (if m then "TUNER ON  ─  PLAY A NOTE" else "TUNER OFF")
                (s { tuiTuningMode = m })

    -- Open SoundFont browser
    EvKey (KChar 'f') [] -> modify' $ \s -> s { tuiMode = SoundFontMode }

    _ -> return ()

handleMainEvent (AppEvent (TUIStatusMsg msg)) =
    modify' $ setStatus msg

-- Detection event: update all signal metrics; also handle JACK status
-- transitions so the UI reacts visually to drops and reconnections.
handleMainEvent (AppEvent (TUIDetection det)) = modify' $ \s ->
    let newJack   = deJackStatus det
        prevJack  = tuiJackStatus s
        jackChanged = newJack /= prevJack
        audioLive = newJack == JackConnected

        mNote = if audioLive then deNote det else Nothing
        hist' = case mNote of
            Nothing     -> tuiNoteHistory s  -- preserve history even when disconnected
            Just (n, v) -> take maxNoteHistory ((n, v) : tuiNoteHistory s)
        wave' = if audioLive
                    then take waveformSamples (deWaveform det ++ repeat 0.0)
                    else replicate waveformSamples 0.0

        -- Flash hero border for 3 ticks on any new note-on.
        flash' = case (tuiLastNote s, mNote) of
            (_, Just _) | mNote /= tuiLastNote s -> 3
            _                                     -> tuiNoteFlash s

        -- Peak velocity: update on new note-on, otherwise hold.
        peak' = case mNote of
            Just (_, v) -> max v (tuiPeakVelocity s)
            Nothing     -> tuiPeakVelocity s

        -- Status: only override on JACK transitions so tap/scale msgs survive.
        s1 = s { tuiLastNote      = mNote
               , tuiNoteHistory   = hist'
               , tuiConfidence    = if audioLive then deConfidence det else 0.0
               , tuiLatency       = if audioLive then deLatency    det else 0.0
               , tuiWaveform      = wave'
               , tuiTuningNote    = if audioLive then deTuningNote    det else Nothing
               , tuiTuningCents   = if audioLive then deTuningCents   det else 0.0
               , tuiTuningInTune  = if audioLive then deTuningInTune  det else False
               , tuiJackStatus    = newJack
               , tuiNoteFlash     = flash'
               , tuiPeakVelocity  = peak'
               }
    in  if not jackChanged
            then s1
            else case (prevJack, newJack) of
                (_, JackConnected)    -> setStatus "JACK RECONNECTED  ─  AWAITING INPUT" s1
                (_, JackDisconnected) -> setStatus "JACK DISCONNECTED  ─  CHECK ROUTING" s1
                (_, JackReconnecting) -> setStatus "JACK RECONNECTING  ─  PLEASE WAIT"   s1
                (_, JackError msg)    -> setStatus ("JACK ERROR  ─  " ++ take 30 msg)    s1

handleMainEvent _ = return ()

-- ── SoundFont mode event handler ─────────────────────────────────────────────

handleSoundFontEvent :: BrickEvent () TUIEvent -> EventM () TUIState ()
handleSoundFontEvent (VtyEvent e) = case e of
    EvKey KEsc         [] -> modify' $ \s -> s { tuiMode = MainMode }
    EvKey (KChar 'q')  [] -> modify' $ \s -> s { tuiMode = MainMode }
    EvKey KEnter       [] -> do
        -- Open browser: try xdg-open (Linux), then open (macOS).
        -- Using spawnCommand with a hardcoded URL; if this ever becomes
        -- user-configurable, switch to System.Process.rawSystem to avoid
        -- shell injection.
        _ <- liftIO $ spawnCommand
            "xdg-open https://musical-artifacts.com 2>/dev/null || open https://musical-artifacts.com 2>/dev/null"
        modify' $ \s ->
            s { tuiMode = MainMode
              , tuiStatusMessage = "SOUNDFONT: Visit musical-artifacts.com to download!"
              }
    _ -> return ()
handleSoundFontEvent _ = return ()

-- ─────────────────────────────────────────────────────────────────────────────
-- SoundFont UI
-- ─────────────────────────────────────────────────────────────────────────────

drawSoundFontUI :: TUIState -> Widget ()
drawSoundFontUI state =
    withBorderStyle retroBorder $
    borderWithLabel (withAttr titleAttr $ str sfTitle) $
    padAll 2 $
    vBox
        [ hCenter $ withAttr splashTitleAttr $ str "★  S O U N D F O N T  ★"
        , str " "
        , hCenter $ withAttr splashSubAttr $ str "Download SoundFonts from"
        , str " "
        , hCenter $ withAttr valueAttr $ str "musical-artifacts.com"
        , str " "
        , donationBanner
        , str " "
        , hCenter $ withAttr splashBodyAttr $ str "SoundFonts will be saved to:"
        , str " "
        , hCenter $ withAttr dimAttr $ str "/etc/demod/sf"
        , hCenter $ withAttr dimAttr $ str "~/.local/share/soundfonts"
        , str " "
        , hCenter $ withAttr dimAttr $ str "Press [Enter] to open in browser"
        , str " "
        , hCenter $ pressEscapeWidget state
        ]
  where
    sfTitle = " ◈  SOUNDFONT  ◈ "
    donationBanner = withBorderStyle innerBorder $
        border $ padAll 1 $ vBox
            [ hCenter $ withAttr heroNoteAttr $ str "PLEASE SUPPORT THIS SERVICE!"
            , str " "
            , hCenter $ withAttr splashBodyAttr $ 
                str "musical-artifacts.com is run by volunteers."
            , hCenter $ withAttr splashBodyAttr $ 
                str "Consider donating to keep it running."
            ]

pressEscapeWidget :: TUIState -> Widget ()
pressEscapeWidget state =
    let blinkOn = tuiTick state `mod` 2 == 0
        atr     = if blinkOn then accentAttr else dimAttr
    in  withAttr atr $ str "[Esc] Back  ·  [Enter] Open Browser"

-- ─────────────────────────────────────────────────────────────────────────────

-- | Haskell's built-in `mod` returns negative values when the dividend is
-- negative (e.g. (0 + (-1)) `mod` 11 = -1, not 10).  Use this instead.
posMod :: Int -> Int -> Int
posMod x n = ((x `mod` n) + n) `mod` n

cycleScale :: Int -> EventM () TUIState ()
cycleScale dir = modify' $ \s ->
    let n = (tuiScaleIndex s + dir) `posMod` length availableScales
    in  setStatus ("SCALE  ─  " ++ safeIndex availableScales n "?")
            (s { tuiScaleIndex = n })

cycleArpeggio :: Int -> EventM () TUIState ()
cycleArpeggio dir = modify' $ \s ->
    let n = (tuiArpeggioIndex s + dir) `posMod` length availableArpeggios
    in  setStatus ("ARPEGGIO  ─  " ++ safeIndex availableArpeggios n "?")
            (s { tuiArpeggioIndex = n })

computeBPM :: [UTCTime] -> Double
computeBPM []  = 120.0
computeBPM [_] = 120.0
computeBPM ts  =
    -- ts is newest-first; pairs are (newer, older).
    -- diffUTCTime newer older is positive, giving the inter-tap interval.
    let pairs     = zip ts (safeTail ts)
        intervals = map (\(newer, older) -> realToFrac (diffUTCTime newer older) :: Double) pairs
        avg       = sum intervals / fromIntegral (length intervals)
    in  clamp 20.0 300.0 (60.0 / avg)

-- Tap quality: standard deviation of inter-tap intervals, normalised.
-- Returns a 0–4 ring string ("○○○○" → "●●●●") reflecting timing tightness.
tapQualityRing :: [UTCTime] -> String
tapQualityRing ts
    | length ts < 4 = replicate 4 '○'   -- need 4 taps for meaningful variance
    | otherwise =
        -- ts is newest-first; (newer, older) pairs give positive intervals.
        let pairs    = zip ts (safeTail ts)
            ivs      = map (\(newer, older) -> realToFrac (diffUTCTime newer older) :: Double) pairs
            mean     = sum ivs / fromIntegral (length ivs)
            variance = sum (map (\x -> (x - mean)^(2::Int)) ivs) / fromIntegral (length ivs)
            stddev   = sqrt variance
            filled   = clamp 1 4 $ if stddev < 0.010 then 4
                                    else if stddev < 0.030 then 3
                                    else if stddev < 0.060 then 2
                                    else 1
        in  replicate filled '●' ++ replicate (4 - filled) '○'

-- Set a status message that auto-reverts to idle after ~3 seconds (12 ticks @ 280 ms).
setStatus :: String -> TUIState -> TUIState
setStatus msg s = s { tuiStatusMessage = msg, tuiStatusExpiry = Just (tuiTick s + statusExpiryTicks) }

-- ─────────────────────────────────────────────────────────────────────────────
-- Attribute names
-- ─────────────────────────────────────────────────────────────────────────────

-- Structural / chrome
titleAttr, valueAttr, dimAttr, bpmAttr   :: AttrName
velAttr, waveAttr, helpAttr              :: AttrName
accentAttr, statusAttr                   :: AttrName
-- Signal quality
highConfAttr, midConfAttr, lowConfAttr   :: AttrName
-- Hero note
heroNoteAttr, heroOctAttr               :: AttrName
-- Tuning
tuningGoodAttr, tuningBadAttr           :: AttrName
-- JACK
jackGoodAttr, jackBadAttr, jackWarnAttr :: AttrName
-- Splash / start menu
splashTitleAttr, splashSubAttr          :: AttrName
splashDivAttr, splashBodyAttr           :: AttrName
-- Brick progress bar (required by ProgressBar widget)
progressCompleteAttr, progressIncompleteAttr :: AttrName

titleAttr            = attrName "title"
valueAttr            = attrName "value"
dimAttr              = attrName "dim"
bpmAttr              = attrName "bpm"
velAttr              = attrName "vel"
waveAttr             = attrName "wave"
helpAttr             = attrName "help"
accentAttr           = attrName "accent"
statusAttr           = attrName "status"
highConfAttr         = attrName "highConf"
midConfAttr          = attrName "midConf"
lowConfAttr          = attrName "lowConf"
heroNoteAttr         = attrName "heroNote"
heroOctAttr          = attrName "heroOct"
tuningGoodAttr       = attrName "tuningGood"
tuningBadAttr        = attrName "tuningBad"
jackGoodAttr         = attrName "jackGood"
jackBadAttr          = attrName "jackBad"
jackWarnAttr         = attrName "jackWarn"
splashTitleAttr      = attrName "splashTitle"
splashSubAttr        = attrName "splashSub"
splashDivAttr        = attrName "splashDiv"
splashBodyAttr       = attrName "splashBody"
progressCompleteAttr   = attrName "progressComplete"
progressIncompleteAttr = attrName "progressIncomplete"

-- ─────────────────────────────────────────────────────────────────────────────
-- Entry points
-- ─────────────────────────────────────────────────────────────────────────────

-- | Fork a thread that restarts itself on exception after a 1-second back-off.
--   Prevents silent thread death from killing animations or the backend bridge.
supervisedForkIO :: IO () -> IO ()
supervisedForkIO action = void $ forkIO $ forever $
    action `catch` \(_ :: SomeException) -> threadDelay 1_000_000

-- | Shared setup: choose a tip, create the BChan, and spawn the animation
--   tick thread.  Both runners call this then add their own bridge thread.
initTUICommon :: Config -> IO (TUIState, BChan TUIEvent)
initTUICommon cfg = do
    posix <- getPOSIXTime
    let tipIdx = (floor posix :: Int) `mod` length tips
        tip    = safeIndex tips tipIdx "Enjoy the music."
        initSt = initialTUIState cfg tip
    bc <- newBChan bChanCapacity
    -- Animation tick: drives logo shimmer and waiting-glyph pulse.
    -- The inner `forever` keeps the delay+write loop running; the outer
    -- `supervisedForkIO` restarts the whole thread if it crashes.
    supervisedForkIO $ forever $ do
        threadDelay tickIntervalUs
        writeBChan bc TUITick
    return (initSt, bc)

runTUI :: Config -> IO ()
runTUI cfg = runTUIWithChannel cfg Nothing

-- | Run TUI driven by a shared TVar from the backend.
--   Polls at 100 Hz but only writes to BChan when relevant state changes,
--   preventing channel saturation and tick-thread starvation.
runTUIWithState :: Config -> TVar ReactorState -> IO ()
runTUIWithState cfg stateVar = do
    (initSt, bc) <- initTUICommon cfg
    prevRef <- newIORef Nothing
    -- Change-gated poll at 100 Hz.  The `forever` ensures continuous polling;
    -- without it the previous code polled exactly once.
    supervisedForkIO $ forever $ do
        threadDelay 10_000  -- 100 Hz
        rs <- atomically $ readTVar stateVar
        prev <- readIORef prevRef
        let mNote = case currentNotes rs of { [] -> Nothing; (n:_) -> Just n }
            key   = ( mNote
                    , noteStateMach       rs
                    , jackStatus          rs
                    , detectionConfidence rs
                    , take 64 (latestWaveform rs)
                    )
        when (prev /= Just key) $ do
            writeIORef prevRef (Just key)
            let det = DetectionEvent
                    { deNote         = mNote
                    , deConfidence   = detectionConfidence rs
                    , deLatency      = detectionLatency    rs
                    , deWaveform     = latestWaveform      rs
                    , deState        = noteStateMach       rs
                    , deTuningNote   = detectedTuningNote   rs
                    , deTuningCents  = detectedTuningCents  rs
                    , deTuningInTune = detectedTuningInTune rs
                    , deJackStatus   = jackStatus          rs
                    }
            writeBChan bc (TUIDetection det)
    let buildVty = mkVty VtyConfig.defaultConfig
    initVty <- buildVty
    void $ customMain initVty buildVty (Just bc) tuiApp initSt

-- | Launch the TUI with an optional legacy Chan bridge.
--   Always uses customMain so the animation tick thread drives start-menu effects.
runTUIWithChannel :: Config -> Maybe (Chan DetectionEvent) -> IO ()
runTUIWithChannel cfg mChan = do
    (initSt, bc) <- initTUICommon cfg
    case mChan of
        Nothing   -> return ()
        Just chan -> supervisedForkIO $ forever $ do
            ev <- readChan chan
            writeBChan bc (TUIDetection ev)
    let buildVty = mkVty VtyConfig.defaultConfig
    initVty <- buildVty
    void $ customMain initVty buildVty (Just bc) tuiApp initSt