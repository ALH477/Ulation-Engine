-- |
-- Module      : DeMoDNote.BPM
-- Description : BPM detection, tap tempo, and quantization
-- Copyright   : 2026
-- License     : MIT
--
-- This module provides BPM detection from tap tempo, beat quantization,
-- and time signature handling.

module DeMoDNote.BPM (
    BPMState(..),
    TapState(..),
    BPMMode(..),
    TimeSignature(..),
    QuantizationGrid(..),
    getGridDivision,
    defaultTimeSignature,
    newBPMState,
    tapBeat,
    getCurrentBPM,
    quantizeToGrid,
    quantizeNoteOnset,
    getBeatPosition,
    isDownbeat,
    autoDetectBPM,
    setBPM,
    setTimeSignature,
    setSwing,
    setQuantization,
    getQuantizationGrid,
    -- Utility functions
    msToSamples,
    samplesToMs,
    msToSamplesSR,
    samplesToMsSR,
    beatToMs,
    msToBeat,
    swingOffset,
    -- Constants
    defaultBPM,
    minBPM,
    maxBPM
) where

import Data.Word (Word64)
import Data.List (sort)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Concurrent.STM (TVar, newTVarIO, readTVar, writeTVar, atomically)
import DeMoDNote.Util (safeTail)

-- BPM ranges
defaultBPM :: Double
defaultBPM = 120.0

minBPM :: Double
minBPM = 30.0

maxBPM :: Double
maxBPM = 300.0

-- Time signatures
data TimeSignature = TimeSignature {
    tsBeats :: Int,      -- Beats per measure (top number)
    tsNoteValue :: Int   -- Note value (bottom number: 4=quarter, 8=eighth)
} deriving (Eq, Show)

defaultTimeSignature :: TimeSignature
defaultTimeSignature = TimeSignature 4 4

-- BPM detection modes
data BPMMode = 
    TapTempo |           -- Manual tapping
    AutoDetect |         -- From note onsets
    ExternalSync |       -- From MIDI clock
    FixedBPM             -- Static BPM
    deriving (Eq, Show)

-- Quantization grid
data QuantizationGrid =
    QOff |
    Q32nd |
    Q16th |
    Q8th |
    QQuarter |
    QHalf
    deriving (Eq, Show, Enum)

getGridDivision :: QuantizationGrid -> Double
getGridDivision QOff = 0
getGridDivision Q32nd = 1/8
getGridDivision Q16th = 1/4
getGridDivision Q8th = 1/2
getGridDivision QQuarter = 1
getGridDivision QHalf = 2

-- Tap tempo state
data TapState = TapState {
    tapTimes :: [Word64],
    lastTapTime :: Word64,
    tapCount :: Int
}

newTapState :: TapState
newTapState = TapState [] 0 0

-- Main BPM state
data BPMState = BPMState {
    bpmMode :: BPMMode,
    currentBPM :: TVar Double,
    targetBPM :: Double,
    timeSignature :: TVar TimeSignature,
    quantization :: TVar QuantizationGrid,
    swing :: TVar Double,
    tapState :: TVar TapState,
    startTime :: Word64,
    lastBeatTime :: TVar Word64,
    beatCount :: TVar Int,
    onsetHistory :: TVar [Word64],
    confidence :: TVar Double
}

-- Create new BPM state
newBPMState :: IO BPMState
newBPMState = do
    now <- getMicroTime
    bpmVar <- newTVarIO defaultBPM
    tsVar <- newTVarIO defaultTimeSignature
    quantVar <- newTVarIO Q16th
    swingVar <- newTVarIO 0.0
    tapVar <- newTVarIO newTapState
    lastBeatVar <- newTVarIO now
    beatCountVar <- newTVarIO 0
    onsetVar <- newTVarIO []
    confVar <- newTVarIO 0.0
    return $ BPMState {
        bpmMode = TapTempo,
        currentBPM = bpmVar,
        targetBPM = defaultBPM,
        timeSignature = tsVar,
        quantization = quantVar,
        swing = swingVar,
        tapState = tapVar,
        startTime = now,
        lastBeatTime = lastBeatVar,
        beatCount = beatCountVar,
        onsetHistory = onsetVar,
        confidence = confVar
    }

-- Get current time in microseconds
getMicroTime :: IO Word64
getMicroTime = do
    t <- getPOSIXTime
    return $ round (t * 1000000)

-- Tap a beat (spacebar or button press)
tapBeat :: BPMState -> IO Double
tapBeat state = do
    now <- getMicroTime
    atomically $ do
        tap <- readTVar (tapState state)
        let taps = take 7 (now : tapTimes tap)
            newTap = TapState taps now (length taps)
        writeTVar (tapState state) newTap
        
        -- Calculate BPM from intervals
        if length taps >= 2
            then do
                let intervals = zipWith (-) taps (safeTail taps)
                    avgInterval = fromIntegral (sum intervals) / fromIntegral (length intervals)
                    bpm = 60000000.0 / avgInterval  -- Convert to BPM
                    clampedBPM = max minBPM (min maxBPM bpm)
                writeTVar (currentBPM state) clampedBPM
                return clampedBPM
            else return 0.0

-- Get current BPM
getCurrentBPM :: BPMState -> IO Double
getCurrentBPM state = atomically $ readTVar (currentBPM state)

-- Set BPM directly
setBPM :: BPMState -> Double -> IO ()
setBPM state bpm = do
    let clamped = max minBPM (min maxBPM bpm)
    atomically $ writeTVar (currentBPM state) clamped

-- Set time signature
setTimeSignature :: BPMState -> Int -> Int -> IO ()
setTimeSignature state beats noteValue = do
    let ts = TimeSignature beats noteValue
    atomically $ writeTVar (timeSignature state) ts

-- Set swing amount (0.0 - 1.0)
setSwing :: BPMState -> Double -> IO ()
setSwing state amount = do
    let clamped = max 0.0 (min 1.0 amount)
    atomically $ writeTVar (swing state) clamped

-- Set quantization
setQuantization :: BPMState -> QuantizationGrid -> IO ()
setQuantization state grid = do
    atomically $ writeTVar (quantization state) grid

-- Get quantization grid divisions
getQuantizationGrid :: BPMState -> IO [Double]
getQuantizationGrid state = do
    grid <- atomically $ readTVar (quantization state)
    ts <- atomically $ readTVar (timeSignature state)
    let division = getGridDivision grid
        beats = fromIntegral (tsBeats ts)
    return $ if division == 0
       then []
       else [0, division .. beats]

-- Quantize a time to the grid
quantizeToGrid :: BPMState -> Word64 -> IO Word64
quantizeToGrid state time = do
    grid <- atomically $ readTVar (quantization state)
    if grid == QOff
       then return time
       else do
           bpm <- atomically $ readTVar (currentBPM state)   -- live BPM, not stale targetBPM
           let beatMs  = 60000.0 / bpm
               gridMs  = beatMs * getGridDivision grid
               timeMs  = fromIntegral time / 1000.0 :: Double
               gridTime = fromIntegral (round (timeMs / gridMs :: Double) :: Int) * gridMs :: Double
           return $ round (gridTime * 1000 :: Double)

-- Quantize a note onset to the beat grid
quantizeNoteOnset :: BPMState -> Word64 -> IO Word64
quantizeNoteOnset state onsetTime = do
    bpm  <- atomically $ readTVar (currentBPM state)   -- live BPM
    ts   <- atomically $ readTVar (timeSignature state)
    grid <- atomically $ readTVar (quantization state)
    let beatMs      = 60000.0 / bpm
        start       = startTime state
        elapsed     = onsetTime - start
        elapsedMs   = fromIntegral elapsed / 1000.0
        beatPos     = elapsedMs / beatMs
        measureLen  = fromIntegral (tsBeats ts)
        posInMeasure = beatPos `mod'` measureLen
        division    = getGridDivision grid
    if division == 0
       then return onsetTime
       else let quantizedPos = fromIntegral (round (posInMeasure / division :: Double) :: Int) * division :: Double
                offsetMs     = (quantizedPos - posInMeasure) * beatMs
            in return $ onsetTime + round (offsetMs * 1000 :: Double)

-- Get current beat position
getBeatPosition :: BPMState -> IO Double
getBeatPosition state = do
    now <- getMicroTime
    bpm <- getCurrentBPM state
    let start = startTime state
        elapsed = now - start
        elapsedMs = fromIntegral elapsed / 1000.0
        beatMs = 60000.0 / bpm
    return $ elapsedMs / beatMs

-- Check if current beat is a downbeat (beat 1)
isDownbeat :: BPMState -> IO Bool
isDownbeat state = do
    pos <- getBeatPosition state
    ts <- atomically $ readTVar (timeSignature state)
    let measureLen = fromIntegral (tsBeats ts)
        posInMeasure = pos `mod'` measureLen
    return $ posInMeasure < 1.0

-- Auto-detect BPM from note onsets
autoDetectBPM :: BPMState -> [Word64] -> IO (Maybe Double)
autoDetectBPM state onsets =
    if length onsets < 4
    then return Nothing
    else do
        -- Calculate intervals between consecutive onsets
        let sorted = sort onsets
            intervals = zipWith (-) (safeTail sorted) sorted
            -- Filter out intervals that are too short or too long
            validIntervals = filter (\i -> i > 100000 && i < 2000000) intervals  -- 100ms to 2s
        if length validIntervals < 3
        then return Nothing
        else do
            -- Find most common interval (mode)
            let -- Try to detect if intervals are consistent with a tempo
                intervalsMs = map (\i -> fromIntegral i / 1000.0 :: Double) validIntervals
                bpms = map (\ms -> 60000.0 / ms) intervalsMs
                avgBPM = sum bpms / fromIntegral (length bpms) :: Double
                -- Round to reasonable BPM
                roundedBPM = fromIntegral (round (avgBPM / 0.5 :: Double) :: Int) * 0.5 :: Double
                clampedBPM = max minBPM (min maxBPM roundedBPM)
            
            -- Calculate confidence based on consistency
            let variance = sum (map (\b -> (b - avgBPM) * (b - avgBPM)) bpms) / fromIntegral (length bpms) :: Double
                conf = max 0.0 (1.0 - variance / 100.0 :: Double) :: Double
            
            atomically $ do
                writeTVar (onsetHistory state) onsets
                writeTVar (confidence state) conf
            
            if conf > 0.5
            then do
                setBPM state clampedBPM
                return $ Just clampedBPM
            else return Nothing

-- Calculate swing offset for a specific beat
swingOffset :: BPMState -> Int -> IO Double
swingOffset state beatNum = do
    swingAmt <- atomically $ readTVar (swing state)
    return $ if swingAmt == 0.0 || even beatNum
       then 0.0
       else swingAmt * 0.5  -- Delay odd beats for swing feel

-- | Convert milliseconds to sample frames (assumes 96000 Hz).
-- Use 'msToSamplesSR' when the sample rate is known.
msToSamples :: Double -> Int
msToSamples ms = round (ms * 96.0)

-- | Convert sample frames to milliseconds (assumes 96000 Hz).
-- Use 'samplesToMsSR' when the sample rate is known.
samplesToMs :: Int -> Double
samplesToMs samples = fromIntegral samples / 96.0

-- | Convert milliseconds to sample frames at an explicit sample rate.
msToSamplesSR :: Int -> Double -> Int
msToSamplesSR sr ms = round (ms * fromIntegral sr / 1000.0)

-- | Convert sample frames to milliseconds at an explicit sample rate.
samplesToMsSR :: Int -> Int -> Double
samplesToMsSR sr samples = fromIntegral samples * 1000.0 / fromIntegral sr

-- Convert beat number to milliseconds
beatToMs :: Double -> Double -> Double
beatToMs bpm beat = (beat * 60000.0) / bpm

-- Convert milliseconds to beat number
msToBeat :: Double -> Double -> Double
msToBeat bpm ms = (ms * bpm) / 60000.0

-- Floating point modulo
mod' :: Double -> Double -> Double
mod' x y = x - y * fromIntegral (floor (x / y) :: Int)
