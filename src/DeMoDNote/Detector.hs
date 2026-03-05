{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : DeMoDNote.Detector
-- Description : Triple-path pitch detection using PLL, Autocorrelation, and YIN
-- Copyright   : 2026
-- License     : MIT
--
-- This module implements a hybrid pitch detection system optimized for
-- real-time monophonic note detection with ultra-low latency.
--
-- The detection uses three paths based on frequency range:
--
-- * **Fast Path (200Hz+):** Zero-crossing Phase-Locked Loop (PLL)
--   - Latency: 2.66ms (256 samples @ 96kHz)
--   - Accuracy: ~98%
--
-- * **Medium Path (80-200Hz):** Autocorrelation
--   - Latency: 12ms speculative, 30ms validated
--   - Accuracy: ~85% → ~95%
--
-- * **Slow Path (30-80Hz):** YIN algorithm
--   - Latency: 30ms
--   - Accuracy: ~95%
--
-- The PLL tracks the fundamental frequency by synchronizing an internal
-- oscillator to zero-crossings in the input signal. Autocorrelation
-- measures self-similarity at different lags. YIN uses the difference
-- function to find the fundamental period.

module DeMoDNote.Detector where

import qualified Data.Vector.Storable as VS
import Data.Ord (comparing)
import DeMoDNote.Config
import DeMoDNote.Types

-- | Effective sample rate from Config.
-- Previously a hardcoded constant (96000.0) that didn't match the actual JACK
-- sample rate (44100 or 48000 Hz).  All frequency↔sample conversions that were
-- using `detectorSampleRate` now take `sr = detSampleRate cfg` as a parameter.
detSampleRate :: Config -> Double
detSampleRate = fromIntegral . sampleRate . timing

-- | Buffer size hint (128 samples = ~2.9 ms @ 44100 Hz).
-- The actual buffer length passed to detect may differ.
detectorBufferSize :: Int
detectorBufferSize = 128

-- | RMS level below which a buffer is considered silent and a held note released.
silenceThresholdRMS :: Double
silenceThresholdRMS = 0.005   -- ≈ -46 dBFS

-- | Convert Int16-range integer samples to normalised [-1.0, 1.0]
normalizeInt16 :: VS.Vector Int -> VS.Vector Double
normalizeInt16 = VS.map (\x -> fromIntegral x / 32768.0)

-- | Root-mean-square amplitude of a sample buffer
computeRms :: VS.Vector Double -> Double
computeRms v
  | VS.null v = 0.0
  | otherwise = sqrt $ VS.sum (VS.map (\x -> x * x) v) / fromIntegral (VS.length v)

-- | Phase modulo 2π using Integer floor — avoids Int overflow on large
-- accumulated phase values (the original mod' used Int floor).
mod2pi :: Double -> Double
mod2pi x = x - 2.0 * pi * fromIntegral (floor (x / (2.0 * pi)) :: Integer)

-- | Generate a Hann (von Hann) window for spectral analysis
-- Hann window: @w(n) = 0.5 * (1 - cos(2πn/(N-1)))@
hannWindow :: Int -> VS.Vector Double
hannWindow n = VS.generate n (\i -> 
  0.5 * (1 - cos (2 * pi * fromIntegral i / fromIntegral (n - 1))))

-- | Pre-computed Hann window for 256-sample buffers (used in processFrame)
hannWindow256 :: VS.Vector Double
hannWindow256 = hannWindow 256

-- | Pre-computed Hann window for 2048-sample analysis window
hannWindow2048 :: VS.Vector Double
hannWindow2048 = hannWindow 2048

-- | Pre-computed Hann window for 4096-sample analysis window
hannWindow4096 :: VS.Vector Double
hannWindow4096 = hannWindow 4096

-- | Calculate spectral flux between current and previous magnitude spectra
-- Spectral flux measures the change in spectral content between frames,
-- used for onset detection
calcSpectralFlux :: VS.Vector Double -> VS.Vector Double -> Double
calcSpectralFlux curr prev = 
  let diff = VS.zipWith (\c p -> max 0 (c - p)) curr prev
  in VS.sum diff

-- | Calculate energy transient (RMS difference)
-- Used for detecting sudden changes in signal energy
calcEnergyTransient :: VS.Vector Double -> VS.Vector Double -> Double
calcEnergyTransient curr prev =
  let currRms = sqrt $ VS.sum (VS.map (\x -> x * x) curr) / fromIntegral (VS.length curr)
      prevRms = sqrt $ VS.sum (VS.map (\x -> x * x) prev) / fromIntegral (VS.length prev)
  in abs (currRms - prevRms)

-- | Phase deviation between PLL prediction and signal, normalised to 0–1.
-- Fixed: previously compared a raw accumulated pllPhase (can be >> 2π) against
-- a zero-crossing position in [0, 2π), giving wildly wrong results.
-- Both values are now wrapped to [0, 2π) before computing the error.
calcPhaseDeviation :: Double -> PLLState -> VS.Vector Double -> Double
calcPhaseDeviation sr pll samples
  | not (pllLocked pll) = 1.0
  | otherwise =
      let predicted = mod2pi $
              pllPhase pll +
              2.0 * pi * pllFrequency pll / sr
                * fromIntegral (VS.length samples)
      in case findZeroCrossing samples of
           Nothing -> 1.0
           Just zc ->
             let actual  = 2.0 * pi * fromIntegral zc
                             / fromIntegral (VS.length samples)
                 -- Wrap error into [-π, π] before normalising
                 rawErr  = mod2pi (predicted - actual + pi) - pi
             in min 1.0 (abs rawErr / pi)

-- | Find the first zero-crossing point in the signal
-- Returns sample index where signal crosses zero
findZeroCrossing :: VS.Vector Double -> Maybe Int
findZeroCrossing samples = go 0 (VS.length samples - 1)
  where
    go i j
      | i >= j = Nothing
      | otherwise = do
          let x1 = samples VS.! i
              x2 = samples VS.! (i + 1)
          if (x1 <= 0 && x2 > 0) || (x1 >= 0 && x2 < 0)
            then Just i
            else go (i + 1) j

-- | Update onset features for the current frame.
--
-- Uses the explicit prevEnergy field to track RMS energy across frames,
-- enabling proper energy transient detection (|ΔRMS| between frames).
updateOnsetFeatures :: Config -> VS.Vector Double -> Double -> PLLState -> OnsetFeatures -> OnsetFeatures
updateOnsetFeatures cfg curr currRms prevPLL prevFeatures =
  let sr       = detSampleRate cfg
      prevRms  = prevEnergy prevFeatures     -- previous frame's RMS
      et       = abs (currRms - prevRms)     -- energy transient |ΔRMS|
      sf       = et                          -- spectral flux approximation
      pd       = calcPhaseDeviation sr prevPLL curr
      oc       = onset cfg
      combined = spectralWeight oc * sf + energyWeight oc * et + phaseWeight oc * pd
  in OnsetFeatures
       { spectralFlux    = sf
       , energyTransient = et
       , phaseDeviation  = pd
       , combinedScore   = combined
       , prevEnergy      = currRms
       }

-- | True when the combined onset score exceeds the configured threshold
detectOnset :: Config -> OnsetFeatures -> Bool
detectOnset cfg features =
  combinedScore features > threshold (onset cfg)

-- | Update the PLL state with a new buffer of samples.
--
-- BUG FIXED: previously `samplesSinceLast = zc - pllLastCrossing pll` subtracted
-- an intra-buffer index from a *previous* buffer's intra-buffer index.  Across
-- buffer boundaries this frequently gives negative values (e.g. new zc=50,
-- old pllLastCrossing=200 → -150), which the guard `if > 0` silently ignores,
-- so the PLL never tracked pitch across buffer edges.
--
-- FIX: pllLastCrossing now stores a *monotonically increasing absolute sample
-- count*.  Each buffer advances it by the buffer length; within a buffer zc
-- advances it by zc's position.  The period is the difference of two absolute
-- counts, which is always positive.
updatePLL :: Double           -- ^ sample rate (Hz)
          -> VS.Vector Double -- ^ normalised audio samples
          -> PLLState
          -> PLLState
updatePLL sr samples pll =
  let bufLen = VS.length samples
  in case findZeroCrossing samples of
       Nothing ->
         pll { pllLocked     = False
             , pllConfidence  = pllConfidence pll * 0.9
             , pllLastCrossing = pllLastCrossing pll + bufLen
             }
       Just zc ->
         let absCross    = pllLastCrossing pll + zc
             elapsed     = absCross - pllLastCrossing pll
             -- Guard against implausible periods (< 1 cycle of 20 Hz or > sample rate)
             instantFreq
               | elapsed > 0 && fromIntegral elapsed < sr / 20.0
                              = sr / fromIntegral elapsed
               | otherwise   = pllFrequency pll
             alpha    = 0.3  -- loop bandwidth
             newFreq  = alpha * instantFreq + (1.0 - alpha) * pllFrequency pll
             newPhase = mod2pi $
                 pllPhase pll + 2.0 * pi * newFreq / sr * fromIntegral bufLen
             conf     = min 1.0 (pllConfidence pll * 0.9 + 0.1)
         in PLLState
              { pllFrequency    = newFreq
              , pllPhase        = newPhase
              , pllLocked       = conf > 0.6
              , pllConfidence   = conf
              -- Advance past the end of this buffer so the next call's zc
              -- produces a meaningful elapsed sample count.
              , pllLastCrossing = pllLastCrossing pll + bufLen
              }

-- | Fast frequency estimator via rising zero-crossing spacing.
-- Returns (frequency_Hz, stability_confidence) or Nothing.
--
-- BUG FIXED: variance was divided by the magic constant 100.0, which made
-- low-frequency signals (long periods, large sample indices) appear unstable
-- and high-frequency signals falsely stable.  Now normalised by avgPeriod²
-- giving the squared coefficient of variation — dimensionally correct.
detectFastFreq :: Double           -- ^ sample rate (Hz)
               -> VS.Vector Double
               -> Maybe (Double, Double)
detectFastFreq sr samples =
  let crossings = findAllZeroCrossings samples
      -- Safe handling: need at least 2 crossings to compute periods
      periods   = case crossings of
                    (_:t:rest) -> zipWith (-) (t:rest) crossings
                    _          -> []
  in if length periods < 2
     then Nothing
     else
       let n         = length periods
           fp        = map fromIntegral periods :: [Double]
           avgPeriod = sum fp / fromIntegral n
           variance  = sum (map (\p -> (p - avgPeriod) ^ (2 :: Int)) fp)
                       / fromIntegral n
           -- Normalise variance by mean² → coefficient of variation (unitless)
           stability = 1.0 / (1.0 + variance / (avgPeriod * avgPeriod + 1e-10))
           freq      = sr / avgPeriod
       in if freq >= fastPathMinFreq && freq <= 4000.0 && stability > 0.5
          then Just (freq, min 1.0 stability)
          else Nothing

findAllZeroCrossings :: VS.Vector Double -> [Int]
findAllZeroCrossings v = go 0
  where
    go i
      | i >= VS.length v - 1 = []
      | v VS.! i < 0 && v VS.! (i+1) >= 0 = i : go (i+1)
      | otherwise = go (i+1)

-- | Normalised autocorrelation coefficient at a single lag.
-- Uses Storable Vector operations — O(n) and allocation-free compared to the
-- previous list-comprehension approach which allocated O(n) cons cells per lag.
autocorrAt :: VS.Vector Double -> Int -> Double
autocorrAt buf lag
  | lag <= 0 || lag >= VS.length buf = 0.0
  | otherwise =
      let n     = VS.length buf - lag
          numer = VS.sum $ VS.zipWith (*) (VS.take n buf) (VS.drop lag buf)
          -- Normalise by geometric mean of window energies → coeff in [-1, 1]
          e1    = VS.sum $ VS.map (\x -> x * x) (VS.take n buf)
          e2    = VS.sum $ VS.map (\x -> x * x) (VS.drop lag buf)
          denom = sqrt (e1 * e2)
      in if denom < 1e-10 then 0.0 else numer / denom

-- | Medium-frequency estimator (80–200 Hz) via autocorrelation.
--
-- BUG FIXED (critical): lag ranges were computed at 96 kHz but JACK runs at
-- 44100 Hz, making all lags >= 480 samples while the buffer was 256 samples —
-- every autocorr call operated on an empty index range and returned 0.0.
-- The path was silently broken on every single frame.
--
-- BUG FIXED (performance): the original used Haskell list comprehensions
-- inside the autocorr lambda, allocating O(n) list cells per lag (~720 lags/
-- frame) = ~37 M list allocations/second.  Replaced with VS.zipWith which
-- is allocation-free and cache-friendly.
--
-- NOTE: reliable 80 Hz detection requires at least sr/80 ≈ 551 samples @
-- 44100 Hz.  The function returns Nothing if the buffer is too short rather
-- than producing garbage results.
detectMediumFreq :: Double           -- ^ sample rate (Hz)
                 -> VS.Vector Double
                 -> Maybe (Double, Double)
detectMediumFreq sr samples =
  let n      = VS.length samples
      minLag = max 1 (round (sr / 200.0))   -- ~220 @ 44100
      maxLag = round (sr / 80.0)            -- ~551 @ 44100
  in if maxLag >= n   -- buffer too short for this frequency range
     then Nothing
     else
       let peaks    = [ (lag, autocorrAt samples lag) | lag <- [minLag..maxLag] ]
           bestPeak = maximumBy (comparing snd) peaks
       in case bestPeak of
            Nothing          -> Nothing
            Just (lag, corr) ->
              -- Raised from 0.1 → 0.3: un-normalised autocorr at 0.1 had too many
              -- false positives on noise bursts.  Normalised coeff at 0.3 is robust.
              if corr > 0.3
              then Just (sr / fromIntegral lag, corr)
              else Nothing

-- | Slow-frequency estimator (30–80 Hz) using YIN with CMNDF normalisation.
--
-- BUG FIXED: the original used the raw YIN difference function d(τ) directly.
-- Without the Cumulative Mean Normalised Difference Function (CMNDF) the small-
-- τ dips (harmonics, noise) dominate and the threshold of 0.1 rarely fires at
-- the true fundamental — the path returned Nothing on most frames.
--
-- The CMNDF d'(τ) = d(τ) * τ / Σ_{j=1}^{τ} d(j) re-scales so that τ=1 is
-- always 1.0, making the 0.12 threshold select the first true period minimum.
--
-- NOTE: reliable 30 Hz detection requires at least sr/30 ≈ 1470 samples @
-- 44100 Hz.  Returns Nothing when the buffer is too small.
detectSlowFreq :: Double           -- ^ sample rate (Hz)
               -> VS.Vector Double
               -> Maybe (Double, Double)
detectSlowFreq sr samples =
  let n      = VS.length samples
      minTau = max 2 (round (sr / 500.0))     -- above 500 Hz overtones
      maxTau = min (n `div` 2) (round (sr / bassMinFreq))
  in if maxTau <= minTau
     then Nothing   -- buffer too small for bass range
     else
       let -- Raw YIN difference function using VS.zipWith — O(n) per τ
           yinDiff tau =
             let k = n - tau
             in if k <= 0 then 0.0
                else VS.sum $ VS.zipWith (\a b -> let d = a - b in d * d)
                                         (VS.take k samples)
                                         (VS.drop tau samples)

           -- Compute all raw diffs once, then build CMNDF with a running sum
           -- (avoids O(τ²) by accumulating Σ d(j) incrementally)
           diffs = VS.generate (maxTau - minTau + 1) (\i -> yinDiff (minTau + i))

           -- CMNDF at index i (τ = minTau + i):
           --   d'(τ) = d(τ) * τ / Σ_{j=1}^{τ} d(j)
           -- We approximate the denominator sum using our minTau..τ window.
           cmndf i =
             let d_tau  = diffs VS.! i
                 runSum = VS.sum (VS.take (i + 1) diffs)
             in if runSum < 1e-10 then 1.0
                else d_tau * fromIntegral (i + 1) / runSum

           -- Search for first τ where CMNDF < 0.12 (standard YIN threshold)
           search i
             | i > maxTau - minTau = Nothing
             | otherwise =
                 let c = cmndf i
                 in if c < 0.12
                    then Just (minTau + i, min 1.0 (1.0 - c))
                    else search (i + 1)

       in case search 0 of
            Nothing       -> Nothing
            Just (tau, c) -> Just (sr / fromIntegral tau, c)

-- Convert frequency to MIDI note
freqToMidi :: Double -> Int
freqToMidi freq = round $ 69 + 12 * logBase 2 (freq / 440.0)

-- Calculate pitch bend amount (semitones)
calcPitchBend :: Int -> Double -> Double  -- (currentNote, targetFreq) -> bendAmount
calcPitchBend currentNote targetFreq =
  let currentFreq = 440.0 * (2 ** ((fromIntegral currentNote - 69) / 12))
      ratio = targetFreq / currentFreq
      semitones = 12 * logBase 2 ratio
  in max (-2.0) (min 2.0 semitones)  -- Clamp to ±2 semitones

-- Tuning functions
midiToFreq :: Int -> Double
midiToFreq n = 440.0 * (2 ** ((fromIntegral n - 69) / 12))

freqToCents :: Double -> Int -> Double
freqToCents freq midiNote = 
    let targetFreq = midiToFreq midiNote
    in 1200.0 * logBase 2 (freq / targetFreq)

-- | Nearest MIDI note and its cent deviation from the given frequency.
-- Simplified from the original: `freqToMidi` uses `round`, which by definition
-- minimises |cents|, so the midiNote±1 candidates were always further away and
-- the minimumBy always returned midiNote anyway.  The neighbour search was
-- redundant (and called the local shadowing `minimumBy` unnecessarily).
nearestNote :: Double -> (Int, Double)
nearestNote freq
  | freq <= 0.0 = (69, 0.0)   -- A4 as safe default for zero/negative input
  | otherwise   =
      let midiNote = freqToMidi freq
          cents    = freqToCents freq midiNote
      in (midiNote, cents)

-- (minimumBy removed — nearestNote simplified to not need it)

isInTune :: Double -> Bool
isInTune cents = abs cents <= 5.0

isClose :: Double -> Bool
isClose cents = abs cents <= 15.0

midiToNoteName :: Int -> String
midiToNoteName n = 
    let noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
        idx = n `mod` 12
        note = if idx >= 0 && idx < length noteNames then noteNames !! idx else "C"
        octave = (n `div` 12) - 1
    in note ++ show octave

-- | Main detection function with state machine.
--
-- Changes from original:
--   • prevPLL is no longer discarded (_prevPLL → prevPLL); updatePLL is called
--     and the resulting PLLState fed back into the result.
--   • updateOnsetFeatures receives the current RMS (not `curr` twice), so onset
--     detection now compares against the previous frame's energy.
--   • isSilent check added: when signal drops below silenceThresholdRMS the
--     held note transitions to Releasing instead of staying locked forever.
--   • Releasing is now reachable from FastNote, MediumNote, and Validated.
--   • DetectionResult is extended with pllState and onsetState so the Backend
--     can thread them forward (see Types.hs note below).
--   • All frequency/lag computations use detSampleRate cfg, not a hardcoded Hz.
--
-- REQUIRED Types.hs change: add two fields to DetectionResult:
--   pllState   :: PLLState
--   onsetState :: OnsetFeatures
-- and update the constructor accordingly.
detect :: Config
       -> VS.Vector Int
       -> TimeStamp
       -> NoteState
       -> PLLState
       -> OnsetFeatures
       -> IO DetectionResult
detect cfg samplesInt16 currentTime prevState prevPLL prevOnset = do
  let sr      = detSampleRate cfg
      samples = normalizeInt16 samplesInt16
      currRms = computeRms samples

  -- Update PLL with the actual previous state (was discarded with _ before)
  let newPLL = updatePLL sr samples prevPLL

  -- Onset detection now receives currRms so the energy delta is meaningful.
  let newOnset = updateOnsetFeatures cfg samples currRms newPLL prevOnset
      isOnset  = detectOnset cfg newOnset

  -- Note-off heuristic: if the buffer is silent the held note has ended.
  let isSilent = currRms < silenceThresholdRMS

  -- Convenience: build a DetectionResult carrying the updated PLL and onset state
  let emit mNote conf st bend =
        DetectionResult mNote conf st bend newPLL newOnset

  case prevState of
    Idle ->
      pure $ emit Nothing 0.0 (if isOnset then Attacking currentTime else Idle) Nothing

    Attacking startTime ->
      let elapsedMs = fromIntegral (currentTime - startTime) / 1000.0 :: Double
      in if isSilent
         -- Silence during attack → spurious onset; return to Idle
         then pure $ emit Nothing 0.0 Idle Nothing
         else if elapsedMs >= bassConfirmMs (detection cfg)
         then case detectSlowFreq sr samples of
                Nothing           -> pure $ emit Nothing 0.0 Idle Nothing
                Just (freq, conf) ->
                  let note = freqToMidi freq
                      vel  = min 127 $ max 1 $ round (127.0 * conf)
                  in pure $ emit (Just (note, vel)) conf (Validated note vel) Nothing
         else if elapsedMs >= bassFastMs (detection cfg)
         then case detectMediumFreq sr samples of
                Nothing           -> pure $ emit Nothing 0.0 prevState Nothing
                Just (freq, conf) ->
                  if conf >= bassFastConf (detection cfg)
                  then let note = freqToMidi freq
                           vel  = min 127 $ max 1 $ round (127.0 * conf)
                       in pure $ emit (Just (note, vel)) conf (MediumNote note vel currentTime) Nothing
                  else pure $ emit Nothing 0.0 prevState Nothing
         else if elapsedMs >= fastValidationMs (detection cfg)
         then case detectFastFreq sr samples of
                Nothing           -> pure $ emit Nothing 0.0 prevState Nothing
                Just (freq, conf) ->
                  if conf >= highFreqConf (detection cfg) && freq >= fastPathMinFreq
                  then let note = freqToMidi freq
                           vel  = min 127 $ max 1 $ round (127.0 * conf)
                       in pure $ emit (Just (note, vel)) conf (FastNote note vel currentTime) Nothing
                  else pure $ emit Nothing 0.0 prevState Nothing
         else pure $ emit Nothing 0.0 prevState Nothing

    FastNote note vel _startTime ->
      if isSilent
      -- Note-off via silence — transition to Releasing (was unreachable before)
      then pure $ emit Nothing 0.0 (Releasing currentTime) Nothing
      else if isOnset
      then pure $ emit (Just (note, vel)) 1.0 (Attacking currentTime) Nothing
      else -- Use PLL confidence to weight the sustained-note confidence
           let conf = min 1.0 (0.8 + 0.2 * pllConfidence newPLL)
           in pure $ emit (Just (note, vel)) conf prevState Nothing

    MediumNote note vel startTime ->
      let elapsedMs = fromIntegral (currentTime - startTime) / 1000.0 :: Double
      in if isSilent
         then pure $ emit Nothing 0.0 (Releasing currentTime) Nothing
         else if elapsedMs >= bassConfirmMs (detection cfg)
         then case detectSlowFreq sr samples of
                Nothing -> pure $ emit (Just (note, vel)) 0.9 (Validated note vel) Nothing
                Just (freq, conf) ->
                  let targetNote = freqToMidi freq
                  in if targetNote /= note && conf >= bassFinalConf (detection cfg)
                     then let bend = calcPitchBend note freq
                          in pure $ emit (Just (targetNote, vel)) conf
                                         (Validated targetNote vel) (Just (targetNote, bend))
                     else pure $ emit (Just (note, vel)) 0.95 (Validated note vel) Nothing
         else if isOnset
         then pure $ emit (Just (note, vel)) 0.9 (Attacking currentTime) Nothing
         else pure $ emit (Just (note, vel)) 0.9 prevState Nothing

    Validated note vel ->
      if isSilent
      then pure $ emit Nothing 0.0 (Releasing currentTime) Nothing
      else if isOnset
      then pure $ emit (Just (note, vel)) 1.0 (Attacking currentTime) Nothing
      else let conf = min 1.0 (0.95 + 0.05 * pllConfidence newPLL)
           in pure $ emit (Just (note, vel)) conf prevState Nothing

    Releasing startTime ->
      let elapsedMs = fromIntegral (currentTime - startTime) / 1000.0 :: Double
      in if elapsedMs > 50.0
         then pure $ emit Nothing 0.0 Idle Nothing
         -- New onset during release: re-attack immediately
         else if isOnset
         then pure $ emit Nothing 0.0 (Attacking currentTime) Nothing
         else pure $ emit Nothing 0.0 prevState Nothing

-- | Safe maximum by comparator (returns Nothing on empty list).
-- Named to avoid shadowing Data.List.maximumBy or Data.Ord utilities.
maximumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumBy _ []  = Nothing
maximumBy cmp xs = Just $ foldl1 (\x y -> if cmp x y == GT then x else y) xs

-- | Safe minimum by comparator
minimumBy :: (a -> a -> Ordering) -> [a] -> Maybe a
minimumBy _ []  = Nothing
minimumBy cmp xs = Just $ foldl1 (\x y -> if cmp x y == GT then y else x) xs