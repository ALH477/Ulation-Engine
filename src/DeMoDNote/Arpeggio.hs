{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : DeMoDNote.Arpeggio
-- Description : Arpeggio patterns and chord utilities
-- Copyright   : 2026
-- License     : MIT
--
-- This module provides arpeggio pattern definitions (up, down, broken, etc.)
-- and chord quality utilities for generating note sequences.

module DeMoDNote.Arpeggio (
    ArpeggioPattern(..),
    Arpeggio(..),
    ChordQuality(..),
    createArpeggio,
    getArpeggioNotes,
    getArpeggioWithRhythm,
    applyPattern,
    -- Pattern generators
    upPattern,
    downPattern,
    upDownPattern,
    downUpPattern,
    randomPattern,
    brokenChordPattern,
    strumPattern,
    walkingBassPattern,
    fingerpickingPattern,
    -- Chord constructors
    majorChord,
    minorChord,
    diminishedChord,
    augmentedChord,
    dominant7Chord,
    major7Chord,
    minor7Chord,
    suspended4Chord,
    -- Utility
    chordToNotes,
    arpeggiateChord,
    allArpeggioPatterns,
    -- IO helpers
    applyPatternIO,
    shuffleIO
) where

import DeMoDNote.Scale (NoteName(..), noteNameToMidi, Scale(..), ScaleType(..))
import DeMoDNote.Util (safeInit, safeTail)
import System.Random (randomRIO)

-- Safe list indexing - returns Nothing if index out of bounds
-- (different signature from DeMoDNote.Util.safeIndex, which requires a default)
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n = safeIndex xs (n - 1)

-- Safe list indexing with default value
safeIndexDef :: a -> [a] -> Int -> a
safeIndexDef def xs n = case safeIndex xs n of
    Just x  -> x
    Nothing -> def

-- Chord qualities
data ChordQuality = 
    MajorTriad |
    MinorTriad |
    DiminishedTriad |
    AugmentedTriad |
    Dominant7 |
    Major7 |
    Minor7 |
    HalfDiminished7 |
    Diminished7 |
    Suspended4 |
    CustomChord [Int]
    deriving (Eq, Show)

-- Arpeggio patterns
data ArpeggioPattern =
    Up |
    Down |
    UpDown |
    DownUp |
    Random |
    Broken3 |
    Broken4 |
    StrumUp |
    StrumDown |
    WalkingBass |
    Fingerpicking |
    Euclidean Int Int |  -- pulses, steps
    CustomPattern String [[Int]]  -- Name and patterns (relative indices)
    deriving (Eq, Show)

data Arpeggio = Arpeggio {
    arpName :: String,
    arpChord :: (NoteName, ChordQuality),
    arpPattern :: ArpeggioPattern,
    arpOctaves :: Int,  -- How many octaves to span
    arpSwing :: Double  -- 0.0 = straight, 1.0 = triplet feel
} deriving (Eq, Show)

-- Chord intervals (in semitones from root)
getChordIntervals :: ChordQuality -> [Int]
getChordIntervals MajorTriad = [0, 4, 7]
getChordIntervals MinorTriad = [0, 3, 7]
getChordIntervals DiminishedTriad = [0, 3, 6]
getChordIntervals AugmentedTriad = [0, 4, 8]
getChordIntervals Dominant7 = [0, 4, 7, 10]
getChordIntervals Major7 = [0, 4, 7, 11]
getChordIntervals Minor7 = [0, 3, 7, 10]
getChordIntervals HalfDiminished7 = [0, 3, 6, 10]
getChordIntervals Diminished7 = [0, 3, 6, 9]
getChordIntervals Suspended4 = [0, 5, 7]
getChordIntervals (CustomChord intervals) = intervals

-- Chord name
getChordName :: ChordQuality -> String
getChordName MajorTriad = ""
getChordName MinorTriad = "m"
getChordName DiminishedTriad = "dim"
getChordName AugmentedTriad = "aug"
getChordName Dominant7 = "7"
getChordName Major7 = "maj7"
getChordName Minor7 = "m7"
getChordName HalfDiminished7 = "m7b5"
getChordName Diminished7 = "dim7"
getChordName Suspended4 = "sus4"
getChordName (CustomChord _) = "custom"

-- Show note name (reusing from Scale)
showNote :: NoteName -> String
showNote C = "C"
showNote Cs = "C#"
showNote D = "D"
showNote Ds = "D#"
showNote E = "E"
showNote F = "F"
showNote Fs = "F#"
showNote G = "G"
showNote Gs = "G#"
showNote A = "A"
showNote As = "A#"
showNote B = "B"

-- Create an arpeggio
createArpeggio :: NoteName -> ChordQuality -> ArpeggioPattern -> Arpeggio
createArpeggio root quality pattern = Arpeggio {
    arpName = showNote root ++ getChordName quality ++ " " ++ showPattern pattern,
    arpChord = (root, quality),
    arpPattern = pattern,
    arpOctaves = 2,  -- Default 2 octaves
    arpSwing = 0.0
}

showPattern :: ArpeggioPattern -> String
showPattern Up = "Up"
showPattern Down = "Down"
showPattern UpDown = "Up-Down"
showPattern DownUp = "Down-Up"
showPattern Random = "Random"
showPattern Broken3 = "Broken-3"
showPattern Broken4 = "Broken-4"
showPattern StrumUp = "Strum-Up"
showPattern StrumDown = "Strum-Down"
showPattern WalkingBass = "Walking-Bass"
showPattern Fingerpicking = "Fingerpicking"
showPattern (Euclidean p s) = "Euclidean-" ++ show p ++ "-" ++ show s
showPattern (CustomPattern name _) = name

-- Convert chord to MIDI notes
chordToNotes :: NoteName -> ChordQuality -> Int -> [Int]
chordToNotes root quality octave = 
    let intervals = getChordIntervals quality
        base = noteNameToMidi root octave
    in map (+ base) intervals

-- Generate arpeggio pattern
getArpeggioNotes :: Arpeggio -> Int -> [Int]
getArpeggioNotes arp startOctave = 
    let (root, quality) = arpChord arp
        intervals = getChordIntervals quality
        base = noteNameToMidi root startOctave
        octaves = arpOctaves arp
        allNotes = concatMap (\o -> map (+ (base + o * 12)) intervals) [0..octaves-1]
    in applyPattern (arpPattern arp) allNotes

-- Apply pattern to note list (pure patterns only; Random requires IO — see applyPatternIO)
applyPattern :: ArpeggioPattern -> [Int] -> [Int]
applyPattern Up notes = notes
applyPattern Down notes = reverse notes
applyPattern UpDown notes = notes ++ reverse (safeInit notes)
applyPattern DownUp notes = reverse notes ++ safeTail notes
applyPattern Random notes = notes  -- order preserved; use applyPatternIO for shuffled output
applyPattern Broken3 notes = 
    if null notes then []
    else take 12 $ concatMap (\i -> map (safeIndexDef 60 notes) [(i `mod` len), ((i+2) `mod` len), ((i+4) `mod` len)]) [0..]
    where len = length notes
applyPattern Broken4 notes = 
    if null notes then []
    else take 16 $ concatMap (\i -> map (safeIndexDef 60 notes) [(i `mod` len), ((i+2) `mod` len), ((i+4) `mod` len), ((i+6) `mod` len)]) [0..]
    where len = length notes
applyPattern StrumUp notes = notes
applyPattern StrumDown notes = reverse notes
applyPattern WalkingBass notes = 
    if null notes then []
    else concatMap (\root -> [root, safeIndexDef 60 (drop 2 notes) 0, root - 1, root + 1]) (takeEvery 3 notes)
    where takeEvery n xs = case drop (n-1) xs of
            (y:ys) -> y : takeEvery n ys
            [] -> []
applyPattern Fingerpicking notes = 
    let bass = case notes of
                 (b:_) -> b
                 [] -> 60
        others = drop 1 notes
    in concat $ replicate 4 $ [bass] ++ reverse others
applyPattern (Euclidean pulses steps) notes = 
    if null notes || steps <= 0 then []
    else let pattern = euclideanPattern pulses steps
             validIndices = [i | i <- [0..steps-1], maybe False id (safeIndex pattern i)]
         in map (safeIndexDef 60 notes) $ map (`mod` length notes) validIndices
applyPattern (CustomPattern _ indices) notes = 
    if null notes then []
    else concatMap (map (safeIndexDef 60 notes)) indices

-- Generate Euclidean rhythm pattern using the Bjorklund/Bresenham algorithm.
-- euclideanPattern pulses steps produces a [Bool] of length `steps` with
-- `pulses` True values distributed as evenly as possible.
-- e.g. euclideanPattern 3 8 = [T,F,F,T,F,F,T,F]
euclideanPattern :: Int -> Int -> [Bool]
euclideanPattern pulses steps
    | steps <= 0  = []
    | pulses <= 0 = replicate steps False
    | pulses >= steps = replicate steps True
    | otherwise   = bjorklund (replicate pulses [True]) (replicate (steps - pulses) [False])
  where
    bjorklund ones zeros
        | length zeros <= 1 = concat ones ++ concat zeros
        | otherwise =
            let (paired, remainder) = if length ones <= length zeros
                    then ( zipWith (++) ones zeros
                         , drop (length ones) zeros )
                    else ( zipWith (++) (take (length zeros) ones) zeros
                         , drop (length zeros) ones )
            in bjorklund paired remainder

-- Get arpeggio with timing information (in milliseconds)
getArpeggioWithRhythm :: Arpeggio -> Int -> [(Int, Double)]  -- [(Note, TimeOffsetMs)]
getArpeggioWithRhythm arp bpm = 
    let notes = getArpeggioNotes arp bpm
        beatMs = 60000.0 / fromIntegral bpm  -- Quarter note duration
        stepMs = beatMs / 4  -- 16th notes
        swing = arpSwing arp
        times = map (\(i :: Int) -> 
            let base = fromIntegral i * stepMs
                offset = if odd i then swing * stepMs * 0.5 else 0
            in base + offset) [0..]
    in zip notes times

-- Chord constructors
majorChord :: NoteName -> Arpeggio
majorChord root = createArpeggio root MajorTriad Up

minorChord :: NoteName -> Arpeggio
minorChord root = createArpeggio root MinorTriad Up

diminishedChord :: NoteName -> Arpeggio
diminishedChord root = createArpeggio root DiminishedTriad Up

augmentedChord :: NoteName -> Arpeggio
augmentedChord root = createArpeggio root AugmentedTriad Up

dominant7Chord :: NoteName -> Arpeggio
dominant7Chord root = createArpeggio root Dominant7 Up

major7Chord :: NoteName -> Arpeggio
major7Chord root = createArpeggio root Major7 Up

minor7Chord :: NoteName -> Arpeggio
minor7Chord root = createArpeggio root Minor7 Up

suspended4Chord :: NoteName -> Arpeggio
suspended4Chord root = createArpeggio root Suspended4 Up

-- Pattern generators with custom settings
upPattern :: Int -> Arpeggio -> Arpeggio
upPattern octaves arp = arp { arpPattern = Up, arpOctaves = octaves }

downPattern :: Int -> Arpeggio -> Arpeggio
downPattern octaves arp = arp { arpPattern = Down, arpOctaves = octaves }

upDownPattern :: Int -> Arpeggio -> Arpeggio
upDownPattern octaves arp = arp { arpPattern = UpDown, arpOctaves = octaves }

downUpPattern :: Int -> Arpeggio -> Arpeggio
downUpPattern octaves arp = arp { arpPattern = DownUp, arpOctaves = octaves }

randomPattern :: Int -> Arpeggio -> Arpeggio
randomPattern octaves arp = arp { arpPattern = Random, arpOctaves = octaves }

brokenChordPattern :: Int -> Arpeggio -> Arpeggio
brokenChordPattern octaves arp = arp { arpPattern = Broken3, arpOctaves = octaves }

strumPattern :: Bool -> Int -> Arpeggio -> Arpeggio
strumPattern up octaves arp = arp { 
    arpPattern = if up then StrumUp else StrumDown, 
    arpOctaves = octaves 
}

walkingBassPattern :: Arpeggio -> Arpeggio
walkingBassPattern arp = arp { arpPattern = WalkingBass, arpOctaves = 1 }

fingerpickingPattern :: Arpeggio -> Arpeggio
fingerpickingPattern arp = arp { arpPattern = Fingerpicking, arpOctaves = 2 }

-- Arpeggiate from a scale
arpeggiateChord :: Scale -> Int -> ChordQuality -> Arpeggio
arpeggiateChord scale degree quality = 
    let intervals = case scaleType scale of
            Major -> [0, 2, 4, 5, 7, 9, 11]
            MinorNatural -> [0, 2, 3, 5, 7, 8, 10]
            _ -> [0, 2, 4, 5, 7, 9, 11]
        rootIdx = (degree - 1) `mod` 7
        rootInterval = safeIndexDef 0 intervals rootIdx
        root = toEnum ((fromEnum (scaleRoot scale) + rootInterval) `mod` 12)
    in createArpeggio root quality Up

-- All available arpeggio patterns
allArpeggioPatterns :: [String]
allArpeggioPatterns = [
    "up", "down", "up-down", "down-up", "random",
    "broken-3", "broken-4", "strum-up", "strum-down",
    "walking-bass", "fingerpicking"
    ]

-- | Shuffle a list randomly (Fisher-Yates via randomRIO).
shuffleIO :: [a] -> IO [a]
shuffleIO []       = return []
shuffleIO xs@[_]   = return xs
shuffleIO xs = do
    i <- randomRIO (0, length xs - 1)
    let (left, right) = splitAt i xs
    case right of
        []        -> shuffleIO xs  -- shouldn't happen; retry
        (x:right') -> do
            rest <- shuffleIO (left ++ right')
            return (x : rest)

-- | IO variant of applyPattern — handles 'Random' by actually shuffling.
applyPatternIO :: ArpeggioPattern -> [Int] -> IO [Int]
applyPatternIO Random notes = shuffleIO notes
applyPatternIO p     notes  = return (applyPattern p notes)
