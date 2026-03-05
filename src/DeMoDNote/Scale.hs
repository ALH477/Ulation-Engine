-- |
-- Module      : DeMoDNote.Scale
-- Description : Musical scale definitions and utilities
-- Copyright   : 2026
-- License     : MIT
--
-- This module provides musical scale definitions (major, minor, pentatonic, etc.)
-- and utilities for note name conversion and scale manipulation.

module DeMoDNote.Scale (
    ScaleType(..),
    Scale(..),
    NoteName(..),
    getScaleNotes,
    midiToNoteName,
    noteNameToMidi,
    getScaleByName,
    allScaleNames,
    transposeScale,
    scaleInterval,
    makeScale,
    -- Common scales
    majorScale,
    minorScale,
    pentatonicMajor,
    pentatonicMinor,
    bluesScale,
    dorianMode,
    phrygianMode,
    lydianMode,
    mixolydianMode,
    chromaticScale
) where



import DeMoDNote.Util (safeIndex)

-- MIDI note numbers: 0-127
-- C4 = 60

data NoteName = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
    deriving (Eq, Show, Enum, Ord)

-- Scale intervals in semitones from root
type ScaleIntervals = [Int]

data ScaleType = 
    Major |
    MinorNatural |
    MinorHarmonic |
    MinorMelodic |
    PentatonicMajor |
    PentatonicMinor |
    Blues |
    Chromatic |
    -- Modes
    Dorian |
    Phrygian |
    Lydian |
    Mixolydian |
    Aeolian |
    Locrian |
    -- Jazz/Other
    WholeTone |
    Diminished |
    Augmented |
    Custom String ScaleIntervals
    deriving (Eq, Show)

data Scale = Scale {
    scaleName :: String,
    scaleType :: ScaleType,
    scaleRoot :: NoteName,
    scaleIntervals :: ScaleIntervals
} deriving (Eq, Show)

-- Note names to MIDI note numbers (C4 = 60)
noteNameToMidi :: NoteName -> Int -> Int
noteNameToMidi note octave = 
    let noteVal = fromEnum note
    in (octave + 1) * 12 + noteVal

-- MIDI note number to note name
midiToNoteName :: Int -> (NoteName, Int)
midiToNoteName midi = 
    let noteIdx = midi `mod` 12
        octave = (midi `div` 12) - 1
    in (toEnum noteIdx, octave)

-- Get note name as string
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

-- Standard scale intervals
majorIntervals :: ScaleIntervals
majorIntervals = [0, 2, 4, 5, 7, 9, 11]

minorNaturalIntervals :: ScaleIntervals
minorNaturalIntervals = [0, 2, 3, 5, 7, 8, 10]

minorHarmonicIntervals :: ScaleIntervals
minorHarmonicIntervals = [0, 2, 3, 5, 7, 8, 11]

minorMelodicIntervals :: ScaleIntervals
minorMelodicIntervals = [0, 2, 3, 5, 7, 9, 11]

pentatonicMajorIntervals :: ScaleIntervals
pentatonicMajorIntervals = [0, 2, 4, 7, 9]

pentatonicMinorIntervals :: ScaleIntervals
pentatonicMinorIntervals = [0, 3, 5, 7, 10]

bluesIntervals :: ScaleIntervals
bluesIntervals = [0, 3, 5, 6, 7, 10]

chromaticIntervals :: ScaleIntervals
chromaticIntervals = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]

-- Modal intervals
dorianIntervals :: ScaleIntervals
dorianIntervals = [0, 2, 3, 5, 7, 9, 10]

phrygianIntervals :: ScaleIntervals
phrygianIntervals = [0, 1, 3, 5, 7, 8, 10]

lydianIntervals :: ScaleIntervals
lydianIntervals = [0, 2, 4, 6, 7, 9, 11]

mixolydianIntervals :: ScaleIntervals
mixolydianIntervals = [0, 2, 4, 5, 7, 9, 10]

aeolianIntervals :: ScaleIntervals
aeolianIntervals = [0, 2, 3, 5, 7, 8, 10]

locrianIntervals :: ScaleIntervals
locrianIntervals = [0, 1, 3, 5, 6, 8, 10]

wholeToneIntervals :: ScaleIntervals
wholeToneIntervals = [0, 2, 4, 6, 8, 10]

diminishedIntervals :: ScaleIntervals
diminishedIntervals = [0, 2, 3, 5, 6, 8, 9, 11]

augmentedIntervals :: ScaleIntervals
augmentedIntervals = [0, 3, 4, 7, 8, 11]

-- Get intervals for a scale type
getIntervals :: ScaleType -> ScaleIntervals
getIntervals Major = majorIntervals
getIntervals MinorNatural = minorNaturalIntervals
getIntervals MinorHarmonic = minorHarmonicIntervals
getIntervals MinorMelodic = minorMelodicIntervals
getIntervals PentatonicMajor = pentatonicMajorIntervals
getIntervals PentatonicMinor = pentatonicMinorIntervals
getIntervals Blues = bluesIntervals
getIntervals Chromatic = chromaticIntervals
getIntervals Dorian = dorianIntervals
getIntervals Phrygian = phrygianIntervals
getIntervals Lydian = lydianIntervals
getIntervals Mixolydian = mixolydianIntervals
getIntervals Aeolian = aeolianIntervals
getIntervals Locrian = locrianIntervals
getIntervals WholeTone = wholeToneIntervals
getIntervals Diminished = diminishedIntervals
getIntervals Augmented = augmentedIntervals
getIntervals (Custom _ intervals) = intervals

-- Get scale name
getScaleTypeName :: ScaleType -> String
getScaleTypeName Major = "Major"
getScaleTypeName MinorNatural = "Natural Minor"
getScaleTypeName MinorHarmonic = "Harmonic Minor"
getScaleTypeName MinorMelodic = "Melodic Minor"
getScaleTypeName PentatonicMajor = "Major Pentatonic"
getScaleTypeName PentatonicMinor = "Minor Pentatonic"
getScaleTypeName Blues = "Blues"
getScaleTypeName Chromatic = "Chromatic"
getScaleTypeName Dorian = "Dorian"
getScaleTypeName Phrygian = "Phrygian"
getScaleTypeName Lydian = "Lydian"
getScaleTypeName Mixolydian = "Mixolydian"
getScaleTypeName Aeolian = "Aeolian"
getScaleTypeName Locrian = "Locrian"
getScaleTypeName WholeTone = "Whole Tone"
getScaleTypeName Diminished = "Diminished"
getScaleTypeName Augmented = "Augmented"
getScaleTypeName (Custom name _) = name

-- Create a scale
makeScale :: NoteName -> ScaleType -> Scale
makeScale root sType = Scale {
    scaleName = showNote root ++ " " ++ getScaleTypeName sType,
    scaleType = sType,
    scaleRoot = root,
    scaleIntervals = getIntervals sType
}

-- Get all notes in a scale within a specific octave range
getScaleNotes :: Scale -> Int -> Int -> [Int]
getScaleNotes scale lowOctave highOctave = 
    let root = scaleRoot scale
        intervals = scaleIntervals scale
        allNotes = [noteNameToMidi root octave + interval 
                   | octave <- [lowOctave..highOctave], 
                     interval <- intervals]
    in filter (<= 127) allNotes

-- Transpose a scale by N semitones
transposeScale :: Scale -> Int -> Scale
transposeScale scale semitones =
    let newRootIdx = (fromEnum (scaleRoot scale) + semitones) `mod` 12
        newRoot = toEnum newRootIdx
    in makeScale newRoot (scaleType scale)

-- Get interval from root for a specific scale degree (1-indexed)
scaleInterval :: Scale -> Int -> Int
scaleInterval scale degree = 
    let intervals = scaleIntervals scale
        idx = (degree - 1) `mod` max 1 (length intervals)
    in safeIndex intervals idx 0

-- Common scale constructors
majorScale :: NoteName -> Scale
majorScale = flip makeScale Major

minorScale :: NoteName -> Scale
minorScale = flip makeScale MinorNatural

pentatonicMajor :: NoteName -> Scale
pentatonicMajor = flip makeScale PentatonicMajor

pentatonicMinor :: NoteName -> Scale
pentatonicMinor = flip makeScale PentatonicMinor

bluesScale :: NoteName -> Scale
bluesScale = flip makeScale Blues

dorianMode :: NoteName -> Scale
dorianMode = flip makeScale Dorian

phrygianMode :: NoteName -> Scale
phrygianMode = flip makeScale Phrygian

lydianMode :: NoteName -> Scale
lydianMode = flip makeScale Lydian

mixolydianMode :: NoteName -> Scale
mixolydianMode = flip makeScale Mixolydian

chromaticScale :: NoteName -> Scale
chromaticScale = flip makeScale Chromatic

-- Scale name parsing
parseScaleName :: String -> Maybe (NoteName, ScaleType)
parseScaleName name = 
    let normalized = filter (/= ' ') $ map toLower name
    in case normalized of
        'c':rest -> parseWithRoot C rest
        'd':'#':rest -> parseWithRoot Ds rest
        'd':rest -> parseWithRoot D rest
        'e':rest -> parseWithRoot E rest
        'f':'#':rest -> parseWithRoot Fs rest
        'f':rest -> parseWithRoot F rest
        'g':'#':rest -> parseWithRoot Gs rest
        'g':rest -> parseWithRoot G rest
        'a':'#':rest -> parseWithRoot As rest
        'a':rest -> parseWithRoot A rest
        'b':rest -> parseWithRoot B rest
        _ -> Nothing
    where
        parseWithRoot root rest
            | rest == "major" = Just (root, Major)
            | rest == "minor" = Just (root, MinorNatural)
            | rest == "minornatural" = Just (root, MinorNatural)
            | rest == "minorharmonic" = Just (root, MinorHarmonic)
            | rest == "minormelodic" = Just (root, MinorMelodic)
            | rest == "pentatonicmajor" = Just (root, PentatonicMajor)
            | rest == "pentatonicminor" = Just (root, PentatonicMinor)
            | rest == "blues" = Just (root, Blues)
            | rest == "chromatic" = Just (root, Chromatic)
            | rest == "dorian" = Just (root, Dorian)
            | rest == "phrygian" = Just (root, Phrygian)
            | rest == "lydian" = Just (root, Lydian)
            | rest == "mixolydian" = Just (root, Mixolydian)
            | rest == "aeolian" = Just (root, Aeolian)
            | rest == "locrian" = Just (root, Locrian)
            | otherwise = Just (root, Major)  -- Default to major

toLower :: Char -> Char
toLower c 
    | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
    | otherwise = c

-- Get scale by name (e.g., "C Major", "A Minor", "D Dorian")
getScaleByName :: String -> Maybe Scale
getScaleByName name = do
    (root, sType) <- parseScaleName name
    return $ makeScale root sType

-- List of all built-in scale names
allScaleNames :: [String]
allScaleNames = 
    let roots = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
        types = [Major, MinorNatural, PentatonicMajor, PentatonicMinor, Blues, 
                 Dorian, Phrygian, Lydian, Mixolydian, Chromatic]
    in [showNote r ++ " " ++ getScaleTypeName t | r <- roots, t <- types]
