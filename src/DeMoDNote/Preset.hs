{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DeMoDNote.Preset (
    Preset(..),
    PresetLibrary,
    BuiltInPreset(..),
    defaultPresets,
    defaultPreset,
    loadPreset,
    savePreset,
    listPresets,
    getPresetByName,
    applyPreset,
    addCustomPreset,
    deletePreset,
    generatePresetTOML,
    -- Built-in presets
    jazzWalkingBass,
    classicalGuitar,
    bluesLead,
    electronic,
    practiceMode,
    -- Parser utilities
    parseScaleType,
    parseNoteName
) where

import GHC.Generics
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort, isPrefixOf, isSuffixOf, find)
import Control.Monad (forM)
import System.Directory (createDirectoryIfMissing, doesFileExist, doesDirectoryExist, getHomeDirectory, removeFile, listDirectory)
import System.FilePath ((</>))
import DeMoDNote.Config
import DeMoDNote.Scale (Scale, ScaleType(..), NoteName(..), makeScale)
import DeMoDNote.Arpeggio (Arpeggio, ArpeggioPattern, createArpeggio, majorChord, minorChord)
import DeMoDNote.BPM (BPMMode(..), QuantizationGrid(..), TimeSignature(..))

-- Built-in preset types
data BuiltInPreset =
    JazzWalkingBass |
    ClassicalGuitar |
    BluesLead |
    Electronic |
    PracticeMode
    deriving (Eq, Show, Enum, Bounded)

-- Preset definition
data Preset = Preset {
    presetName :: String,
    presetDescription :: String,
    -- Detection settings
    presetOnsetThreshold :: Double,
    presetFastValidationMs :: Double,
    presetBassFastMs :: Double,
    presetBassConfirmMs :: Double,
    -- Scale/Arpeggio settings
    presetScale :: Maybe (NoteName, ScaleType),
    presetArpeggio :: Maybe (NoteName, String),  -- Root and chord quality
    presetArpeggioPattern :: Maybe String,  -- Pattern name
    -- BPM settings
    presetBPM :: Double,
    presetBPMMode :: BPMMode,
    presetQuantization :: QuantizationGrid,
    presetTimeSignature :: TimeSignature,
    presetSwing :: Double,
    -- SoundFont settings
    presetSoundFont :: Maybe FilePath,
    presetProgram :: Maybe Int,  -- MIDI program (0-127)
    presetBank :: Maybe Int,     -- MIDI bank (0-127)
    -- RT settings
    presetSingleCore :: Bool,
    presetPriorityAudio :: Int,
    presetPriorityDetect :: Int
} deriving (Show, Generic, Eq)

-- Preset library
type PresetLibrary = Map String Preset

-- Default preset configuration
defaultPreset :: Preset
defaultPreset = Preset {
    presetName = "Default",
    presetDescription = "Balanced settings for general use",
    presetOnsetThreshold = 0.67,
    presetFastValidationMs = 2.66,
    presetBassFastMs = 12.0,
    presetBassConfirmMs = 30.0,
    presetScale = Nothing,
    presetArpeggio = Nothing,
    presetArpeggioPattern = Nothing,
    presetBPM = 120.0,
    presetBPMMode = TapTempo,
    presetQuantization = Q16th,
    presetTimeSignature = TimeSignature 4 4,
    presetSwing = 0.0,
    presetSoundFont = Nothing,
    presetProgram = Nothing,
    presetBank = Nothing,
    presetSingleCore = True,
    presetPriorityAudio = 99,
    presetPriorityDetect = 98
}

-- Built-in presets
jazzWalkingBass :: Preset
jazzWalkingBass = defaultPreset {
    presetName = "jazz",
    presetDescription = "Jazz walking bass with swing feel",
    presetScale = Just (C, Dorian),
    presetArpeggio = Just (C, "minor7"),
    presetArpeggioPattern = Just "walking-bass",
    presetBPM = 120.0,
    presetSwing = 0.33,  -- Light swing
    presetProgram = Just 33,  -- Electric Bass
    presetFastValidationMs = 5.33  -- Slightly slower for bass
}

classicalGuitar :: Preset
classicalGuitar = defaultPreset {
    presetName = "classical",
    presetDescription = "Classical guitar fingerpicking",
    presetScale = Just (E, Major),
    presetArpeggio = Just (E, "major"),
    presetArpeggioPattern = Just "fingerpicking",
    presetBPM = 100.0,
    presetSwing = 0.0,
    presetProgram = Just 24,  -- Nylon Guitar
    presetQuantization = Q8th
}

bluesLead :: Preset
bluesLead = defaultPreset {
    presetName = "blues",
    presetDescription = "Blues lead guitar with bends",
    presetScale = Just (A, Blues),
    presetArpeggio = Just (A, "dominant7"),
    presetArpeggioPattern = Just "up-down",
    presetBPM = 100.0,
    presetSwing = 0.5,  -- Heavy swing
    presetProgram = Just 27,  -- Clean Guitar
    presetOnsetThreshold = 0.55  -- More sensitive
}

electronic :: Preset
electronic = defaultPreset {
    presetName = "electronic",
    presetDescription = "Electronic music with sharp timing",
    presetScale = Just (C, Chromatic),
    presetBPM = 128.0,
    presetBPMMode = FixedBPM,
    presetQuantization = Q16th,
    presetSwing = 0.0,
    presetProgram = Just 81,  -- Lead 1 (Square)
    presetFastValidationMs = 2.66,
    presetOnsetThreshold = 0.7
}

practiceMode :: Preset
practiceMode = defaultPreset {
    presetName = "practice",
    presetDescription = "Slow tempo for practice with metronome",
    presetScale = Just (C, Major),
    presetBPM = 60.0,
    presetQuantization = QQuarter,  -- Slower quantization
    presetSwing = 0.0,
    presetFastValidationMs = 5.0,  -- More accurate
    presetBassConfirmMs = 50.0
}

-- All built-in presets
defaultPresets :: PresetLibrary
defaultPresets = Map.fromList [
    ("jazz", jazzWalkingBass),
    ("classical", classicalGuitar),
    ("blues", bluesLead),
    ("electronic", electronic),
    ("practice", practiceMode)
  ]

-- Get preset directory
getPresetDir :: IO FilePath
getPresetDir = do
    home <- getHomeDirectory
    let dir = home </> ".config" </> "demod-note" </> "presets"
    createDirectoryIfMissing True dir
    return dir

-- Load all presets (built-in + custom)
loadAllPresets :: IO PresetLibrary
loadAllPresets = do
    let builtins = defaultPresets
    presetDir <- getPresetDir
    dirExists <- doesDirectoryExist presetDir
    if dirExists
        then do
            files <- listDirectory presetDir
            let tomlFiles = filter (".toml" `isSuffixOf`) files
            customPresets <- forM tomlFiles $ \f -> do
                let path = presetDir </> f
                loadPreset path
            let loaded = [p | Right p <- customPresets]
            return $ Map.union (Map.fromList [(presetName p, p) | p <- loaded]) builtins
        else return builtins

-- List all available preset names
listPresets :: IO [String]
listPresets = do
    presets <- loadAllPresets
    return $ sort $ Map.keys presets

-- Get preset by name
getPresetByName :: String -> IO (Maybe Preset)
getPresetByName name = do
    presets <- loadAllPresets
    return $ Map.lookup name presets

-- Apply preset to configuration
applyPreset :: Preset -> Config -> Config
applyPreset preset cfg = cfg {
    detection = (detection cfg) {
        onsetThresh = presetOnsetThreshold preset,
        fastValidationMs = presetFastValidationMs preset,
        bassFastMs = presetBassFastMs preset,
        bassConfirmMs = presetBassConfirmMs preset
    },
    rt = (rt cfg) {
        singleCore = presetSingleCore preset,
        priorityAudio = presetPriorityAudio preset,
        priorityDetect = presetPriorityDetect preset
    },
    activePreset = presetName preset
}

-- Load preset from file (TOML)
loadPreset :: FilePath -> IO (Either String Preset)
loadPreset path = do
    exists <- doesFileExist path
    if not exists
    then return $ Left $ "Preset file not found: " ++ path
    else do
        content <- readFile path
        case parseSimplePreset content of
            Left err -> return $ Left $ "Parse error: " ++ err
            Right p -> return $ Right p

-- Simple parser that extracts preset name from TOML
parseSimplePreset :: String -> Either String Preset
parseSimplePreset content =
    case nameLine of
        Nothing -> Left "Missing 'name' field in preset"
        Just n  -> Right $ defaultPreset { presetName = n }
  where
    ls = lines content
    nameLine = do
        -- Find the first line that starts with "name" (ignoring leading spaces)
        l <- find (\line -> "name" `isPrefixOf` dropWhile (== ' ') (stripComments line)) ls
        -- Split on '=' and strip the value of whitespace and optional TOML quotes
        let val = strip (drop 1 (dropWhile (/= '=') l))
        if null val then Nothing else Just val
    stripComments l = if '#' `elem` l then takeWhile (/= '#') l else l
    -- Strip surrounding whitespace, then strip a single pair of double-quotes if present
    strip s =
        let s' = dropWhile (== ' ') (reverse (dropWhile (== ' ') (reverse s)))
        in  case s' of
                ('"':rest) -> takeWhile (/= '"') rest
                _          -> s'

-- Save preset to file (TOML)
savePreset :: Preset -> FilePath -> IO ()
savePreset preset path = do
    let content = generatePresetTOML preset
    writeFile path content

-- Generate TOML content for preset
generatePresetTOML :: Preset -> String
generatePresetTOML preset = unlines [
    "# DeMoDNote Preset: " ++ presetName preset,
    "description = " ++ show (presetDescription preset),
    "",
    "[detection]",
    "onsetThreshold = " ++ show (presetOnsetThreshold preset),
    "fastValidationMs = " ++ show (presetFastValidationMs preset),
    "bassFastMs = " ++ show (presetBassFastMs preset),
    "bassConfirmMs = " ++ show (presetBassConfirmMs preset),
    "",
    "[scale]",
    case presetScale preset of
        Just (root, sType) -> "root = \"" ++ show root ++ "\"\ntype = \"" ++ show sType ++ "\""
        Nothing -> "# No scale specified",
    "",
    "[bpm]",
    "tempo = " ++ show (presetBPM preset),
    "mode = \"" ++ show (presetBPMMode preset) ++ "\"",
    "quantization = \"" ++ show (presetQuantization preset) ++ "\"",
    "swing = " ++ show (presetSwing preset),
    "",
    "[soundfont]",
    case presetSoundFont preset of
        Just sf -> "path = " ++ show sf
        Nothing -> "# No soundfont specified",
    case presetProgram preset of
        Just prog -> "program = " ++ show prog
        Nothing -> "",
    "",
    "[rt]",
    "singleCore = " ++ show (presetSingleCore preset),
    "priorityAudio = " ++ show (presetPriorityAudio preset),
    "priorityDetect = " ++ show (presetPriorityDetect preset)
  ]

-- Add custom preset to library
addCustomPreset :: Preset -> IO ()
addCustomPreset preset = do
    presetDir <- getPresetDir
    let path = presetDir </> (presetName preset ++ ".toml")
    savePreset preset path

-- Delete custom preset
deletePreset :: String -> IO ()
deletePreset name = do
    presetDir <- getPresetDir
    let path = presetDir </> (name ++ ".toml")
    exists <- doesFileExist path
    if exists
    then removeFile path
    else putStrLn $ "Preset not found: " ++ name

-- Convert string to scale type (for future use)
parseScaleType :: String -> ScaleType
parseScaleType "major" = Major
parseScaleType "minor" = MinorNatural
parseScaleType "minor-natural" = MinorNatural
parseScaleType "minor-harmonic" = MinorHarmonic
parseScaleType "minor-melodic" = MinorMelodic
parseScaleType "pentatonic-major" = PentatonicMajor
parseScaleType "pentatonic-minor" = PentatonicMinor
parseScaleType "blues" = Blues
parseScaleType "chromatic" = Chromatic
parseScaleType "dorian" = Dorian
parseScaleType "phrygian" = Phrygian
parseScaleType "lydian" = Lydian
parseScaleType "mixolydian" = Mixolydian
parseScaleType "aeolian" = Aeolian
parseScaleType "locrian" = Locrian
parseScaleType _ = Major  -- Default

-- Convert string to note name (for future use)
parseNoteName :: String -> NoteName
parseNoteName "c" = C
parseNoteName "c#" = Cs
parseNoteName "d" = D
parseNoteName "d#" = Ds
parseNoteName "e" = E
parseNoteName "f" = F
parseNoteName "f#" = Fs
parseNoteName "g" = G
parseNoteName "g#" = Gs
parseNoteName "a" = A
parseNoteName "a#" = As
parseNoteName "b" = B
parseNoteName _ = C  -- Default
