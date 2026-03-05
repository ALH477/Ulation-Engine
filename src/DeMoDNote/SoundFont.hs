module DeMoDNote.SoundFont (
    SoundFont(..),
    SoundFontManager,
    SoundFontInfo(..),
    initSoundFontManager,
    loadSoundFont,
    unloadSoundFont,
    getLoadedSoundFont,
    listSoundFonts,
    findSoundFont,
    validateSoundFont,
    getSoundFontInfo,
    setMIDIProgram,
    setMIDIBank,
    sendMidiProgramChange,
    -- Default soundfonts
    defaultSoundFontPaths,
    appSoundFontDir,
    systemSoundFontDir,
    userSoundFontDir,
    -- FluidSynth integration
    startFluidSynth,
    stopFluidSynth,
    isFluidSynthRunning,
    restartFluidSynth,
    reloadSoundFont
) where

import System.Directory (doesFileExist, doesDirectoryExist, getHomeDirectory, listDirectory, createDirectoryIfMissing, getFileSize)
import System.FilePath ((</>), takeFileName, takeExtension)
import System.Process (ProcessHandle, terminateProcess, getProcessExitCode, spawnProcess)

-- import Control.Concurrent (threadDelay)  -- Kept for future use
-- import Control.Monad (filterM, when)  -- Kept for future use
import Data.List (isSuffixOf, find)
-- import Data.Maybe (fromMaybe)  -- Kept for future use

-- SoundFont file information
data SoundFont = SoundFont {
    sfPath :: FilePath,
    sfName :: String,
    sfSize :: Integer,
    sfLoaded :: Bool,
    sfPrograms :: [Int]  -- Available MIDI programs in this soundfont
} deriving (Show, Eq)

-- SoundFont metadata
data SoundFontInfo = SoundFontInfo {
    sfiName :: String,
    sfiPath :: FilePath,
    sfiVersion :: String,
    sfiTargetEngine :: String,  -- e.g., "FluidSynth", "GeneralUser"
    sfiPresets :: [String],     -- Preset names
    sfiSizeMB :: Double
} deriving (Show, Eq)

-- SoundFont manager state
data SoundFontManager = SoundFontManager {
    sfmAppDir :: FilePath,       -- /etc/demod/sf (app-specific)
    sfmSystemDir :: FilePath,
    sfmUserDir :: FilePath,
    sfmCurrentSoundFont :: Maybe SoundFont,
    sfmMidiProgram :: Int,
    sfmMidiBank :: Int,
    sfmFluidSynthProcess :: Maybe ProcessHandle,
    sfmFluidSynthPort :: Int
}

-- Application-specific soundfont directory (admin-managed)
appSoundFontDir :: FilePath
appSoundFontDir = "/etc/demod/sf"

-- Default search paths for soundfonts (priority order)
defaultSoundFontPaths :: [FilePath]
defaultSoundFontPaths = [
    "/etc/demod/sf",           -- App-specific (highest priority)
    "/usr/share/soundfonts",
    "/usr/local/share/soundfonts",
    "/opt/soundfonts"
    ]

systemSoundFontDir :: FilePath
systemSoundFontDir = "/usr/share/soundfonts"

userSoundFontDir :: IO FilePath
userSoundFontDir = do
    home <- getHomeDirectory
    let dir = home </> ".local" </> "share" </> "soundfonts"
    createDirectoryIfMissing True dir
    return dir

-- Initialize soundfont manager
initSoundFontManager :: IO SoundFontManager
initSoundFontManager = do
    userDir <- userSoundFontDir
    return $ SoundFontManager {
        sfmAppDir = appSoundFontDir,
        sfmSystemDir = systemSoundFontDir,
        sfmUserDir = userDir,
        sfmCurrentSoundFont = Nothing,
        sfmMidiProgram = 0,
        sfmMidiBank = 0,
        sfmFluidSynthProcess = Nothing,
        sfmFluidSynthPort = 9800
    }

-- Validate a soundfont file (check extension and existence)
validateSoundFont :: FilePath -> IO (Either String SoundFont)
validateSoundFont path = do
    exists <- doesFileExist path
    if not exists
    then return $ Left $ "SoundFont not found: " ++ path
    else do
        let ext = takeExtension path
        if ext `elem` [".sf2", ".sf3", ".dls"]
        then do
            fileSize <- getFileSize path
            return $ Right $ SoundFont {
                sfPath = path,
                sfName = takeFileName path,
                sfSize = fileSize,
                sfLoaded = False,
                sfPrograms = [0..127]
            }
        else return $ Left $ "Invalid SoundFont format: " ++ ext

-- Load a soundfont into the manager
loadSoundFont :: SoundFontManager -> FilePath -> IO (Either String SoundFontManager)
loadSoundFont manager path = do
    result <- validateSoundFont path
    case result of
        Left err -> return $ Left err
        Right sf -> do
            -- Stop existing fluidsynth if running
            _ <- stopFluidSynth manager
            -- Start new fluidsynth with this soundfont
            newManager <- startFluidSynth manager path
            return $ Right newManager {
                sfmCurrentSoundFont = Just sf { sfLoaded = True }
            }

-- Unload current soundfont
unloadSoundFont :: SoundFontManager -> IO SoundFontManager
unloadSoundFont manager = do
    manager' <- stopFluidSynth manager
    return $ manager' {
        sfmCurrentSoundFont = Nothing
    }

-- Get currently loaded soundfont
getLoadedSoundFont :: SoundFontManager -> Maybe SoundFont
getLoadedSoundFont = sfmCurrentSoundFont

-- List all available soundfonts in search paths (priority order)
listSoundFonts :: SoundFontManager -> IO [SoundFontInfo]
listSoundFonts manager = do
    appSFs <- listDirSFs (sfmAppDir manager)
    userSFs <- listDirSFs (sfmUserDir manager)
    systemSFs <- listDirSFs (sfmSystemDir manager)
    -- Priority order: app dir first, then user, then system
    let allPaths = appSFs ++ userSFs ++ systemSFs
    mapM pathToInfo allPaths
    where
        listDirSFs dir = do
            exists <- doesDirectoryExist dir
            if not exists
            then return []
            else do
                files <- listDirectory dir
                let sfFiles = filter (\f -> any (`isSuffixOf` f) [".sf2", ".sf3", ".dls"]) files
                return $ map (dir </>) sfFiles
        
        pathToInfo path = return $ SoundFontInfo {
            sfiName = takeFileName path,
            sfiPath = path,
            sfiVersion = "Unknown",
            sfiTargetEngine = "General MIDI",
            sfiPresets = [],  -- Would parse from SF2 file
            sfiSizeMB = 0.0   -- Would get from file
        }

-- Find a soundfont by name (partial match)
findSoundFont :: SoundFontManager -> String -> IO (Maybe SoundFontInfo)
findSoundFont manager query = do
    sfs <- listSoundFonts manager
    return $ find (\sf -> query `isInfixOf` sfiName sf) sfs
    where
        isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
        isPrefixOf [] _ = True
        isPrefixOf _ [] = False
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        tails [] = [[]]
        tails xs = xs : tails (drop 1 xs)

-- Get detailed info about a soundfont
getSoundFontInfo :: FilePath -> IO (Either String SoundFontInfo)
getSoundFontInfo path = do
    valid <- validateSoundFont path
    case valid of
        Left err -> return $ Left err
        Right sf -> return $ Right $ SoundFontInfo {
            sfiName = sfName sf,
            sfiPath = path,
            sfiVersion = "2.01",  -- Would parse from file
            sfiTargetEngine = "FluidSynth",
            sfiPresets = ["Acoustic Grand Piano", "Bright Acoustic Piano", "Electric Grand Piano"],  -- Would parse
            sfiSizeMB = fromIntegral (sfSize sf) / (1024 * 1024)
        }

-- Set MIDI program (instrument)
setMIDIProgram :: SoundFontManager -> Int -> IO SoundFontManager
setMIDIProgram manager prog = do
    let clamped = max 0 (min 127 prog)
    -- Send program change message
    sendMidiProgramChange manager clamped (sfmMidiBank manager)
    return $ manager { sfmMidiProgram = clamped }

-- Set MIDI bank
setMIDIBank :: SoundFontManager -> Int -> IO SoundFontManager
setMIDIBank manager bank = do
    let clamped = max 0 (min 127 bank)
    return $ manager { sfmMidiBank = clamped }

-- Send MIDI program change (would actually send to synth)
sendMidiProgramChange :: SoundFontManager -> Int -> Int -> IO ()
sendMidiProgramChange _manager program bank = do
    -- This would send actual MIDI messages
    putStrLn $ "MIDI Program Change: Bank " ++ show bank ++ ", Program " ++ show program

-- Start FluidSynth with a soundfont
startFluidSynth :: SoundFontManager -> FilePath -> IO SoundFontManager
startFluidSynth manager soundfontPath = do
    let port = sfmFluidSynthPort manager
        cmd  = "fluidsynth"
        -- Pass args directly to spawnProcess (not via shell) so paths with
        -- spaces are handled correctly.
        args = ["-a", "jack", "-m", "jack", "-g", "0.8", "-p", "DeMoDNote", soundfontPath]

    putStrLn $ "Starting FluidSynth with: " ++ soundfontPath
    ph <- spawnProcess cmd args
    return $ manager
        { sfmFluidSynthProcess = Just ph
        , sfmFluidSynthPort    = port
        , sfmCurrentSoundFont  = Just $ SoundFont soundfontPath (takeFileName soundfontPath) 0 True [0..127]
        }

-- Stop FluidSynth
stopFluidSynth :: SoundFontManager -> IO SoundFontManager
stopFluidSynth manager = do
    case sfmFluidSynthProcess manager of
        Nothing -> return manager
        Just ph -> do
            terminateProcess ph
            putStrLn "Stopping FluidSynth"
            return $ manager { sfmFluidSynthProcess = Nothing }

-- Check if FluidSynth is running
isFluidSynthRunning :: SoundFontManager -> IO Bool
isFluidSynthRunning manager = 
    case sfmFluidSynthProcess manager of
        Nothing -> return False
        Just ph -> do
            exitCode <- getProcessExitCode ph
            return $ case exitCode of
                Nothing -> True  -- Process still running
                Just _ -> False  -- Process has exited

-- Restart FluidSynth with current soundfont
restartFluidSynth :: SoundFontManager -> IO SoundFontManager
restartFluidSynth manager = do
    manager' <- stopFluidSynth manager
    case sfmCurrentSoundFont manager' of
        Nothing -> return manager'
        Just sf -> startFluidSynth manager' (sfPath sf)

-- Reload soundfont into currently running FluidSynth
reloadSoundFont :: SoundFontManager -> FilePath -> IO (Either String SoundFontManager)
reloadSoundFont manager path = do
    result <- validateSoundFont path
    case result of
        Left err -> return $ Left err
        Right _sf -> do
            running <- isFluidSynthRunning manager
            if running
                then do
                    manager' <- stopFluidSynth manager
                    manager'' <- startFluidSynth manager' path
                    return $ Right manager''
                else do
                    manager' <- startFluidSynth manager path
                    return $ Right manager'
