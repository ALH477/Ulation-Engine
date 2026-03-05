{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module DeMoDNote.Backend (
    module DeMoDNote.Types,
    module DeMoDNote.Config, 
    module DeMoDNote.Detector,
    DetectionEvent(..),
    runBackend,
    runBackendSimple,
    JackState(..),
    newJackState,
    cfloatToInt16
) where

import DeMoDNote.Types
import DeMoDNote.Config hiding (sampleRate, bufferSize)
import DeMoDNote.Detector
import DeMoDNote.OSC
import DeMoDNote.SoundFont (SoundFontManager, isFluidSynthRunning, stopFluidSynth)

import qualified Sound.JACK as JACK
import qualified Sound.JACK.Audio as JAudio
import qualified Control.Monad.Exception.Synchronous as Sync
import qualified Control.Monad.Trans.Class as Trans

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM

import Foreign.C.Error (Errno(..))
import Foreign.C.Types (CFloat(..))
import Foreign.Marshal.Array (copyArray)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekElemOff)

import Control.Concurrent (threadDelay, forkIO, killThread, MVar, newEmptyMVar, tryTakeMVar, tryPutMVar, ThreadId)
import Control.Concurrent.STM
import Control.Exception (try, SomeException, catch, Exception)
-- Note: DeMoDNote.Error provides typed error handling (DeMoDError).
-- Consider migrating SomeException usage to typed errors for better error handling.
import Control.Monad (forM_, when, forever, void)
import Data.Int (Int16)
import Data.IORef
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import System.Posix.Signals (installHandler, sigINT, sigTERM, Handler(..))
import System.Process (spawnProcess, readProcess)

-------------------------------------------------------------------------------
-- Logging
-------------------------------------------------------------------------------

logInfo :: String -> IO ()
logInfo s = putStrLn s >> hFlush stdout

logErr :: String -> IO ()
logErr s = hPutStrLn stderr s >> hFlush stderr

-------------------------------------------------------------------------------
-- JackState (uses JackStatus from Types.hs)
-------------------------------------------------------------------------------

data JackState = JackState
    { jsStatus               :: !(IORef JackStatus)
    , jsReconnectAttempts    :: !(IORef Int)
    , jsMaxReconnectAttempts :: !Int
    , jsShouldReconnect      :: !(IORef Bool)
    -- | ThreadId of the currently running detector thread.
    --   Updated by jackSession so the reconnect loop can kill the old thread
    --   before starting a new JACK session, preventing multiple detectors
    --   racing on the same ring buffer.
    , jsDetectorThread       :: !(IORef (Maybe ThreadId))
    }

newJackState :: Int -> IO JackState
newJackState maxAttempts =
    JackState
        <$> newIORef JackDisconnected
        <*> newIORef 0
        <*> pure maxAttempts
        <*> newIORef True
        <*> newIORef Nothing

-------------------------------------------------------------------------------
-- JackException
-------------------------------------------------------------------------------

data JackException
    = JackConnectionFailed String
    | JackDisconnectedErr  String
    | JackReconnectFailed  Int
    | JackClientError      String
    deriving (Show)

instance Exception JackException

-------------------------------------------------------------------------------
-- DetectionEvent
-------------------------------------------------------------------------------

data DetectionEvent = DetectionEvent
    { deNote         :: Maybe (Int, Int)
    , deConfidence   :: Double
    , deLatency      :: Double
    , deWaveform     :: [Double]
    , deState        :: NoteState
    , deTuningNote   :: Maybe Int
    , deTuningCents  :: Double
    , deTuningInTune :: Bool
    , deJackStatus   :: JackStatus
    }

-------------------------------------------------------------------------------
-- JACK Server Management
-------------------------------------------------------------------------------

isJACKRunning :: IO Bool
isJACKRunning = do
    result <- try (readProcess "jack_control" ["status"] "") :: IO (Either SomeException String)
    case result of
        Right output ->
            -- "running" `elem` words "not running"  →  True  (false positive!).
            -- Check for the absence of "not" before "running" instead.
            let ws = words output
            in  return $ "running" `elem` ws && "not" `notElem` takeWhile (/= "running") ws
        Left _ -> do
            -- Fall back to pgrep; a successful exit (Right) means jackd was found.
            result2 <- try (readProcess "pgrep" ["-x", "jackd"] "") :: IO (Either SomeException String)
            return $ case result2 of
                Right out -> not (null (words out))
                Left  _   -> False

startJACKServer :: IO Bool
startJACKServer = do
    putStrLn "Attempting to start JACK server..."
    result <- try $ spawnProcess "jackd" ["-d", "dummy", "-r", "44100", "-p", "256", "-n", "2"]
    case result of
        Left e  -> logErr ("Failed to spawn jackd: " ++ show (e :: SomeException)) >> return False
        Right _ -> do
            -- jackd is a daemon; give it 2 s to initialise before checking.
            -- The original code called waitForProcess, which blocks until jackd
            -- exits (potentially hours), making this function never return.
            threadDelay 2_000_000
            ok <- isJACKRunning
            if ok then logInfo "JACK server started." >> return True
                  else logErr  "JACK server failed to start." >> return False

-------------------------------------------------------------------------------
-- Reconnection with Exponential Backoff
-------------------------------------------------------------------------------

reconnectBackoff :: Int -> IO ()
reconnectBackoff n = do
    let µs = min 16000000 (2 ^ n * 1000000)
    logInfo $ "Backoff: waiting " ++ show (µs `div` 1000000) ++ " s..."
    threadDelay µs

attemptReconnect :: JackState -> IO Bool
attemptReconnect st = do
    attempts <- readIORef (jsReconnectAttempts st)
    let maxAttempts = jsMaxReconnectAttempts st
    if attempts >= maxAttempts
        then do
            logErr "Max reconnection attempts reached"
            return False
        else do
            logInfo $ "Reconnection attempt " ++ show (attempts + 1) ++ "..."
            modifyIORef' (jsReconnectAttempts st) (+ 1)
            reconnectBackoff attempts
            return True

-------------------------------------------------------------------------------
-- Time
-------------------------------------------------------------------------------

-- | Monotonic wall-clock time in microseconds.
--   Previously used getCPUTime which returns CPU-time-used (not wall time)
--   in picoseconds — wrong for timestamping real-time audio events on a
--   multi-core machine where CPU time can be faster or slower than wall time.
getMicroTime :: IO Word64
getMicroTime = do
    t <- getMonotonicTimeNSec
    return $! t `div` 1000

-------------------------------------------------------------------------------
-- Audio Ring Buffer
-------------------------------------------------------------------------------

data AudioRingBuffer = AudioRingBuffer
    { rbMVec   :: !(VSM.IOVector Int16)
    , rbWritePos :: !(IORef Int)
    , rbSize   :: !Int
    }

newAudioRingBuffer :: Int -> IO AudioRingBuffer
newAudioRingBuffer size = do
    mvec <- VSM.new size
    wp <- newIORef 0
    return $ AudioRingBuffer mvec wp size

-- | Write n CFloat samples from @ptr@ into the ring buffer starting at @wp@,
--   wrapping around correctly.  Previously only the first chunk (up to the end
--   of the buffer) was written; samples past the wrap point were silently lost.
writeFloat2Int16Frame :: VSM.IOVector Int16 -> Int -> Int -> Ptr CFloat -> Int -> IO Int
writeFloat2Int16Frame mvec wp size ptr n = do
    let remaining = size - wp
        chunk1    = min n remaining          -- samples before the wrap
        chunk2    = n - chunk1               -- samples after the wrap (0 if no wrap)
    -- First chunk: wp .. wp+chunk1-1
    forM_ [0..chunk1-1] $ \i -> do
        val <- peekElemOff ptr i :: IO CFloat
        VSM.write mvec (wp + i) (cfloatToInt16 val)
    -- Second chunk: 0 .. chunk2-1  (only when n > remaining)
    forM_ [0..chunk2-1] $ \i -> do
        val <- peekElemOff ptr (chunk1 + i) :: IO CFloat
        VSM.write mvec i (cfloatToInt16 val)
    return $ (wp + n) `mod` size

-- Write audio samples from JACK callback to ring buffer and signal detector thread
writeToRingBuffer :: AudioState -> Ptr CFloat -> Int -> IO ()
writeToRingBuffer audioState buf n = do
    let rb   = audioRingBuffer audioState
        mvec = rbMVec rb
        size = rbSize rb
    wp    <- readIORef (rbWritePos rb)
    newWp <- writeFloat2Int16Frame mvec wp size buf n
    -- writeFloat2Int16Frame already returns (wp + n) `mod` size.
    writeIORef (rbWritePos rb) newWp
    _ <- tryPutMVar (rbSemaphore audioState) ()
    return ()

-------------------------------------------------------------------------------
-- AudioState
-------------------------------------------------------------------------------

data AudioState = AudioState
    { audioRingBuffer :: !AudioRingBuffer
    , sampleCounter   :: !(TVar Word64)
    , running         :: !(TVar Bool)
    , currentNote     :: !(TVar (Maybe (Int, Int)))
    , lastConfidence  :: !(TVar Double)
    , lastLatency     :: !(TVar Double)
    , lastWaveform    :: !(TVar [Double])
    , oscClient       :: !(TVar (Maybe OscClient))
    , sampleRate      :: !(TVar Int)
    , bufferSize      :: !(TVar Int)
    , xrunCount       :: !(IORef Word64)
    , rbSemaphore     :: !(MVar ())
    }

newAudioState :: Int -> IO AudioState
newAudioState ringSize = do
    rb <- newAudioRingBuffer ringSize
    counter <- newTVarIO 0
    run <- newTVarIO True
    note <- newTVarIO Nothing
    conf <- newTVarIO 0.0
    lat <- newTVarIO 0.0
    wave <- newTVarIO []
    osc <- newTVarIO Nothing
    sr <- newTVarIO 44100
    bs <- newTVarIO 256
    xruns <- newIORef 0
    sem <- newEmptyMVar
    return $ AudioState rb counter run note conf lat wave osc sr bs xruns sem

cfloatToInt16 :: CFloat -> Int16
cfloatToInt16 (CFloat f) = round (max (-1.0) (min 1.0 f) * 32767)

-------------------------------------------------------------------------------
-- Real-time Process Callback (Exact jack-0.7.2.2 signature + NFrames unwrap)
-------------------------------------------------------------------------------

jackProcessCallback
    :: AudioState
    -> JACK.Port JAudio.Sample JACK.Input
    -> JACK.Port JAudio.Sample JACK.Output
    -> JACK.NFrames
    -> Sync.ExceptionalT Errno IO ()
jackProcessCallback audioState inPort outPort nframes = Trans.lift go
  where
    go = do
        let JACK.NFrames w = nframes
            n = fromIntegral w :: Int

        -- A zero-frame callback is JACK's xrun notification.
        if n == 0
          then modifyIORef' (xrunCount audioState) (+1)
          else do
            inPtr <- JAudio.getBufferPtr inPort nframes
            writeToRingBuffer audioState (castPtr inPtr) n

            outPtr <- JAudio.getBufferPtr outPort nframes
            -- copyArray takes an *element* count, not a byte count.
            -- The original (n * sizeOf CFloat) was copying 4× too many elements,
            -- overwriting memory beyond the end of the output buffer.
            copyArray outPtr inPtr n
        
        return ()

-------------------------------------------------------------------------------
-- Clean control loop (plain IO)
-------------------------------------------------------------------------------

jackControlLoop :: AudioState -> IO ()
jackControlLoop audioState = loop
  where
    loop = do
        isRunning <- atomically $ readTVar (running audioState)
        if isRunning
            then threadDelay 100000 >> loop
            else logInfo "Shutdown requested - exiting JACK session"

-------------------------------------------------------------------------------
-- One full JACK lifetime (withClientDefault + withPort + withProcess)
-------------------------------------------------------------------------------

jackSession
    :: Config
    -> AudioState
    -> JackState
    -> TVar ReactorState
    -> IO ()
jackSession cfg audioState jackState stateVar = do
    logInfo "Starting JACK client..."

    -- Kill any detector thread left over from a previous session.
    -- Without this, every reconnect spawns an additional detector; after N
    -- reconnects there are N detectors all reading the same ring buffer.
    mOld <- readIORef (jsDetectorThread jackState)
    case mOld of
        Just tid -> do
            killThread tid
            logInfo "Stopped previous detector thread."
        Nothing  -> return ()

    tid <- forkIO $ detectorThread cfg audioState jackState stateVar
    writeIORef (jsDetectorThread jackState) (Just tid)

    JACK.handleExceptions $
        JACK.withClientDefault "DeMoDNote" $ \client ->
            JACK.withPort client "input" $ \inPort ->
                JACK.withPort client "output" $ \outPort ->
                    JACK.withProcess client (jackProcessCallback audioState inPort outPort) $
                        JACK.withActivation client $ do
                            sr <- Trans.lift $ JACK.getSampleRate client
                            bs <- Trans.lift $ JACK.getBufferSize client

                            Trans.lift $ atomically $ writeTVar (sampleRate audioState) sr
                            Trans.lift $ atomically $ writeTVar (bufferSize audioState) bs
                            Trans.lift $ writeIORef (jsStatus jackState) JackConnected

                            Trans.lift $ logInfo $
                                "✅ JACK connected (SR=" ++ show sr ++ ", BS=" ++ show bs ++ ")"

                            Trans.lift $ jackControlLoop audioState

    logInfo "JACK session ended cleanly"

-------------------------------------------------------------------------------
-- Reconnection Loop
-------------------------------------------------------------------------------

jackLoop :: Config -> AudioState -> JackState -> TVar ReactorState -> IO ()
jackLoop cfg audioState jackState stateVar = do
    recon    <- readIORef (jsShouldReconnect jackState)
    running' <- atomically $ readTVar (running audioState)
    when (recon && running') $ do
        catch (jackSession cfg audioState jackState stateVar) 
              (\(e :: SomeException) -> do
                logErr $ "JACK session lost: " ++ show e
                writeIORef (jsStatus jackState) (JackError "session lost")
                ok <- attemptReconnect jackState
                when ok $ jackLoop cfg audioState jackState stateVar)

-------------------------------------------------------------------------------
-- Detector Thread (exact original)
-------------------------------------------------------------------------------

detectorThread :: Config -> AudioState -> JackState -> TVar ReactorState -> IO ()
detectorThread cfg audioState jackState stateVar = do
    logInfo "Detector thread started."
    loop (0 :: Int)
  where
    loop !silentTicks = do
        isRunning <- atomically $ readTVar (running audioState)
        if not isRunning
          then do
            sendFinalNoteOff audioState
            logInfo "Detector thread stopping."
          else do
            mReady <- tryTakeMVar (rbSemaphore audioState)
            case mReady of
                Nothing -> do
                    threadDelay 10000
                    xruns <- readIORef (xrunCount audioState)
                    -- Log once when the threshold is first crossed, not every tick.
                    when (silentTicks == 200) $
                        logErr $ "[Detector] No JACK frames for ~2s (xruns=" ++ show xruns ++ ")"
                    loop (silentTicks + 1)
                Just () -> do
                    processFrame cfg audioState jackState stateVar
                    loop 0

sendFinalNoteOff :: AudioState -> IO ()
sendFinalNoteOff audioState = do
    curr <- atomically $ readTVar (currentNote audioState)
    case curr of
        Just (note, _) -> do
            mClient <- atomically $ readTVar (oscClient audioState)
            case mClient of
                Just client -> sendNoteOff client note `catch` (\(_ :: SomeException) -> return ())
                Nothing -> return ()
        Nothing -> return ()

processFrame :: Config -> AudioState -> JackState -> TVar ReactorState -> IO ()
processFrame cfg audioState jackState stateVar = do
    let rb = audioRingBuffer audioState
    wp   <- readIORef (rbWritePos rb)
    let size = rbSize rb

    -- Read last 256 samples from the ring buffer.
    let startPos = if wp >= 256 then wp - 256 else size - (256 - wp)
    samplesVec <- VS.generateM 256 $ \i ->
        VSM.read (rbMVec rb) ((startPos + i) `mod` size)
    let samplesInt = VS.map fromIntegral samplesVec :: VS.Vector Int

    when (VS.length samplesVec >= 128) $ do
        currentTime <- getMicroTime

        let !samplesD = VS.map (\x -> fromIntegral x / 32768.0 :: Double) samplesVec
            !waveform = VS.toList samplesD

        -- Read previous reactor state so detection is stateful across frames.
        -- Previously Idle/defaultPLLState/defaultOnsetFeatures were hard-coded,
        -- meaning the note state-machine could never progress past its first frame.
        prevRS <- atomically $ readTVar stateVar
        let prevNoteState    = noteStateMach  prevRS
            prevPLL          = pllStateMach   prevRS
            prevOnset        = onsetFeatures  prevRS

        result <- detect cfg samplesInt currentTime prevNoteState prevPLL prevOnset

        let (detTuningNote, detTuningCents) = case detectedNote result of
                Nothing        -> (Nothing, 0.0)
                Just (note, _) ->
                    let (nearest, cents) = nearestNote (midiToFreq note)
                    in  (Just nearest, cents)
            detTuningInTune = isInTune detTuningCents

        -- Compute actual latency from JACK buffer size and sample rate.
        -- Previously hardcoded to 2.66 ms regardless of config.
        sr  <- atomically $ readTVar (sampleRate audioState)
        bs  <- atomically $ readTVar (bufferSize audioState)
        let !actualLatency = if sr > 0
                             then fromIntegral bs / fromIntegral sr * 1000.0
                             else 2.66 :: Double

        -- Update all TVars in a single transaction so readers never observe
        -- a half-updated state (previously three separate atomically calls).
        jackSt <- readIORef (jsStatus jackState)
        let newRS = ReactorState
              { currentNotes        = maybe [] (:[]) (detectedNote result)
              , noteStateMach       = noteState result
              , pllStateMach        = pllState  result   -- persist PLL state
              , onsetFeatures       = onsetState result  -- persist onset features
              , lastOnsetTime       = currentTime
              , config              = cfg
              , reactorBPM          = prevRS `seq` reactorBPM prevRS -- preserve tap BPM
              , reactorThreshold    = reactorThreshold prevRS
              , jackStatus          = jackSt
              , detectionConfidence = confidence result
              , detectionLatency    = actualLatency
              , latestWaveform      = waveform
              , detectedTuningNote  = detTuningNote
              , detectedTuningCents = detTuningCents
              , detectedTuningInTune = detTuningInTune
              }
        atomically $ do
            writeTVar (lastConfidence audioState) (confidence result)
            writeTVar (lastLatency    audioState) actualLatency
            writeTVar (lastWaveform   audioState) waveform
            writeTVar stateVar newRS

        handleNoteChange audioState (detectedNote result) (noteState result)
                         detTuningNote detTuningCents detTuningInTune

handleNoteChange :: AudioState -> Maybe (MIDINote, Velocity) -> NoteState -> Maybe Int -> Double -> Bool -> IO ()
handleNoteChange audioState mNote noteState detTuningNote detTuningCents detTuningInTune = do
    curr    <- atomically $ readTVar (currentNote audioState)
    mClient <- atomically $ readTVar (oscClient   audioState)   -- read once
    let sendOn  n v = case mClient of
            Just c  -> sendNoteOn  c n v `catch` (\(_ :: SomeException) -> return ())
            Nothing -> return ()
        sendOff n   = case mClient of
            Just c  -> sendNoteOff c n   `catch` (\(_ :: SomeException) -> return ())
            Nothing -> return ()
    case (curr, mNote) of
        (Nothing, Just (note, vel)) -> do
            logInfo $ "Note On: " ++ show note
            sendOn note vel
            atomically $ writeTVar (currentNote audioState) $ Just (note, vel)
        (Just (note, _), Nothing) -> do
            logInfo $ "Note Off: " ++ show note
            sendOff note
            atomically $ writeTVar (currentNote audioState) Nothing
        (Just (oldNote, _), Just (newNote, vel)) | newNote /= oldNote -> do
            logInfo $ "Note Off: " ++ show oldNote
            sendOff oldNote
            logInfo $ "Note On: " ++ show newNote
            sendOn newNote vel
            atomically $ writeTVar (currentNote audioState) $ Just (newNote, vel)
        _ -> return ()

-------------------------------------------------------------------------------
-- Main Backend Entry Points
-------------------------------------------------------------------------------

runBackend :: Config -> TVar ReactorState -> Maybe SoundFontManager -> IO ()
runBackend cfg state mSynthManager = do
    logInfo "Starting DeMoD-Note JACK backend..."
    
    started <- startJACKServer
    when started $ logInfo "JACK server started"
    
    audioState <- newAudioState 8192
    jackState <- newJackState 5
    
    mOscClient <- (do
        client <- createOscClient "127.0.0.1" 57120
        logInfo "OSC client connected"
        return $ Just client) `catch` (\e -> do
        logErr $ "Could not create OSC client: " ++ show (e :: SomeException)
        return Nothing)
    atomically $ writeTVar (oscClient audioState) mOscClient
    
    -- Log FluidSynth status
    case mSynthManager of
        Nothing -> logInfo "FluidSynth: disabled"
        Just sm -> do
            running <- isFluidSynthRunning sm
            if running
                then logInfo "FluidSynth: connected and ready"
                else logInfo "FluidSynth: not running"
    
    _ <- installHandler sigINT (Catch $ do
        atomically $ writeTVar (running audioState) False
        writeIORef (jsShouldReconnect jackState) False
        logInfo "SIGINT received, shutting down..."
        case mSynthManager of
            Nothing -> return ()
            Just sm -> void $ try @SomeException $ stopFluidSynth sm
        ) Nothing
    _ <- installHandler sigTERM (Catch $ do
        atomically $ writeTVar (running audioState) False  
        writeIORef (jsShouldReconnect jackState) False
        logInfo "SIGTERM received, shutting down..."
        case mSynthManager of
            Nothing -> return ()
            Just sm -> void $ try @SomeException $ stopFluidSynth sm
        ) Nothing
    
    forkIO $ forever $
        jackLoop cfg audioState jackState state
            `catch` (\(e :: SomeException) -> do
                logErr $ "jackLoop crashed: " ++ show e
                threadDelay 1_000_000)
    
    logInfo "DeMoD-Note JACK backend started successfully."
    
    forever $ threadDelay 1000000

runBackendSimple :: Config -> TVar ReactorState -> IO ()
runBackendSimple _cfg _state = do
    logInfo "Starting DeMoDNote (passthrough mode)..."
    logInfo "Note: JACK server must be running with input/output ports"
    logInfo "Use: jackd -d dummy -r 44100 -p 256"
    logInfo "Then connect: jack_connect system:capture_1 DeMoDNote:input"
    logInfo "         jack_connect DeMoDNote:output system:playback_1"
    -- Block the calling thread so the process stays alive.
    -- Previously returned immediately, which would cause callers that expect
    -- a blocking call (e.g. the main thread) to exit.
    forever $ threadDelay 1_000_000