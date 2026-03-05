{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | OSC server/client for DeMoDNote.
--
-- Inbound  (oscReceiveLoop):    listens on port N for control messages
-- Outbound (OscClient):         sends note on/off to MIDI bridge on port M
-- Broadcast (statusBroadcaster): sends status to port N+1 at 20 Hz
--
-- Key fixes over original:
--  • AsciiString names are decoded with ascii_to_string (not show)
--  • statusBroadcaster uses a plain unconnected UDP socket + sendTo,
--    not a bound server socket (which has no peer and silently drops sends)
--  • The broadcast socket is bracketed with `finally close` so it is
--    always released even if the broadcast thread is cancelled
--  • resolveOnce caches the target SockAddr so getAddrInfo is called once
--    per OscClient lifetime instead of once per message

module DeMoDNote.OSC (
    module Sound.Osc,
    module Sound.Osc.Datum,
    OscCommand(..),
    OscState(..),
    newOscState,
    startOSC,
    OscClient(..),
    createOscClient,
    sendNoteOn,
    sendNoteOff,
    closeOscClient
) where

import Sound.Osc
import Sound.Osc.Datum
import Network.Socket
import Network.Socket.ByteString (sendTo)
import qualified Data.ByteString.Lazy as BL
import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay, Chan, newChan, writeChan, readChan)
import Control.Exception (try, SomeException, finally)
import Control.Monad (forever, when)
import Data.IORef
import qualified Sound.Osc.Transport.Fd as Fd
import Sound.Osc.Transport.Fd.Udp (Udp(..), with_udp, udpServer)
import System.IO (hPutStrLn, stderr)

import DeMoDNote.Preset
import DeMoDNote.Types

-- ─────────────────────────────────────────────────────────────────────────────
-- Command type
-- ─────────────────────────────────────────────────────────────────────────────

data OscCommand
    = SetBPM Double
    | SetThreshold Double
    | TriggerNote Int Int
    | ReleaseNote Int
    | LoadPreset String
    | GetStatus
    | SetDebug Bool
    deriving (Show, Eq)

-- ─────────────────────────────────────────────────────────────────────────────
-- OSC server state
-- ─────────────────────────────────────────────────────────────────────────────

data OscState = OscState
    { oscBPM       :: Double
    , oscThreshold :: Double
    , oscDebug     :: Bool
    , oscCommands  :: Chan OscCommand
    }

newOscState :: IO OscState
newOscState = do
    cmds <- newChan
    return OscState
        { oscBPM       = 120.0
        , oscThreshold = -40.0
        , oscDebug     = False
        , oscCommands  = cmds
        }

-- ─────────────────────────────────────────────────────────────────────────────
-- OSC server entry point
-- ─────────────────────────────────────────────────────────────────────────────

startOSC :: Int -> TVar ReactorState -> IO ()
startOSC port state = do
    oscState <- newOscState
    putStrLn $ "Starting OSC server on port " ++ show port
    _ <- forkIO $ oscCommandHandler oscState state
    _ <- forkIO $ statusBroadcaster port state
    with_udp (udpServer "127.0.0.1" port) $ \udp -> do
        putStrLn "OSC server listening..."
        oscReceiveLoop udp oscState state

oscReceiveLoop :: Udp -> OscState -> TVar ReactorState -> IO ()
oscReceiveLoop udp oscState state = forever $ do
    result <- try $ Fd.recvPacket udp
    case result of
        Left (_ :: SomeException) -> threadDelay 100000
        Right pkt -> case pkt of
            Packet_Message msg -> handleOscMessage msg oscState state
            _                  -> return ()

-- ─────────────────────────────────────────────────────────────────────────────
-- Message dispatch
-- ─────────────────────────────────────────────────────────────────────────────

handleOscMessage :: Message -> OscState -> TVar ReactorState -> IO ()
handleOscMessage msg oscState state = do
    let addr = messageAddress msg
        args = messageDatum msg

    case addr of
        "/demod/set/bpm" -> case args of
            [Float bpm] -> do
                writeChan (oscCommands oscState) $ SetBPM (realToFrac bpm)
                putStrLn $ "OSC: Set BPM = " ++ show bpm
            _ -> putStrLn "OSC: bad args for /demod/set/bpm"

        "/demod/set/threshold" -> case args of
            [Float db] -> do
                writeChan (oscCommands oscState) $ SetThreshold (realToFrac db)
                putStrLn $ "OSC: Set threshold = " ++ show db
            _ -> putStrLn "OSC: bad args for /demod/set/threshold"

        "/demod/note/trigger" -> case args of
            [Int32 note, Int32 vel] -> do
                writeChan (oscCommands oscState) $
                    TriggerNote (fromIntegral note) (fromIntegral vel)
                putStrLn $ "OSC: Trigger note=" ++ show note ++ " vel=" ++ show vel
            _ -> putStrLn "OSC: bad args for /demod/note/trigger"

        "/demod/note/off" -> case args of
            [Int32 note] -> do
                writeChan (oscCommands oscState) $ ReleaseNote (fromIntegral note)
                putStrLn $ "OSC: Note off=" ++ show note
            _ -> putStrLn "OSC: bad args for /demod/note/off"

        "/demod/preset/load" -> case args of
            [AsciiString name] -> do
                -- FIX: use ascii_to_string, not `show`.  `show` produces
                -- Haskell repr including the Ascii constructor and quotes.
                let nameStr = ascii_to_string name
                writeChan (oscCommands oscState) $ LoadPreset nameStr
                putStrLn $ "OSC: Load preset = " ++ nameStr
            _ -> putStrLn "OSC: bad args for /demod/preset/load"

        "/demod/status" -> do
            st <- readTVarIO state
            putStrLn $ "OSC: Status — " ++ show (length (currentNotes st))
                     ++ " notes active"

        "/demod/debug" -> case args of
            [Int32 val] -> do
                writeChan (oscCommands oscState) $ SetDebug (val /= 0)
                putStrLn $ "OSC: Debug = " ++ show (val /= 0)
            _ -> return ()

        _ -> putStrLn $ "OSC: Unknown address: " ++ addr

-- ─────────────────────────────────────────────────────────────────────────────
-- Command handler
-- ─────────────────────────────────────────────────────────────────────────────

oscCommandHandler :: OscState -> TVar ReactorState -> IO ()
oscCommandHandler oscState state = do
    result <- try $ readChan (oscCommands oscState)
    case result of
        Left (_ :: SomeException) -> return ()
        Right cmd -> do
            applyCommand state cmd
            oscCommandHandler oscState state

applyCommand :: TVar ReactorState -> OscCommand -> IO ()
applyCommand state cmd = case cmd of
    SetBPM bpm ->
        atomically $ modifyTVar' state $ \s -> s { reactorBPM = bpm }
    SetThreshold db ->
        atomically $ modifyTVar' state $ \s -> s { reactorThreshold = db }
    TriggerNote note vel ->
        atomically $ modifyTVar' state $ \s -> s { currentNotes = [(note, vel)] }
    ReleaseNote _ ->
        atomically $ modifyTVar' state $ \s -> s { currentNotes = [] }
    LoadPreset name -> do
        mPreset <- getPresetByName name
        case mPreset of
            Nothing -> putStrLn $ "OSC: Preset not found: " ++ name
            Just p  -> do
                atomically $ modifyTVar' state $ \s ->
                    let newCfg = applyPreset p (config s)
                    in  s { config = newCfg }
                putStrLn $ "OSC: Loaded preset: " ++ name
    GetStatus -> return ()
    SetDebug _ -> return ()

-- ─────────────────────────────────────────────────────────────────────────────
-- Status broadcaster
--
-- Sends /demod/status to 127.0.0.1:(port+1) at 20 Hz.
--
-- FIX: the original used udpServer (which *binds* a port) and then called
-- Fd.sendMessage on it.  A server socket has no default destination so
-- sendMessage would silently fail or error.  The correct approach is an
-- unconnected UDP socket + sendTo with an explicit target SockAddr.
-- The socket is bracketed with `finally close` so it is released even if
-- the thread is killed via Async.cancel.
-- ─────────────────────────────────────────────────────────────────────────────

statusBroadcaster :: Int -> TVar ReactorState -> IO ()
statusBroadcaster port state = do
    let targetPort = port + 1
    sockResult <- try $ do
        sock      <- socket AF_INET Datagram defaultProtocol
        addrInfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just (show targetPort))
        case addrInfos of
            []       -> fail $ "statusBroadcaster: cannot resolve 127.0.0.1:" ++ show targetPort
            (ai : _) -> return (sock, addrAddress ai)
    case sockResult of
        Left (e :: SomeException) ->
            hPutStrLn stderr $ "[OSC] statusBroadcaster: setup failed: " ++ show e
        Right (sock, target) ->
            finally (broadcastLoop sock target) (close sock)
  where
    broadcastLoop sock target = forever $ do
        threadDelay 50000  -- 20 Hz
        st <- readTVarIO state
        let msg   = Message "/demod/status"
                        [ Int32 $ fromIntegral $ length $ currentNotes st
                        , Float $ realToFrac $ reactorBPM st
                        ]
            bytes = BL.toStrict (encodeMessage msg)
        _ <- try (sendTo sock bytes target) :: IO (Either SomeException Int)
        return ()

-- ─────────────────────────────────────────────────────────────────────────────
-- Outbound OSC client (sends note-on/off to the MIDI bridge)
--
-- The target SockAddr is resolved once at creation time so each send avoids
-- a DNS/getaddrinfo call.
-- ─────────────────────────────────────────────────────────────────────────────

data OscClient = OscClient
    { oscTargetHost :: String
    , oscTargetPort :: Int
    , oscSocket     :: Socket         -- always valid; protected by oscConnected
    , oscTarget     :: SockAddr       -- resolved once at construction
    , oscConnected  :: IORef Bool
    }

createOscClient :: String -> Int -> IO OscClient
createOscClient host port = do
    putStrLn $ "Creating OSC client → " ++ host ++ ":" ++ show port
    sock      <- socket AF_INET Datagram defaultProtocol
    setSocketOption sock ReuseAddr 1
    addrInfos <- getAddrInfo Nothing (Just host) (Just (show port))
    case addrInfos of
        [] -> do
            close sock
            fail $ "createOscClient: cannot resolve " ++ host ++ ":" ++ show port
        (ai : _) -> do
            connected <- newIORef True
            putStrLn $ "OSC client ready → " ++ host ++ ":" ++ show port
            return OscClient
                { oscTargetHost = host
                , oscTargetPort = port
                , oscSocket     = sock
                , oscTarget     = addrAddress ai
                , oscConnected  = connected
                }

sendOscMessage :: OscClient -> Message -> IO ()
sendOscMessage client msg = do
    ok <- readIORef (oscConnected client)
    when ok $ do
        let bytes = BL.toStrict (encodeMessage msg)
        result <- try $ sendTo (oscSocket client) bytes (oscTarget client)
                        :: IO (Either SomeException Int)
        case result of
            Left e -> do
                hPutStrLn stderr $ "[OSC] Send error: " ++ show e
                writeIORef (oscConnected client) False
            Right _ -> return ()

sendNoteOn :: OscClient -> Int -> Int -> IO ()
sendNoteOn client note vel = do
    sendOscMessage client $
        Message "/demod/note/trigger"
            [Int32 (fromIntegral note), Int32 (fromIntegral vel)]
    ok <- readIORef (oscConnected client)
    when ok $
        putStrLn $ "[OSC] Note On: " ++ show note ++ " vel=" ++ show vel
                ++ " → " ++ oscTargetHost client ++ ":" ++ show (oscTargetPort client)

sendNoteOff :: OscClient -> Int -> IO ()
sendNoteOff client note = do
    sendOscMessage client $
        Message "/demod/note/off" [Int32 (fromIntegral note)]
    ok <- readIORef (oscConnected client)
    when ok $
        putStrLn $ "[OSC] Note Off: " ++ show note
                ++ " → " ++ oscTargetHost client ++ ":" ++ show (oscTargetPort client)

closeOscClient :: OscClient -> IO ()
closeOscClient client = do
    putStrLn $ "Closing OSC client: " ++ oscTargetHost client
             ++ ":" ++ show (oscTargetPort client)
    writeIORef (oscConnected client) False
    result <- try (close (oscSocket client)) :: IO (Either SomeException ())
    case result of
        Left e  -> hPutStrLn stderr $ "[OSC] Error closing socket: " ++ show e
        Right _ -> return ()
