{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module DeMoDNote.Recording (
    RecordedEvent(..),
    EventType(..),
    Session(..),
    RecordingState(..),
    initRecording,
    startRecording,
    stopRecording,
    toggleRecording,
    recordEvent,
    flushRecording,
    sessionToCsv,
    sessionToProtobuf,
    getRecordingFilename
) where

import Data.Word (Word64, Word8)
import Data.Time (getCurrentTime, UTCTime(..))
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.List (intercalate)
import Control.Concurrent (MVar, newMVar)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Binary (Binary(..), encode)
import Data.Binary.Put (putWord64le, putWord8)
import Data.Binary.Get (Get, getWord64le, getWord8)

data EventType = NoteOn | NoteOff | Detection
    deriving (Show, Eq, Generic)

instance Binary EventType where
    put NoteOn = putWord8 0
    put NoteOff = putWord8 1
    put Detection = putWord8 2
    get = do
        tag <- getWord8
        case tag of
            0 -> return NoteOn
            1 -> return NoteOff
            _ -> return Detection

data RecordedEvent = RecordedEvent
    { timestamp      :: !Word64
    , eventType     :: !EventType
    , note          :: !Word8
    , velocity      :: !Word8
    , confidence    :: !Float
    , tuningCents   :: !Float
    } deriving (Show, Generic)

instance Binary RecordedEvent where
    put (RecordedEvent t et n v conf tc) = do
        putWord64le t
        put et
        putWord8 n
        putWord8 v
        put conf
        put tc
    get = RecordedEvent <$> getWord64le <*> get <*> getWord8 <*> getWord8 <*> get <*> get

data Session = Session
    { sessionId      :: !T.Text
    , sessionStart   :: !UTCTime
    , preset          :: !T.Text
    , events         :: ![RecordedEvent]
    } deriving (Show, Generic)

instance Binary Session where
  put (Session sid ss presetVal evts) = do
    put sid
    -- Store full POSIX timestamp with microsecond precision
    let posixTs = round (utcTimeToPOSIXSeconds ss * 1e6) :: Word64
    put posixTs
    put presetVal
    put evts
  get = Session <$> get <*> (posixMicrosToUTCTime <$> (get :: Get Word64)) <*> get <*> get
    where
      posixMicrosToUTCTime :: Word64 -> UTCTime
      posixMicrosToUTCTime us = 
        let secs = fromIntegral (us `div` 1000000)
            micros = fromIntegral (us `mod` 1000000) / 1e6
        in posixSecondsToUTCTime (secs + micros)

data RecordingState = RecordingState
    { recEnabled     :: !Bool
    , recEvents     :: ![RecordedEvent]
    , recSessionId  :: !T.Text
    , recStartTime  :: !UTCTime
    , recPreset     :: !T.Text
    , recMVar       :: !(MVar ())
    }

initRecording :: T.Text -> IO RecordingState
initRecording presetName = do
    now <- getCurrentTime
    newSessionId <- T.pack . formatTime defaultTimeLocale "%Y%m%d-%H%M%S" <$> getCurrentTime
    mv <- newMVar ()
    return $ RecordingState
        { recEnabled = False
        , recEvents = []
        , recSessionId = newSessionId
        , recStartTime = now
        , recPreset = presetName
        , recMVar = mv
        }

startRecording :: RecordingState -> IO RecordingState
startRecording rec = do
    now <- getCurrentTime
    return $ rec
        { recEnabled = True
        , recEvents = []
        , recStartTime = now
        }

stopRecording :: RecordingState -> IO (Session, RecordingState)
stopRecording rec = do
    let session = Session
            { sessionId = recSessionId rec
            , sessionStart = recStartTime rec
            , preset = recPreset rec
            , events = recEvents rec
            }
    return (session, rec { recEnabled = False, recEvents = [] })

toggleRecording :: RecordingState -> IO RecordingState
toggleRecording rec
    | recEnabled rec = snd <$> stopRecording rec
    | otherwise      = startRecording rec

getTimestamp :: UTCTime -> Word64
getTimestamp ut = floor $ utcTimeToPOSIXSeconds ut * 1000000

recordEvent :: RecordingState -> EventType -> Int -> Int -> Float -> Float -> IO RecordingState
recordEvent rec et noteVal vel conf tc = do
    if recEnabled rec
        then do
            now <- getCurrentTime
            let evt = RecordedEvent 
                    { timestamp = getTimestamp now - getTimestamp (recStartTime rec)
                    , eventType = et
                    , note = fromIntegral noteVal
                    , velocity = fromIntegral vel
                    , confidence = conf
                    , tuningCents = tc
                    }
            return rec { recEvents = evt : recEvents rec }
        else return rec

flushRecording :: RecordingState -> FilePath -> IO ()
flushRecording rec path = do
    let session = Session
            { sessionId = recSessionId rec
            , sessionStart = recStartTime rec
            , preset = recPreset rec
            , events = recEvents rec
            }
    BSL.writeFile (path ++ ".protobuf") (sessionToProtobuf session)
    writeFile (path ++ ".csv") (sessionToCsv session)
    return ()

sessionToProtobuf :: Session -> BSL.ByteString
sessionToProtobuf = encode

sessionToCsv :: Session -> String
sessionToCsv session = unlines $ header : map eventToCsv (events session)
  where
    header = "timestamp_us,event_type,note,velocity,confidence,tuning_cents"
    eventToCsv e = intercalate ","
        [ show (timestamp e)
        , show (eventType e)
        , show (note e)
        , show (velocity e)
        , show (confidence e)
        , show (tuningCents e)
        ]

getRecordingFilename :: IO String
getRecordingFilename = do
    now <- getCurrentTime
    return $ formatTime defaultTimeLocale "recording-%Y%m%d-%H%M%S" now
