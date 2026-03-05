{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : DeMoDNote.Export
-- Description : Export recording sessions to various formats
-- 
-- This module provides functionality to convert DeMoD-Note recording sessions
-- to standard formats like MIDI, MusicXML, and JSON for use with other tools.

module DeMoDNote.Export (
    -- * MIDI Export
    MidiExportConfig(..),
    defaultMidiConfig,
    sessionToMidi,
    writeMidiFile,
    
    -- * JSON Export
    sessionToJson,
    writeJsonFile,
    
    -- * MusicXML Export  
    MusicXmlConfig(..),
    defaultMusicXmlConfig,
    sessionToMusicXml,
    writeMusicXmlFile,
    
    -- * Generic Export
    ExportFormat(..),
    exportSession,
    exportSessionToFile
) where

import DeMoDNote.Recording (Session(..), RecordedEvent(..), EventType(..))

import Data.Word (Word8, Word16, Word32, Word64)
import Data.Int (Int16, Int32)
import Data.Bits (shiftR, (.&.), (.|.))
import Data.List (sortOn)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Text as T
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as AP
import Data.Aeson ((.=))

-- =============================================================================
-- Data Types
-- =============================================================================

-- | Export format options
data ExportFormat 
    = ExportMidi FilePath
    | ExportJson FilePath
    | ExportMusicXml FilePath
    deriving (Show, Eq)

-- | Configuration for MIDI export
data MidiExportConfig = MidiExportConfig
    { midiPpq        :: !Word16     -- ^ Pulses per quarter note (default: 480)
    , midiTempo      :: !Int        -- ^ Tempo in BPM (default: 120)
    , midiChannel    :: !Word8      -- ^ MIDI channel (0-15, default: 0)
    , midiProgram    :: !Word8      -- ^ MIDI program/instrument (0-127, default: 0)
    , midiIncludePitchBend :: !Bool -- ^ Include pitch bend from Detection events
    , midiPitchBendRange :: !Word8  -- ^ Pitch bend range in semitones (default: 2)
    } deriving (Show, Eq)

-- | Default MIDI export configuration
defaultMidiConfig :: MidiExportConfig
defaultMidiConfig = MidiExportConfig
    { midiPpq = 480
    , midiTempo = 120
    , midiChannel = 0
    , midiProgram = 0
    , midiIncludePitchBend = True
    , midiPitchBendRange = 2
    }

-- | Configuration for MusicXML export
data MusicXmlConfig = MusicXmlConfig
    { xmlTitle      :: !String
    , xmlComposer   :: !String
    , xmlInstrument :: !String
    } deriving (Show, Eq)

-- | Default MusicXML configuration
defaultMusicXmlConfig :: MusicXmlConfig
defaultMusicXmlConfig = MusicXmlConfig
    { xmlTitle = "DeMoD-Note Recording"
    , xmlComposer = ""
    , xmlInstrument = "Guitar"
    }

-- =============================================================================
-- MIDI Export
-- =============================================================================

-- | MIDI file header chunk
data MidiHeader = MidiHeader
    { hFormat    :: !Word16  -- 0=single track, 1=multi-track
    , hNumTracks :: !Word16
    , hDivision  :: !Word16  -- PPQ
    }

-- | MIDI track with events
data MidiTrack = MidiTrack
    { tEvents :: [(Word32, MidiEvent)]  -- (delta_ticks, event)
    }

-- | MIDI event types
data MidiEvent
    = NoteOnEvent !Word8 !Word8 !Word8      -- channel, note, velocity
    | NoteOffEvent !Word8 !Word8 !Word8     -- channel, note, velocity
    | ProgramChangeEvent !Word8 !Word8      -- channel, program
    | PitchBendEvent !Word8 !Int16          -- channel, bend (-8192 to 8191)
    | TempoEvent !Int32                      -- microseconds per quarter note
    | EndOfTrackEvent

-- | Convert session to MIDI file bytes
sessionToMidi :: MidiExportConfig -> Session -> BSL.ByteString
sessionToMidi config session =
    let header = MidiHeader 0 1 (midiPpq config)
        track = buildMidiTrack config session
    in encodeMidiFile header [track]

-- | Write MIDI file to disk
writeMidiFile :: MidiExportConfig -> Session -> FilePath -> IO ()
writeMidiFile config session path = 
    BSL.writeFile path (sessionToMidi config session)

-- | Build a MIDI track from session events
buildMidiTrack :: MidiExportConfig -> Session -> MidiTrack
buildMidiTrack config@MidiExportConfig{..} session =
    let -- Sort events by timestamp
        sortedEvents = sortOn timestamp (events session)
        
        -- Convert timestamps from microseconds to ticks
        usToTicks us = fromIntegral $ 
            (us * fromIntegral midiPpq * fromIntegral midiTempo) `div` 60000000
        
        -- Build event list with delta times
        midiEvents = sessionEventsToMidi config sortedEvents usToTicks
        
        -- Add tempo event at start and end-of-track at end
        tempoUs = fromIntegral (60000000 `div` midiTempo :: Int) :: Int32
        allEvents = (0, TempoEvent tempoUs) 
                  : (0, ProgramChangeEvent midiChannel midiProgram)
                  : midiEvents
                  ++ [(0, EndOfTrackEvent)]
        
    in MidiTrack allEvents

-- | Convert session events to MIDI events
sessionEventsToMidi :: MidiExportConfig -> [RecordedEvent] -> (Word64 -> Word32) -> [(Word32, MidiEvent)]
sessionEventsToMidi MidiExportConfig{..} evts usToTicks = 
    go evts 0 []
  where
    go [] _ acc = reverse acc
    go (e:es) lastTick acc =
        let tick = usToTicks (timestamp e)
            delta = if tick >= lastTick then tick - lastTick else 0
            midiEvt = case eventType e of
                NoteOn -> NoteOnEvent midiChannel (note e) (velocity e)
                NoteOff -> NoteOffEvent midiChannel (note e) (velocity e)
                Detection -> 
                    if midiIncludePitchBend 
                    then pitchBendFromCents midiChannel (tuningCents e)
                    else NoteOnEvent midiChannel (note e) (velocity e)
        in go es tick ((delta, midiEvt) : acc)

-- | Convert cents deviation to pitch bend value
-- Range is -8192 to 8191, where 0 = center
-- Pitch bend range determines how many semitones correspond to full bend
pitchBendFromCents :: Word8 -> Float -> MidiEvent
pitchBendFromCents channel cents =
    let -- Convert cents to pitch bend units
        -- Full range (±8192) = ±pitchBendRange semitones
        -- 1 semitone = 100 cents
        defaultRange = 2 :: Float  -- Default 2 semitones
        bendValue = round (cents * 8192 / (defaultRange * 100)) :: Int
        clamped = max (-8192) (min 8191 bendValue) :: Int
    in PitchBendEvent channel (fromIntegral clamped :: Int16)

-- | Encode complete MIDI file
encodeMidiFile :: MidiHeader -> [MidiTrack] -> BSL.ByteString
encodeMidiFile header tracks = 
    BB.toLazyByteString $ encodeHeader header <> mconcat (map encodeTrack tracks)

-- | Encode MIDI header chunk
encodeHeader :: MidiHeader -> BB.Builder
encodeHeader MidiHeader{..} =
    BB.string7 "MThd" <>
    BB.word32BE 6 <>
    BB.word16BE hFormat <>
    BB.word16BE hNumTracks <>
    BB.word16BE hDivision

-- | Encode MIDI track chunk
encodeTrack :: MidiTrack -> BB.Builder
encodeTrack MidiTrack{..} =
    let eventData = mconcat $ map encodeEvent tEvents
        trackLen = fromIntegral (BSL.length $ BB.toLazyByteString eventData)
    in BB.string7 "MTrk" <>
       BB.word32BE trackLen <>
       eventData

-- | Encode a single MIDI event with variable-length delta time
encodeEvent :: (Word32, MidiEvent) -> BB.Builder
encodeEvent (delta, evt) =
    encodeVarLength delta <>
    encodeMidiEvent evt

-- | Encode variable-length quantity
encodeVarLength :: Word32 -> BB.Builder
encodeVarLength n = encodeVarLengthBytes n
  where
    encodeVarLengthBytes x
        | x < 128 = BB.word8 (fromIntegral x)
        | x < 16384 = 
            BB.word8 (fromIntegral ((x `shiftR` 7) .|. 0x80)) <>
            BB.word8 (fromIntegral (x .&. 0x7F))
        | x < 2097152 =
            BB.word8 (fromIntegral ((x `shiftR` 14) .|. 0x80)) <>
            BB.word8 (fromIntegral (((x `shiftR` 7) .&. 0x7F) .|. 0x80)) <>
            BB.word8 (fromIntegral (x .&. 0x7F))
        | otherwise =
            BB.word8 (fromIntegral ((x `shiftR` 21) .|. 0x80)) <>
            BB.word8 (fromIntegral (((x `shiftR` 14) .&. 0x7F) .|. 0x80)) <>
            BB.word8 (fromIntegral (((x `shiftR` 7) .&. 0x7F) .|. 0x80)) <>
            BB.word8 (fromIntegral (x .&. 0x7F))

-- | Encode MIDI event data
encodeMidiEvent :: MidiEvent -> BB.Builder
encodeMidiEvent (NoteOnEvent ch n v) =
    BB.word8 (0x90 .|. (ch .&. 0x0F)) <>
    BB.word8 (n .&. 0x7F) <>
    BB.word8 (v .&. 0x7F)
encodeMidiEvent (NoteOffEvent ch n v) =
    BB.word8 (0x80 .|. (ch .&. 0x0F)) <>
    BB.word8 (n .&. 0x7F) <>
    BB.word8 (v .&. 0x7F)
encodeMidiEvent (ProgramChangeEvent ch pgm) =
    BB.word8 (0xC0 .|. (ch .&. 0x0F)) <>
    BB.word8 (pgm .&. 0x7F)
encodeMidiEvent (PitchBendEvent ch val) =
    -- Pitch bend: 14-bit value centered at 8192
    -- val is -8192 to 8191, so add 8192 to get 0-16383
    let centered = fromIntegral val + 8192 :: Int
        lsb = fromIntegral (centered .&. 0x7F) :: Word8
        msb = fromIntegral ((centered `shiftR` 7) .&. 0x7F) :: Word8
    in BB.word8 (0xE0 .|. (ch .&. 0x0F)) <>
       BB.word8 lsb <>
       BB.word8 msb
encodeMidiEvent (TempoEvent us) =
    BB.word8 0xFF <> BB.word8 0x51 <> BB.word8 3 <>
    BB.word8 (fromIntegral ((us `shiftR` 16) .&. 0xFF)) <>
    BB.word8 (fromIntegral ((us `shiftR` 8) .&. 0xFF)) <>
    BB.word8 (fromIntegral (us .&. 0xFF))
encodeMidiEvent EndOfTrackEvent =
    BB.word8 0xFF <> BB.word8 0x2F <> BB.word8 0

-- =============================================================================
-- JSON Export
-- =============================================================================

-- | JSON representation of session
data SessionJson = SessionJson
    { jsonSessionId    :: !T.Text
    , jsonSessionStart :: !T.Text
    , jsonPreset       :: !T.Text
    , jsonEvents       :: ![EventJson]
    }

data EventJson = EventJson
    { jsonTimestamp   :: !Word64
    , jsonEventType   :: !T.Text
    , jsonNote        :: !Word8
    , jsonVelocity    :: !Word8
    , jsonConfidence  :: !Float
    , jsonTuningCents :: !Float
    }

instance A.ToJSON EventJson where
    toJSON EventJson{..} = A.object
        [ "timestamp" .= jsonTimestamp
        , "event_type" .= jsonEventType
        , "note" .= jsonNote
        , "velocity" .= jsonVelocity
        , "confidence" .= jsonConfidence
        , "tuning_cents" .= jsonTuningCents
        ]

instance A.ToJSON SessionJson where
    toJSON SessionJson{..} = A.object
        [ "session_id" .= jsonSessionId
        , "session_start" .= jsonSessionStart
        , "preset" .= jsonPreset
        , "events" .= jsonEvents
        ]

-- | Convert session to JSON bytes
sessionToJson :: Session -> BSL.ByteString
sessionToJson session =
    let startStr = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" (sessionStart session)
        eventsJson = map eventToJson (events session)
        sessionJson = SessionJson
            { jsonSessionId = sessionId session
            , jsonSessionStart = startStr
            , jsonPreset = preset session
            , jsonEvents = eventsJson
            }
    in AP.encodePretty' AP.defConfig { AP.confIndent = AP.Spaces 2 } sessionJson

-- | Convert single event to JSON
eventToJson :: RecordedEvent -> EventJson
eventToJson e = EventJson
    { jsonTimestamp = timestamp e
    , jsonEventType = eventTypeToText (eventType e)
    , jsonNote = note e
    , jsonVelocity = velocity e
    , jsonConfidence = confidence e
    , jsonTuningCents = tuningCents e
    }
  where
    eventTypeToText NoteOn = "note_on"
    eventTypeToText NoteOff = "note_off"
    eventTypeToText Detection = "detection"

-- | Write JSON file to disk
writeJsonFile :: Session -> FilePath -> IO ()
writeJsonFile session path = BSL.writeFile path (sessionToJson session)

-- =============================================================================
-- MusicXML Export
-- =============================================================================

-- | Convert session to MusicXML bytes
sessionToMusicXml :: MusicXmlConfig -> Session -> BSL.ByteString
sessionToMusicXml config session =
    let noteGroups = groupNoteEvents (events session)
        -- Default 120 BPM; a richer implementation would parse tempo from events.
        bpm = 120 :: Int
    in BB.toLazyByteString $ buildMusicXml config bpm noteGroups

-- | Write MusicXML file to disk
writeMusicXmlFile :: MusicXmlConfig -> Session -> FilePath -> IO ()
writeMusicXmlFile config session path = 
    BSL.writeFile path (sessionToMusicXml config session)

-- | Group consecutive note events (NoteOn followed by NoteOff)
groupNoteEvents :: [RecordedEvent] -> [(RecordedEvent, Maybe RecordedEvent)]
groupNoteEvents evts = go (sortOn timestamp evts) []
  where
    go [] acc = reverse acc
    go (e:es) acc = case eventType e of
        NoteOn -> 
            let matchingOff = findNoteOff (note e) es
                remaining = case matchingOff of
                    Just off -> filter (\x -> timestamp x /= timestamp off) es
                    Nothing -> es
            in go remaining ((e, matchingOff) : acc)
        _ -> go es acc
    
    findNoteOff _ [] = Nothing
    findNoteOff n (e:es)
        | eventType e == NoteOff && note e == n = Just e
        | otherwise = findNoteOff n es

-- | Build MusicXML document
buildMusicXml :: MusicXmlConfig -> Int -> [(RecordedEvent, Maybe RecordedEvent)] -> BB.Builder
buildMusicXml MusicXmlConfig{..} bpm notes =
    BB.string7 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" <>
    BB.string7 "<!DOCTYPE score-partwise PUBLIC \"-//Recordare//DTD MusicXML 4.0//EN\" \"http://www.musicxml.org/dtds/partwise.dtd\">\n" <>
    BB.string7 "<score-partwise version=\"4.0\">\n" <>
    BB.string7 "  <work><work-title>" <> BB.string7 xmlTitle <> BB.string7 "</work-title></work>\n" <>
    (if null xmlComposer
     then mempty
     else BB.string7 "  <identification><creator type=\"composer\">" <>
          BB.string7 xmlComposer <> BB.string7 "</creator></identification>\n") <>
    BB.string7 "  <part-list>\n" <>
    BB.string7 "    <score-part id=\"P1\"><part-name>" <> BB.string7 xmlInstrument <> BB.string7 "</part-name></score-part>\n" <>
    BB.string7 "  </part-list>\n" <>
    BB.string7 "  <part id=\"P1\">\n" <>
    buildMeasure 1 bpm notes <>
    BB.string7 "  </part>\n" <>
    BB.string7 "</score-partwise>\n"

-- | Build a single measure
buildMeasure :: Int -> Int -> [(RecordedEvent, Maybe RecordedEvent)] -> BB.Builder
buildMeasure num bpm notes =
    BB.string7 "    <measure number=\"" <> BB.string7 (show num) <> BB.string7 "\">\n" <>
    BB.string7 "      <attributes>\n" <>
    BB.string7 "        <divisions>480</divisions>\n" <>
    BB.string7 "        <time><beats>4</beats><beat-type>4</beat-type></time>\n" <>
    BB.string7 "        <clef><sign>G</sign><line>2</line></clef>\n" <>
    BB.string7 "      </attributes>\n" <>
    mconcat (map (buildNote bpm) notes) <>
    BB.string7 "    </measure>\n"

-- | Build a single note element
-- divisions = 480 ticks per quarter note (declared in <divisions>).
-- duration_us * bpm / 60_000_000 gives duration in quarter notes;
-- multiply by 480 to get ticks.
buildNote :: Int -> (RecordedEvent, Maybe RecordedEvent) -> BB.Builder
buildNote bpm (onEvt, mOffEvt) =
    let midiNote = fromIntegral (note onEvt) :: Int
        (step, alter, octave) = midiToMusicXmlPitch midiNote
        durationUs = case mOffEvt of
            Just off -> timestamp off - timestamp onEvt
            Nothing  -> 500000  -- Default: half a second (~quarter note @ 120 BPM)
        -- Convert microseconds → MusicXML ticks at 480 PPQ
        -- ticks = duration_us * bpm * 480 / 60_000_000
        ticksRaw  = fromIntegral durationUs * fromIntegral bpm * 480
                    `div` (60000000 :: Word64)
        -- Clamp: minimum 1 tick (grace note floor), maximum 1920 (whole note)
        divisions = fromIntegral (max 1 (min 1920 ticksRaw)) :: Word64
    in BB.string7 "      <note>\n" <>
       BB.string7 "        <pitch>\n" <>
       BB.string7 "          <step>" <> BB.string7 [step] <> BB.string7 "</step>\n" <>
       (if alter > 0
        then BB.string7 "          <alter>" <> BB.string7 (show alter) <> BB.string7 "</alter>\n"
        else mempty) <>
       BB.string7 "          <octave>" <> BB.string7 (show octave) <> BB.string7 "</octave>\n" <>
       BB.string7 "        </pitch>\n" <>
       BB.string7 "        <duration>" <> BB.string7 (show divisions) <> BB.string7 "</duration>\n" <>
       BB.string7 "      </note>\n"

-- | Convert MIDI note number to MusicXML pitch components
midiToMusicXmlPitch :: Int -> (Char, Int, Int)
midiToMusicXmlPitch midi =
    let octave = midi `div` 12 - 1
        semitoneInOctave = midi `mod` 12
        
        -- Find the natural note and alteration
        (step, alter) = findNatural semitoneInOctave
        
    in (step, alter, octave)
  where
    findNatural s = 
        case s of
            0  -> ('C', 0)
            1  -> ('C', 1)   -- C#
            2  -> ('D', 0)
            3  -> ('D', 1)   -- D#
            4  -> ('E', 0)
            5  -> ('F', 0)
            6  -> ('F', 1)   -- F#
            7  -> ('G', 0)
            8  -> ('G', 1)   -- G#
            9  -> ('A', 0)
            10 -> ('A', 1)   -- A#
            11 -> ('B', 0)
            _  -> ('C', 0)

-- =============================================================================
-- Generic Export
-- =============================================================================

-- | Export session to specified format
exportSession :: ExportFormat -> Session -> BSL.ByteString
exportSession (ExportMidi _) session = sessionToMidi defaultMidiConfig session
exportSession (ExportJson _) session = sessionToJson session
exportSession (ExportMusicXml _) session = sessionToMusicXml defaultMusicXmlConfig session

-- | Export session to file
exportSessionToFile :: ExportFormat -> Session -> IO ()
exportSessionToFile (ExportMidi path) session = writeMidiFile defaultMidiConfig session path
exportSessionToFile (ExportJson path) session = writeJsonFile session path
exportSessionToFile (ExportMusicXml path) session = writeMusicXmlFile defaultMusicXmlConfig session path