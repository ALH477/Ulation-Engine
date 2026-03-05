{-# LANGUAGE OverloadedStrings #-}

module DeMoDNote.Monitor where

import Web.Scotty
import Control.Concurrent.STM
import Data.Aeson (object, (.=))
import DeMoDNote.Types

startMonitor :: Int -> TVar ReactorState -> IO ()
startMonitor port state = scotty port $ do
  get "/status" $ do
    st <- liftIO $ readTVarIO state
    json $ object
        [ "notes"       .= currentNotes st
        , "confidence"  .= detectionConfidence st
        , "latency_ms"  .= detectionLatency st
        , "bpm"         .= reactorBPM st
        , "jack_status" .= show (jackStatus st)
        , "note_state"  .= show (noteStateMach st)
        , "tuning_note"  .= detectedTuningNote st
        , "tuning_cents" .= detectedTuningCents st
        , "tuning_ok"    .= detectedTuningInTune st
        , "waveform"    .= take 64 (latestWaveform st)
        ]
