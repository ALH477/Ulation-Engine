module DeMoDNote.OSCSpec where

import Test.Hspec
import Test.QuickCheck
import DeMoDNote.OSC

spec :: Spec
spec = do
    describe "OSC.OscCommand" $ do
        it "SetBPM stores the value" $
            SetBPM 120.0 `shouldBe` SetBPM 120.0
        it "SetThreshold stores the value" $
            SetThreshold (-40.0) `shouldBe` SetThreshold (-40.0)
        it "TriggerNote stores note and velocity" $
            TriggerNote 60 100 `shouldBe` TriggerNote 60 100
        it "ReleaseNote stores the note" $
            ReleaseNote 60 `shouldBe` ReleaseNote 60
        it "LoadPreset stores the name" $
            LoadPreset "jazz" `shouldBe` LoadPreset "jazz"

    describe "OSC message encoding" $ do
        it "encodes note-on message correctly" $ do
            let msg = Message "/demod/note/trigger" [Int32 60, Int32 100]
                addr = messageAddress msg
            addr `shouldBe` "/demod/note/trigger"
        
        it "encodes note-off message correctly" $ do
            let msg = Message "/demod/note/off" [Int32 60]
                addr = messageAddress msg
            addr `shouldBe` "/demod/note/off"
        
        it "encodes set BPM message correctly" $ do
            let msg = Message "/demod/set/bpm" [Float 120.0]
                args = messageDatum msg
            args `shouldBe` [Float 120.0]
        
        it "encodes set threshold message correctly" $ do
            let msg = Message "/demod/set/threshold" [Float (-40.0)]
                args = messageDatum msg
            args `shouldBe` [Float (-40.0)]
    
    describe "OSC argument handling" $ do
        it "extracts Int32 from datum" $ property $ \n ->
            let datum = Int32 (fromIntegral (n :: Int))
            in case datum of
                 Int32 v -> fromIntegral v `shouldBe` (n :: Int)
                 _ -> expectationFailure "Wrong datum type"
        
        it "extracts Float from datum" $ property $ \f ->
            let datum = Float (f :: Float)
            in case datum of
                 Float v -> v `shouldBe` f
                 _ -> expectationFailure "Wrong datum type"
