{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module CircuitSpec where

import Circuit
import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM)
import Data.Array.BitArray
import Data.Functor
import Data.List qualified as List
import Data.Word
import GHC.Base
import GHC.Num ((-))
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "runCmd" $ do
    it "can be toggled" $ do
      let control = fromList $ (== 1) <$> [1, 0, 0, 0] <> [0, 0, 0, 0]
      let target = fromList $ (== 1) <$> [0, 0, 0, 0] <> [0, 0, 0, 1]
      let state = fromList $ (== 1) <$> [1, 0, 0, 0] <> [0, 0, 0, 0]
      let cmd = Toggle (ToggleCmd control target)
      let expected = fromList $ (== 1) <$> [1, 0, 0, 0] <> [0, 0, 0, 0]
      runCmd cmd state `shouldBe` expected
    it "can be rotated" $ do
      let state = fromList $ (== 1) <$> [1, 0, 0, 0] <> [0, 0, 0, 1]
      let cmd = Rotate (RotateCmd 1)
      let expected = fromList $ (== 1) <$> [0, 0, 0, 0] <> [0, 0, 1, 1]
      runCmd cmd state `shouldBe` expected
    it "can be swapped" $ do
      let state = fromList $ (== 1) <$> [1, 0, 0, 0] <> [0, 0, 0, 0]
      let cmd = Swap (SwapCmd 0 7)
      let expected = fromList $ (== 1) <$> [0, 0, 0, 0] <> [0, 0, 0, 1]
      runCmd cmd state `shouldBe` expected
  describe "runCmdReverse" $ do
    it "can be toggled" $ do
      let control = fromList $ (== 1) <$> [1, 0, 0, 0] <> [0, 0, 0, 0]
      let target = fromList $ (== 1) <$> [0, 0, 0, 0] <> [0, 0, 0, 1]
      let state = fromList $ (== 1) <$> [1, 0, 0, 0] <> [0, 0, 0, 0]
      let cmd = Toggle (ToggleCmd control target)
      let expected = fromList $ (== 1) <$> [1, 0, 0, 0] <> [0, 0, 0, 0]
      runCmdReverse cmd state `shouldBe` expected
    it "can be rotated" $ do
      let state = fromList $ (== 1) <$> [0, 0, 0, 0] <> [0, 0, 1, 1]
      let cmd = Rotate (RotateCmd 1)
      let expected = fromList $ (== 1) <$> [1, 0, 0, 0] <> [0, 0, 0, 1]
      runCmdReverse cmd state `shouldBe` expected
    it "can be swapped" $ do
      let state = fromList $ (== 1) <$> [0, 0, 0, 0] <> [0, 0, 0, 1]
      let cmd = Swap (SwapCmd 0 7)
      let expected = fromList $ (== 1) <$> [1, 0, 0, 0] <> [0, 0, 0, 0]
      runCmdReverse cmd state `shouldBe` expected
    it "is the reverse of runCmd" $
      property $ \(cmd :: Cmd) (state :: BitArray Word8) ->
        runCmdReverse cmd (runCmd cmd state) `shouldBe` state
  describe "runCircuit" $ do
    it "is id on the empty circuit" $
      property $ \(state :: BitArray Word8) ->
        runCircuit mempty state `shouldBe` state
    it "is the reverse of runCircuitReverse" $
      property $ \(circuit :: Circuit) (state :: BitArray Word8) ->
        runCircuit circuit (runCircuitReverse circuit state) `shouldBe` state
  describe "bitArray" $ do
    it "can be constructed from a word8 and converted back" $
      property $ \(x :: Word8) ->
        toWord8 (fromWord8 x) `shouldBe` x
  describe "encode/decode" $ do
    describe "encodeCommand" $ do
      it "can be decoded" $
        exampleCmd `forM_` canBeDecoded
      it "can be decoded" $
        property canBeDecoded
    describe "encodeCircuit" $ do
      it "can be decoded" $
        property canBeDecoded'

-- the other way around is not true, because the encoding is not unique, multiple codes will get decoded to same circuit/cmd

canBeDecoded :: Cmd -> Expectation
canBeDecoded cmd = decodeCommand (encodeCommand cmd) `shouldBe` cmd

canBeDecoded' :: Circuit -> Expectation
canBeDecoded' circuit = decodeCircuit (encodeCircuit circuit) `shouldBe` circuit

exampleCmd :: [Cmd]
exampleCmd =
  [ Swap (SwapCmd 0 1),
    Swap (SwapCmd 7 0),
    Rotate (RotateCmd 1)
  ]
