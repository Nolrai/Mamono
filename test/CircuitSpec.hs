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
import Data.Vector qualified as Vector
import Data.Word
import GHC.Base
import GHC.Num ((-))
import Test.Hspec
import Test.QuickCheck hiding ((.&.))

spec :: Spec
spec = do
  describe "runCmd" $ do
    it "can toggle" $ do
      property $ \(c :: Word8) (t :: Word8) -> do
        let control = fromWord8 c
        let target = fromWord8 t
        let state = control
        let cmd = Cmd control target
        let expected = state .^. target
        runCmd cmd state `shouldBe` expected
    it "can not toggle" $ do
      property $ \(c :: Word8) (t :: Word8) -> c /= 0 ==> do
        let control = fromWord8 c
        let target = fromWord8 t
        let state = control .^. true (0, 7)
        let cmd = Cmd control target
        let expected = state .^. target
        c /= 0 ==> runCmd cmd state `shouldBe` state
    it "should never do anything else" $ do
      property $ \(c :: Word8) (t :: Word8) (s :: Word8) -> do
        let control = fromWord8 c
        let target = fromWord8 t
        let state = fromWord8 s
        let cmd = Cmd control target
        let expected = state .^. target
        let actual = runCmd cmd state
        actual /= state ==> actual `shouldBe` expected

    it "is the reverse of runCmd" $
      property $ \(cmd :: Cmd) (state :: BitArray Word8) ->
        runCmd cmd (runCmd cmd state) `shouldBe` state
  describe "runCircuit" $ do
    it "is runCmd on singletons" $
      property $ \(cmd :: Cmd) (state :: BitArray Word8) ->
        runCircuit (Vector.singleton cmd) state `shouldBe` runCmd cmd state
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
        property canBeDecoded
    describe "encodeCircuit" $ do
      it "can be decoded" $
        property canBeDecoded'
-- the other way around is not true, because the encoding is not unique, multiple codes will get decoded to same circuit/cmd

  describe "toWord8" $ do
    it "is the reverse of fromWord8" $
      property $ \(x :: Word8) ->
        toWord8 (fromWord8 x) `shouldBe` x
    it "is an embedding" $
      property $ \(x :: Word8) y ->
        x /= y ==> fromWord8 x `shouldNotBe` fromWord8 y

canBeDecoded :: Cmd -> Expectation
canBeDecoded cmd = decodeCommand (encodeCommand cmd) `shouldBe` cmd

canBeDecoded' :: Circuit -> Expectation
canBeDecoded' circuit = decodeCircuit (encodeCircuit circuit) `shouldBe` circuit

exampleCmd :: Cmd
exampleCmd =
  Cmd
    (fromWord8 0x0F)
    (fromWord8 0xF0)
