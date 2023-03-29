{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# HLINT ignore "Use head" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module CircuitSpec where

import Circuit
import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM)
import Data.Bits
import Data.Functor
import Data.List qualified as List
import Data.Vector qualified as Vector
import Data.Word
import GHC.Base
import GHC.Num ((-))
import Relude (show)
import Test.Hspec
import Test.QuickCheck hiding ((.&.))
import Test.QuickCheck qualified as QC

class TestForAll a where
  testForAll :: a -> Expectation

instance TestForAll (Word8 -> Expectation) where
  testForAll f = forM_ [0 .. 255] $ \i -> f i

instance TestForAll (Word8 -> Word8 -> Expectation) where
  testForAll f = forM_ [0 .. 255] $ \i -> testForAll (f i)

instance TestForAll (Cmd -> Expectation) where
  testForAll f = testForAll (\c t -> f $ Cmd c t)

infix 0 -->

(-->) :: Bool -> Expectation -> Expectation
b --> e = when b e

spec :: Spec
spec = do
  describe "(`xor`)" $ do
    it "should be true if a and b are different" $ do
      testForAll $ \(a :: Word8) (b :: Word8) -> do
        a /= b --> ((a, a `xor` b) `shouldNotBe` (a, 0))
    it "should be false if a and b are the same" $ do
      testForAll $ \(a :: Word8) -> do
        let b = a
        (a, a `xor` b) `shouldBe` (a, 0)
  describe "test" $ do
    it "should allways be true if a is false" $ do
      testForAll $ \(b :: Word8) -> do
        let a = 0
        (a, b, test a b) `shouldBe` (a, b, True)
    it "should be true if b is true" $ do
      testForAll $ \(a :: Word8) -> do
        let b = 0xFF
        (a, b, test a b) `shouldBe` (a, b, True)
    it "If a == b then test a b == True" $ do
      testForAll $ \(a :: Word8) -> do
        let b = a
        (a, b, test a b) `shouldBe` (a, b, True)
    it "test a (a .|. b) == True" $ do
      testForAll $ \(a :: Word8) (b' :: Word8) -> do
        let b = a .|. b'
        (a, b, test a b) `shouldBe` (a, b, True)
    it "If b is the compliment of a, and a /= 0 then test a b == False" $ do
      testForAll $ \(a :: Word8) -> do
        let b = complement a
        a /= 0 --> (a, b, test a b) `shouldBe` (a, b, False)
    it "If (a .&. c == 0) then test a (b `xor` c) == test a b" $ do
      QC.property $ \(a :: Word8) (b :: Word8) ->
        testForAll $ \(c :: Word8) -> do
          a
            .&. c
            == 0
            --> (a, b, c, test a (b `xor` c))
            `shouldBe` (a, b, c, test a b)
    it "setting bits in a cant make it true!" $ do
      QC.property $ \(a :: Word8) (b :: Word8) ->
        testForAll $ \(c :: Word8) -> do
          not (test a b) --> (a, b, c, test (a .|. c) b) `shouldBe` (a, b, c, False)
    it "clearing bits in b cant make it true!" $ do
      QC.property $ \(a :: Word8) (b :: Word8) ->
        testForAll $ \(c :: Word8) -> do
          not (test a b) --> (a, b, c, test a (b .&. c)) `shouldBe` (a, b, c, False)
    it "clearing bits in a cant make it false!" $ do
      QC.property $ \(a :: Word8) (b :: Word8) ->
        testForAll $ \(c :: Word8) -> do
          test a b --> (a, b, c, test (a .&. c) b) `shouldBe` (a, b, c, True)
    it "setting bits in b cant make it false!" $ do
      QC.property $ \(a :: Word8) (b :: Word8) ->
        testForAll $ \(c :: Word8) -> do
          test a b --> (a, b, c, test a (b .|. c)) `shouldBe` (a, b, c, True)

  describe "runCmd" $ do
    it "is id on noop" $ do
      testForAll $ \(control :: Word8) (state :: Word8) -> do
        let cmd = Cmd control 0
        (cmd, state, runCmd cmd state) `shouldBe` (cmd, state, state)      
    it "can toggle" $ do
      testForAll $ \(control :: Word8) (target :: Word8) -> do
        let state = control
        let cmd = Cmd {..}
        let expected = state `xor` target
        (control
          .&. target
          == 0)
          --> ((cmd, state, runCmd cmd state) `shouldBe` (cmd, state, expected))
    it "can not toggle" $ do
      testForAll $ \(control :: Word8) (target :: Word8) -> do
        let state = control `xor` 0xFF
        let cmd = Cmd {..}
        control
          .&. target
          == 0
          && control
          /= 0
          --> (cmd, state, runCmd cmd state)
          `shouldBe` (cmd, state, state)
    it "should never do anything else" $ do
      QC.property $ \(control :: Word8) (target :: Word8) ->
        testForAll $ \(state :: Word8) -> do
          let cmd = Cmd {..}
          let expected = state `xor` target
          let actual = runCmd cmd state
          actual /= state --> (cmd, state, actual) `shouldBe` (cmd, state, expected)
    it "is the reverse of runCmd" $
      property $ \(cmd :: Cmd) ->
        testForAll $ \(state :: Word8) -> do
          let actual = runCmd cmd (runCmd cmd state)
          (cmd, state, actual) `shouldBe` (cmd, state, state)
  describe "runCircuit" $ do
    it "is runCmd on singletons" $
      property $ \(cmd :: Cmd) (state :: Word8) ->
        runCircuit (Vector.singleton cmd) state `shouldBe` runCmd cmd state
    it "is id on the empty circuit" $
      property $ \(state :: Word8) ->
        runCircuit mempty state `shouldBe` state
    it "is the reverse of runCircuitReverse" $
      property $ \(circuit :: Circuit) (state :: Word8) ->
        runCircuit circuit (runCircuitReverse circuit state) `shouldBe` state
  describe "encode/decode" $ do
    describe "encodeCommand" $ do
      it "can be decoded" $
        testForAll $
          \(control :: Word8) (target :: Word8) -> canBeDecoded Cmd {..}
    describe "encodeCircuit" $ do
      it "can be decoded" $
        property canBeDecoded'

-- the other way around is not true, because the encoding is not unique, multiple codes will get decoded to same circuit/cmd

canBeDecoded :: Cmd -> Expectation
canBeDecoded cmd = decodeCommand (encodeCommand cmd) `shouldBe` cmd

canBeDecoded' :: Circuit -> Expectation
canBeDecoded' circuit = decodeCircuit (encodeCircuit circuit) `shouldBe` circuit

exampleCmd :: Cmd
exampleCmd =
  Cmd 0x0F 0xF0
