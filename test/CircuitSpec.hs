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
import Data.Array.BitArray as BitArray
import Data.Functor
import Data.List qualified as List
import Data.Vector qualified as Vector
import Data.Word
import GHC.Base
import GHC.Num ((-))
import Test.Hspec
import Test.QuickCheck hiding ((.&.))
import qualified Test.QuickCheck as QC

class TestForAll a where
  testForAll :: a -> Expectation 

instance TestForAll (Word8 -> Expectation) where
  testForAll f = forM_ [0..255] $ \i -> f i

instance TestForAll (Word8 ->  Word8 -> Expectation) where
  testForAll f = forM_ [0..255] $ \i -> testForAll (f i)

instance TestForAll (Cmd -> Expectation) where
  testForAll f = testForAll (\ c t -> f $ Cmd (fromWord8 c) (fromWord8 t))

infix 0 -->

(-->) :: Bool -> Expectation -> Expectation
b --> e = when b e

spec :: Spec
spec = do
  describe "(.^.)" $ do
    it "should be true if a and b are different" $ do
      testForAll $ \ (a :: Word8) (b :: Word8) -> do
        let a' = fromWord8 a
        let b' = fromWord8 b
        a /= b --> 
          (a' .^. b') `shouldNotBe` fromWord8 0
    it "should be false if a and b are the same" $ do
      testForAll $ \ (a :: Word8) -> do
        let a' = fromWord8 a
        let b' = a'
        (a' .^. b') `shouldBe` fromWord8 0
  describe "test" $ do
    it "should allways be true if a is false" $ do
      testForAll $ \ (b :: Word8) -> do
        let a' = false (0, 7)
        let b' = fromWord8 b
        test a' b' `shouldBe` True
    it "should be true if b is true" $ do
      testForAll $ \ (a :: Word8) -> do
        let a' = fromWord8 a
        let b' = true (0, 7)
        test a' b' `shouldBe` True
    it "If a == b then test a b == True" $ do
      testForAll $ \ (a :: Word8) -> do
        let a' = fromWord8 a
        let b' = a'
        test a' b' `shouldBe` True
    it "test a (a .|. b) == True" $ do
      testForAll $ \ (a :: Word8) (b :: Word8) -> do
        let a' = fromWord8 a
        let b' = a' .|. fromWord8 b
        test a' b' `shouldBe` True
    it "If b is the compliment of a, and a /= 0 then test a b == False" $ do
      testForAll $ \ (a :: Word8) -> do
        let a' = fromWord8 a
        let b' = BitArray.map not a'
        a /= 0 --> test a' b' `shouldBe` False
    it "If (a .&. c == false (0, 7)) then test a (b .^. c) == test a b" $ do
      QC.property $ \ (a :: Word8) (b :: Word8) -> 
        testForAll $ \ (c :: Word8) ->
          do
          let a' = fromWord8 a
          let b' = fromWord8 b
          let c' = fromWord8 c
          a' .&. c' == false (0, 7) -->
            test a' (b' .^. c') `shouldBe` test a' b'
    it "setting bits in a cant make it true!" $ do
       QC.property $ \ (a :: Word8) (b :: Word8) -> 
        testForAll $ \ (c :: Word8) -> do
        let a' = fromWord8 a
        let b' = fromWord8 b
        let c' = fromWord8 c
        not (test a' b') --> test (a' .|. c') b' `shouldBe` False
    it "clearing bits in b cant make it true!" $ do
      QC.property $ \ (a :: Word8) (b :: Word8) -> 
        testForAll $ \ (c :: Word8) -> do
        let a' = fromWord8 a
        let b' = fromWord8 b
        let c' = fromWord8 c
        not (test a' b') --> test a' (b' .&. c') `shouldBe` False
    it "clearing bits in a cant make it false!" $ do
      QC.property $ \ (a :: Word8) (b :: Word8) -> 
        testForAll $ \ (c :: Word8) -> do
        let a' = fromWord8 a
        let b' = fromWord8 b
        let c' = fromWord8 c
        test a' b' --> test (a' .&. c') b' `shouldBe` True
    it "setting bits in b cant make it false!" $ do
      QC.property $ \ (a :: Word8) (b :: Word8) -> 
        testForAll $ \ (c :: Word8) -> do
        let a' = fromWord8 a
        let b' = fromWord8 b
        let c' = fromWord8 c
        test a' b' --> test a' (b' .|. c') `shouldBe` True

  describe "runCmd" $ do
    it "can toggle" $ do
      testForAll $ \(c :: Word8) (t :: Word8) -> do
        let control = fromWord8 c
        let target = fromWord8 t
        let state = control
        let cmd = Cmd control target
        let expected = state .^. target
        runCmd cmd state `shouldBe` expected
    it "can not toggle" $ do
      testForAll $ \(c :: Word8) (t :: Word8) -> do
        let control = fromWord8 c
        let target = fromWord8 t
        let state = control .^. true (0, 7)
        let cmd = Cmd control target
        let expected = state .^. target
        c /= 0 --> runCmd cmd state `shouldBe` state
    it "should never do anything else" $ do
      QC.property $ \(c :: Word8) (t :: Word8) -> 
        testForAll $ \ (s :: Word8) -> do
        let control = fromWord8 c
        let target = fromWord8 t
        let state = fromWord8 s
        let cmd = Cmd control target
        let expected = state .^. target
        let actual = runCmd cmd state
        actual /= state --> actual `shouldBe` expected
    it "is the reverse of runCmd" $
      property $ \(cmd :: Cmd) ->  
        testForAll $ \ (state :: Word8) -> do
        let state' = fromWord8 state
        runCmd cmd (runCmd cmd state') `shouldBe` state'
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
        testForAll $ \(x :: Word8) ->
        toWord8 (fromWord8 x) `shouldBe` x
  describe "encode/decode" $ do
    describe "encodeCommand" $ do
      it "can be decoded" $
        testForAll $ \ (c :: Word8) (x :: Word8) -> canBeDecoded (Cmd (fromWord8 c) (fromWord8 x))
    describe "encodeCircuit" $ do
      it "can be decoded" $
        property canBeDecoded'
-- the other way around is not true, because the encoding is not unique, multiple codes will get decoded to same circuit/cmd

  describe "toWord8" $ do
    it "is the reverse of fromWord8" $
      testForAll $ \(x :: Word8) ->
        toWord8 (fromWord8 x) `shouldBe` x
    it "is an embedding" $
      testForAll $ \(x :: Word8) y ->
        x /= y --> fromWord8 x `shouldNotBe` fromWord8 y

canBeDecoded :: Cmd -> Expectation
canBeDecoded cmd = decodeCommand (encodeCommand cmd) `shouldBe` cmd

canBeDecoded' :: Circuit -> Expectation
canBeDecoded' circuit = decodeCircuit (encodeCircuit circuit) `shouldBe` circuit

exampleCmd :: Cmd
exampleCmd =
  Cmd
    (fromWord8 0x0F)
    (fromWord8 0xF0)
