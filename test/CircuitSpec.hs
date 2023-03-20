{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CircuitSpec where

import Circuit
import Control.Exception (evaluate)
import Data.Array.BitArray
import Data.Functor
import Data.List qualified as List
import GHC.Base
import GHC.Num ((-))
import Test.Hspec
import Test.QuickCheck

fromList :: [Bool] -> BitArray Int
fromList l = listArray (0, List.length l - 1) l

instance Arbitrary ToggleCmd where
  arbitrary = ToggleCmd <$> arbitrary <*> arbitrary

instance Arbitrary RotateCmd where
  arbitrary = RotateCmd <$> arbitrary

instance Arbitrary SwapCmd where
  arbitrary = SwapCmd <$> arbitrary <*> arbitrary

instance Arbitrary Cmd where
  arbitrary = oneof [Toggle <$> arbitrary, Rotate <$> arbitrary, Swap <$> arbitrary]

instance Arbitrary (BitArray Int) where
  arbitrary = fromList <$> arbitrary
  shrink = fmap fromList . shrink . elems

instance Eq (BitArray Int) where
  (==) a b = bounds a == bounds b && and (zipWith (==) a b)

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
      let expected = fromList $ (== 1) <$> [1, 1, 0, 0] <> [0, 0, 0, 0]
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
      let state = fromList $ (== 1) <$> [1, 1, 0, 0] <> [0, 0, 0, 0]
      let cmd = Rotate (RotateCmd 1)
      let expected = fromList $ (== 1) <$> [1, 0, 0, 0] <> [0, 0, 0, 1]
      runCmdReverse cmd state `shouldBe` expected
    it "can be swapped" $ do
      let state = fromList $ (== 1) <$> [0, 0, 0, 0] <> [0, 0, 0, 1]
      let cmd = Swap (SwapCmd 0 7)
      let expected = fromList $ (== 1) <$> [1, 0, 0, 0] <> [0, 0, 0, 0]
      runCmdReverse cmd state `shouldBe` expected
    it "is the reverse of runCmd" $
      property $ \(cmd :: Cmd) (state :: BitArray Int) ->
        isValid cmd ==>
          runCmdReverse cmd (runCmd cmd state) `shouldBe` state
  describe "runCircuit" $ do
    it "is id on the empty circuit" $
      property $ \(state :: BitArray Int) ->
        runCircuit [] state `shouldBe` state
    it "is the reverse of runCircuitReverse" $
      property $ \(circuit :: Circuit) (state :: BitArray Int) ->
        List.all isValid circuit ==>
          runCircuit circuit (runCircuitReverse circuit state) `shouldBe` state
