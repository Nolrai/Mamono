{-# LANGUAGE ScopedTypeVariables #-}

module Luhn.InternalSpec (spec) where

import Control.Monad (zipWithM_)
import Luhn.Internal (doubleEveryOther, sumDigits, toDigits)
import Test.Hspec
import Test.QuickCheck

spec :: Spec
spec = do
  describe "toDigits" $ do
    context "converts a number to a list of digits" $ do
      it "(examples)" $ do
        toDigits 1234567 `shouldBe` [1, 2, 3, 4, 5, 6, 7]
        toDigits 987 `shouldBe` [9, 8, 7]
        toDigits 123 `shouldBe` [1, 2, 3]
      context "(quickcheck)" $ do
        it "the list is a list of digits" $
          property $ \(NonNegative x) -> mapM_ (`shouldSatisfy` isDigit) $ toDigits x
        it "is undone by fromDigits" $
          property $ \(NonNegative x) -> fromDigits (toDigits x) `shouldBe` x
        it "(concatMap show . toDigits) x == show x | (for positive values)" $
          property $ \(Positive x) -> (concatMap show . toDigits) x `shouldBe` show x
  describe "doubleEveryOther" $ do
    context "(examples)" $
      zipWithM_
        (\index example -> it ("example " <> show index) example)
        [0 ..]
        [ doubleEveryOther [1, 2, 3, 4, 5, 6] `shouldBe` [2, 2, 6, 4, 10, 6],
          doubleEveryOther [1, 2] `shouldBe` [2, 2],
          doubleEveryOther [1] `shouldBe` [2],
          doubleEveryOther [1, 0] `shouldBe` [2, 0],
          doubleEveryOther [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1] `shouldBe` [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2], -- these four are from a quick check case that was failing.
          doubleEveryOther [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0] `shouldBe` [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0],
          doubleEveryOther [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1] `shouldBe` [2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2],
          doubleEveryOther [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1] `shouldBe` [2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1]
        ]
    context "(quickcheck)" $ do
      it "preserves length" $
        property $ \x -> length (doubleEveryOther x) `shouldBe` length x
      it "is 'causual'" $
        property $ \x y z -> take (length x) (doubleEveryOther (x ++ y)) `shouldBe` take (length x) (doubleEveryOther (x ++ z))
      it "doesn't change the result after dropEveryOtherLeft" $
        property $ \x -> dropEveryOtherLeft (doubleEveryOther x) `shouldBe` dropEveryOtherLeft x
      it "is the same as map (*2) after dropEveryOtherRight" $
        property $ \x -> dropEveryOtherRight (doubleEveryOther x) `shouldBe` dropEveryOtherRight (map (* 2) x)
  describe "sumDigits" $ do
    context "preserves mod 9" $ do
      it "(on singletons)" $
        property $ \(Positive x) -> (sumDigits [x] `mod` 9) `shouldBe` (x `mod` 9)
      it "(on sums)" $
        property $ \xs -> (sumDigits (map abs xs) `mod` 9) `shouldBe` (sum (map abs xs) `mod` 9)
    context "when all numbers are less than 10" $ do
      it "sums the list of integers (example)" $ do
        sumDigits [1, 2, 3, 4, 5, 6] `shouldBe` sum [1, 2, 3, 4, 5, 6]
      it "sums the list of integers (quickcheck)" $ do
        property
          ( \(xs' :: [NonNegative Integer]) ->
              let xs = map (\(NonNegative x) -> x `mod` 10) xs'
               in all (< 10) xs ==> sumDigits xs `shouldBe` sum xs
          )

fromDigits :: Num p => [p] -> p
fromDigits = go 0
  where
    go soFar [] = soFar
    go soFar (x : xs) = go (soFar * 10 + x) xs

isDigit :: (Ord p, Num p) => p -> Bool
isDigit n = 0 <= n && n < 10

dropEveryOtherLeft, dropEveryOtherRight :: [a] -> [a]
dropEveryOtherLeft [] = []
dropEveryOtherLeft [x] = []
dropEveryOtherLeft (_ : y : xs) = y : dropEveryOtherLeft xs
dropEveryOtherRight [] = []
dropEveryOtherRight [x] = [x]
dropEveryOtherRight (x : _ : xs) = x : dropEveryOtherRight xs
