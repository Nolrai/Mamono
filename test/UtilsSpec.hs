{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module UtilsSpec where

import Circuit
import Data.ByteString as ByteString
import qualified Data.List as List
import Data.Vector as Vector
import Relude
import Test.Hspec
import Test.QuickCheck
import Utils

spec :: Spec
spec = do
  describe "scoreWord8" $ do
    it "scores a circuit against a single word8" $ do
      let circuit = Vector.fromList [Cmd (Circuit.fromWord8 1) (Circuit.fromWord8 2)]
      scoreWord8 1 circuit `shouldBe` 3
    it "is between 0 and 255" $ do
      property $ \(w :: Word8) (circuit :: Circuit) -> do
        let score = scoreWord8 w circuit
        score `shouldSatisfy` (>= 0)
        score `shouldSatisfy` (<= 255)
  describe "scoreLine" $ do
    it "scores a circuit against a line" $ do
      let line = ByteString.pack [3, 7, 2]
      let circuit = Vector.fromList [Cmd (Circuit.fromWord8 1) (Circuit.fromWord8 2)]
      scoreLine line circuit `shouldBe` List.sum [1, 5, 2]
    it "is between 0 and 255 times the length of the line" $ do
      property $ \(line :: [Word8]) (circuit :: Circuit) -> do
        let score = scoreLine (ByteString.pack line) circuit
        score `shouldSatisfy` (>= 0)
        score `shouldSatisfy` (<= 255 * List.length line)
  describe "scoreLines" $ do
    it "scores a circuit against a set of lines" $ do
      let lines = Vector.fromList [ByteString.pack [1, 2, 3], ByteString.pack [4, 5, 6]]
      let circuit = Vector.fromList [Cmd (Circuit.fromWord8 1) (Circuit.fromWord8 2)]
      scoreLines lines circuit `shouldBe` ((3 + 2 + 1 + 4 + 7 + 6) / 6)
    it "is between 0 and 255" $ do
      property $ \(lines :: [[Word8]]) (circuit :: Circuit) -> do
        not (List.null lines || List.all List.null lines) ==> do
          let score = scoreLines (Vector.fromList $ ByteString.pack <$> lines) circuit
          score `shouldSatisfy` (>= 0)
          score `shouldSatisfy` (<= 255)
