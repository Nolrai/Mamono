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
import GHC.Float (log)

spec :: Spec
spec = do
  describe "scoreWord8" $ do
    it "is between 0 and log 255" $ do
      property $ \(w :: Word8) (circuit :: Circuit) -> do
        let score = scoreWord8 w circuit
        score `shouldSatisfy` (>= 0)
        score `shouldSatisfy` (<= 8)
  describe "scoreLine" $ do
    it "is between 0 and log 255 times the length of the line" $ do
      property $ \(line :: [Word8]) (circuit :: Circuit) -> do
        let score = scoreLine (ByteString.pack line) circuit
        score `shouldSatisfy` (>= 0)
        score `shouldSatisfy` (<= 8 * fromIntegral (List.length line))
  describe "scoreLines" $ do
    it "is between 0 and log 255" $ do
      property $ \(lines :: [[Word8]]) (circuit :: Circuit) -> do
        not (List.null lines || List.all List.null lines) ==> do
          let score = scoreLines (Vector.fromList $ ByteString.pack <$> lines) circuit
          score `shouldSatisfy` (>= 0)
          score `shouldSatisfy` (<= 8)
