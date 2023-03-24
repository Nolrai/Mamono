{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BinaryLiterals #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}

module SerializationSpec where

import Circuit
import Data.Bool (not)
import Data.ByteString (readFile)
import Data.List qualified as List
import Data.Vector qualified
import Relude ( ($), Eq )
import Relude.Function
import Serialization
import Test.Hspec
import Test.QuickCheck
import Control.Monad (forM_)
import Relude.Base (Show)

roundTrip :: (Show a, Eq a, Serialize a) => a -> Expectation
roundTrip c = do
  let serialized = serialize c
  deserialized <- deserialize serialized
  deserialized `shouldBe` c

spec :: Spec
spec = do
  describe "Serialization" $ do
    it "should serialize and deserialize a circuit" $ do
      property $ \(c :: Circuit) ->
        not (List.null c) ==> roundTrip c
    it "should serialize and deserialize a population" $ do
      property $ \(p' :: [Circuit]) ->
        let p = List.filter (not . List.null) p'
         in not (List.null p) ==> roundTrip p
