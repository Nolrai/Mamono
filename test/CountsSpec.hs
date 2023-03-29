{-# LANGUAGE NoImplicitPrelude #-}

module CountsSpec where

import Counts
import Test.Hspec
import Test.Hspec.QuickCheck as QC
import Test.QuickCheck as QC
import Relude
import Data.ByteString as BS
import Data.Vector

spec :: Spec
spec = focus $ do
  describe "counts" $ do
    it "counts how many times a byte occurs in a ByteString" $
      QC.property $ \(NonEmpty bs') ix ->
        let bs = BS.pack bs' in
        case bs `BS.indexMaybe` (ix `mod` max 1 (BS.length bs)) of
          Nothing -> True `shouldBe` True
          Just x -> counts bs ! fromIntegral x `shouldBe` fromIntegral (BS.length (BS.filter (== x) bs))
    it "is a monoid homomorphism" $
      QC.property $ \(NonEmpty bs0') (NonEmpty bs1') ->
        let bs0 = BS.pack bs0' in
        let bs1 = BS.pack bs1' in
        counts (bs0 <> bs1) `shouldBe` sumCounts (counts bs0) (counts bs1)
    it "sum of counts is the length of the ByteString" $
      QC.property $ \(NonEmpty bs') ->
        let bs = BS.pack bs' in
        Vector.sum (counts bs) `shouldBe` BS.length bs
  describe "huffmanTree" $ do
    it ""
