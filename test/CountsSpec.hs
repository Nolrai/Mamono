{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CountsSpec where

import Counts
import Test.Hspec
import Test.Hspec.QuickCheck as QC
import Test.QuickCheck as QC
import Relude
import Data.ByteString as BS
import Data.Vector as Vector
import Data.List (nub)

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
    it "is a balenced binary tree on the empty ByteString" $
     huffmanTree BS.empty `shouldSatisfy` balenced
    it "has no duplicates" $
      property $ \ bs' -> do
        let bs = BS.pack bs'
        huffmanTree bs `shouldSatisfy` noDuplicates
    it "contains every byte" $
      property $ \ bs' -> do
        let bs = BS.pack bs'
        Relude.toList (huffmanTree bs) `shouldMatchList` [0 .. 255]
    it "unaffected by mulitples of the counts" $
      property $ \ bs' -> do
        let bs = BS.pack bs'
        huffmanTree (bs <> bs) `shouldBe` huffmanTree bs
  describe "makeEncoderFromTree" $ do
    it "should fail if the tree is missing a byte" $
      property $ \ bs' -> do
        let bs = BS.pack bs'
            t = huffmanTree bs
        (sequenceA . makeEncoderFromTree =<< (t `delete` 0)) `shouldBe` Nothing
    it "should fail if the tree has duplicates" $
      property $ \ bs' -> do
        let bs = BS.pack bs'
            t = huffmanTree bs
        (sequenceA . makeEncoderFromTree $ (t `Node` Leaf 0)) `shouldBe` Nothing
    it "otherwise it should succeed" $
      property $ \ bs' -> do
        let bs = BS.pack bs'
            t = huffmanTree bs
        (sequenceA . makeEncoderFromTree) t `shouldSatisfy` isJust
    it "makes encoder that is inverse of decoder" $
      property $ \ (bs' :: String) -> do
        let bs = Relude.encodeUtf8 bs' <> BS.singleton 0
            t = huffmanTree bs
        BS.writeFile "test0.txt" bs
        case Relude.sequenceA $ makeEncoderFromTree t of
          Nothing -> True `shouldBe` True
          Just e -> do
            encode e "test0.txt" "test1.txt"
            decode t "test1.txt" "test2.txt" 
            result <- BS.readFile "test2.txt"
            BS.takeWhile (/= 0) result `shouldBe` BS.takeWhile (/= 0) bs

delete :: BinaryTree a -> a -> Maybe (BinaryTree a)
delete (Leaf a) x = Nothing
delete (Node a b) x =
  case (delete a x, delete b x) of
    (Just a', Just b') -> Just (Node a' b')
    (Just a', Nothing) -> Just a'
    (Nothing, Just b') -> Just b'
    (Nothing, Nothing) -> Nothing

balenced :: BinaryTree a -> Bool
balenced (Leaf _) = True
balenced (Node l r) = size l == size r && balenced l && balenced r

size :: BinaryTree a -> Int
size (Leaf _) = 1
size (Node l r) = 1 + size l + size r

noDuplicates :: Eq a => BinaryTree a -> Bool
noDuplicates tree = Relude.length (Relude.toList tree) == Relude.length (nub (Relude.toList tree))

instance Foldable BinaryTree where
  foldMap f (Leaf x) = f x
  foldMap f (Node l r) = Relude.foldMap f l <> Relude.foldMap f r
