{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Counts where

import Circuit
import Data.Vector as Vector
import Data.Vector.Mutable as MV
import Data.Vector.Algorithms.Intro as MV
import Data.ByteString as ByteString
import Data.ByteString.Lazy qualified as Lazy
import Relude hiding (Dual)
import qualified Data.List as List
import Control.Parallel
import Data.Conduit as C
import Data.Conduit.Combinators as C
import Data.PQueue.Prio.Min as PQ
import Data.Bits
import Data.Binary.Builder
import Data.Text as Text
import Control.Monad.ST as ST
import Text.Printf
import DualNumbers
import qualified Conduit as C

-- | given a sample of text, return a vector of counts
counts :: ByteString -> Vector Int
counts = ByteString.foldl (\acc c -> acc // [(fromIntegral c, acc ! fromIntegral c + 1)]) (Vector.replicate 256 0)

sumCounts :: Vector Int -> Vector Int -> Vector Int
sumCounts = Vector.zipWith (+)

countFile :: FilePath -> IO (Vector Int)
countFile path = do
  acc <- MV.replicate 256 0
  runConduitRes $ sourceFile path .| C.mapM_E (MV.modify acc (+1) . fromIntegral)
  Vector.freeze acc

entropy :: Vector Int -> Double
entropy counts = Vector.sum $ Vector.map (\c -> let p = fromIntegral c / total in if p == 0 then 0 else -p * logBase 2 p) counts
  where
    total = fromIntegral $ Vector.sum counts

data BinaryTree a = Leaf a | Node (BinaryTree a) (BinaryTree a) deriving (Show, Eq, Ord, Read)

huffmanTreeFromCounts :: Vector Int -> BinaryTree Word8
huffmanTreeFromCounts c = go $ PQ.fromList $ Vector.toList $ Vector.imap (\i x -> (addEpsilon x, Leaf (fromIntegral i))) c
  where
    go pq
      | PQ.size pq == 1 = snd $ PQ.findMin pq
      | otherwise = case (PQ.findMin pq, PQ.findMin $ PQ.deleteMin pq) of
        ((c1, t1), (c2, t2)) -> go $ PQ.insert (c1 + c2) (Node t1 t2) $ PQ.deleteMin $ PQ.deleteMin pq

huffmanTree :: ByteString -> BinaryTree Word8
huffmanTree = huffmanTreeFromCounts . counts

-- | given a huffman tree, return a vector of encodings
makeEncoderFromTree :: MonadFail m => BinaryTree Word8 -> Vector (m (Vector Bool))
makeEncoderFromTree tree = Vector.generate 255 (\ i -> handleResult i . flip go tree . fromIntegral $ i)
  where
    go :: Word8 -> BinaryTree Word8 -> [ [Bool] ]
    go w (Leaf w') = if w == w' then pure [] else []
    go w (Node l r) = (True :) <$> go w l <|> (False :) <$> go w r
    handleResult i [] = fail $ "makeEncoderFromTree: no encoding for " <> show i
    handleResult _ [x] = pure (Vector.fromList x)
    handleResult i xs = fail $ "makeEncoderFromTree: multiple encodings for " <> show i <> ": " <> show xs

-- | encode a file using a vector of encodings
encode :: (Int -> IO ()) -> Vector (Vector Bool) -> FilePath -> FilePath -> IO ()
encode onProgress encoder input output =
  runConduitRes $ sourceFile input .| reportProgress onProgress .| C.concatMapCE  ((encoder !) . fromIntegral) .| C.concat .| collectBits .| builderToByteString .| sinkFile output

reportProgress :: MonadIO m => (Int -> IO ()) -> ConduitT ByteString ByteString m ()
reportProgress onProgress = iterM (liftIO . onProgress . ByteString.length)

collectBits :: (PrimMonad m, Monad m) => ConduitT Bool Builder m ()
collectBits = conduitVector 8 .| C.map vectorToByte .| C.map Data.Binary.Builder.singleton
  where
    vectorToByte :: Vector Bool -> Word8
    vectorToByte = Vector.ifoldl' (\acc i b -> if b then setBit acc i else acc) 0

-- | decode a file using a huffman tree
decode:: (Int -> IO ()) -> BinaryTree Word8 -> FilePath -> FilePath -> IO ()
decode onProgress tree input output =
  runConduitRes $ sourceFile input .| reportProgress onProgress .| bytesToBits .| decodeTree tree .| builderToByteString .| sinkFile output

bytesToBits :: MonadFail m => ConduitT ByteString Bool m ()
bytesToBits = C.concatMapCE byteToBits .| C.concat
  where
  byteToBits byte = testBit byte <$> Vector.enumFromN 0 8

decodeTree :: Monad m => BinaryTree Word8 -> ConduitT Bool Builder m ()
decodeTree tree = go tree
  where
    go (Leaf w) = C.yield (Data.Binary.Builder.singleton w) >> go tree
    go (Node l r) = C.await >>= maybe (pure ()) (\b -> go (if b then l else r))

showCode :: Vector Bool -> Text
showCode = Text.pack . Vector.toList . Vector.map (bool '0' '1')

formatEncoderRow :: Int -> Vector Bool -> Text
formatEncoderRow i code = Text.pack $ printf "%3d : %s\n" i (showCode code)

showEncoder, showEncoderDF, showEncoderBF :: Vector (Vector Bool) -> Text
showEncoder = Vector.ifoldl' (\acc i x -> acc <> formatEncoderRow i x) ""
showEncoderDF = Vector.foldl' (\acc (i, x) -> acc <> formatEncoderRow i x) "" . vectorSortBy (compare `on` (\ (ix, str) -> (Vector.length str, str))) . Vector.indexed
showEncoderBF = Vector.foldl' (\acc (i, x) -> acc <> formatEncoderRow i x) "" . vectorSortBy (compare `on` snd) . Vector.indexed

vectorSortBy :: (a -> a -> Ordering) -> Vector a -> Vector a
vectorSortBy f v = runST $ do
  mv <- Vector.thaw v
  MV.sortBy f mv
  Vector.freeze mv
