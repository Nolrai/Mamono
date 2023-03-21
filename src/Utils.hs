{-# LANGUAGE NoImplicitPrelude #-}

module Utils where

import Circuit
import Control.Monad
import Data.ByteString as ByteString
import Data.Set
import Data.Vector as V
import Data.Word (Word8)
import GHC.Base
import GHC.Enum
import GHC.Num (Num (..))
import GHC.Real (fromIntegral)
import Moo.GeneticAlgorithm
import System.IO (FilePath, IOMode (ReadMode), openFile)
import Test.QuickCheck

newLine :: Word8
newLine = 10

readFileToLineVector :: FilePath -> IO (Vector ByteString)
readFileToLineVector path = do
  handle <- openFile path ReadMode
  contents <- hGetContents handle
  let lineVector = V.fromList $ ByteString.split newLine contents
  return lineVector

-- | Score a circuit against a library of lines
scoreCircuit :: Vector ByteString -> Circuit -> Gen Int
scoreCircuit library circuit = do
  let libraryLength = V.length library
  ix <- chooseInt (0, libraryLength - 1)
  let line = library ! ix
  pure $ scoreLine line circuit

-- | Score a circuit against a single line
scoreLine :: ByteString -> Circuit -> Int
scoreLine line circuit =
  ByteString.foldl (\acc c -> acc + scoreWord8 c circuit) 0 line

-- | Score a circuit against a single word8
scoreWord8 :: Word8 -> Circuit -> Int
scoreWord8 w circuit =
  fromIntegral . toWord8 . runCircuit circuit $ fromWord8 w
