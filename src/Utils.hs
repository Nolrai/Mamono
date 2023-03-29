{-# LANGUAGE NoImplicitPrelude #-}

module Utils where

import Circuit
import Control.Applicative ((<$>))
import Control.Monad
import Data.ByteString as ByteString
import qualified Data.ByteString as Text
import Data.List as List
import Data.Set
import qualified Data.Text.Encoding as Text
import Data.Vector as V
import Data.Vector as Vector
import Data.Word (Word8)
import GHC.Base
import GHC.Enum
import GHC.Num (Num (..))
import GHC.Real (fromIntegral)
import Moo.GeneticAlgorithm
import Relude (Show, show, (/), Floating (logBase))
import System.IO (FilePath, IOMode (ReadMode), openFile)
import GHC.Float (Floating(log1p, log))

-- score a circuit against a set of lines
scoreLines :: Vector ByteString -> Circuit -> Double
scoreLines lines circuit = List.sum ((`scoreLine` circuit) <$> lines) / fromIntegral (Vector.sum $ ByteString.length <$> lines)

-- | Score a circuit against a single line
scoreLine :: ByteString -> Circuit -> Double
scoreLine line circuit =
  ByteString.foldl (\acc c -> acc + scoreWord8 c circuit) 0 line

-- | Score an encoder against a single line
scoreEncoder :: ByteString -> Vector Word8 -> Double
scoreEncoder line v =
  ByteString.foldl (\acc c -> acc + (log1p (fromIntegral (v ! fromIntegral c)) / log 2)) 0 line

-- | Score a circuit against a single word8
scoreWord8 :: Word8 -> Circuit -> Double
scoreWord8 w circuit =
  (/ log 2) . log1p . fromIntegral . runCircuit circuit $ w -- log1p the set {0..n} takes log(n+1) bits

showToBS :: Show a => a -> ByteString
showToBS = Text.encodeUtf8 . show
