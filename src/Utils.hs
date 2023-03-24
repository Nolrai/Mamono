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
import Relude (Show, show, (/))
import System.IO (FilePath, IOMode (ReadMode), openFile)

-- score a circuit against a set of lines
scoreLines :: Vector ByteString -> Circuit -> Double
scoreLines lines circuit = fromIntegral (List.sum $ (`scoreLine` circuit) <$> lines) / fromIntegral (Vector.sum $ ByteString.length <$> lines)

-- | Score a circuit against a single line
scoreLine :: ByteString -> Circuit -> Int
scoreLine line circuit =
  ByteString.foldl (\acc c -> acc + scoreWord8 c circuit) 0 line

-- | Score a circuit against a single word8
scoreWord8 :: Word8 -> Circuit -> Int
scoreWord8 w circuit =
  fromIntegral . toWord8 . runCircuit circuit $ fromWord8 w

showToBS :: Show a => a -> ByteString
showToBS = Text.encodeUtf8 . show
