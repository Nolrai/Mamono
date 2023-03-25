{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Circuit where

import Control.Applicative ((<$>))
import Control.Monad (guard, replicateM)
import Data.Array.BitArray as BitArray
import Data.Array.BitArray.ByteString
import Data.ByteString qualified as BS
import Data.Ix
import Data.List (reverse)
import Data.List qualified as List
import Data.Maybe
import Data.Vector qualified as V
import Data.Word
import Debug.Trace
import GHC.Base
import GHC.Int
import GHC.Num
import GHC.Real hiding ((%))
import Moo.GeneticAlgorithm
import Moo.GeneticAlgorithm.Binary
import Test.QuickCheck hiding ((.&.))
import Text.Show
import Formatting
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy as Text.Lazy

(.&.) :: BitArray Word8 -> BitArray Word8 -> BitArray Word8
(.&.) = zipWith (&&)

(.|.) :: BitArray Word8 -> BitArray Word8 -> BitArray Word8
(.|.) = zipWith (||)

(.^.) :: BitArray Word8 -> BitArray Word8 -> BitArray Word8
(.^.) = zipWith (/=)

(<<<.) :: BitArray Word8 -> Word8 -> BitArray Word8
arr <<<. n =
  let (start, end) = bounds arr
   in ixmap (0, end - start) (\i -> start + ((i + n) `mod` (end - start + 1))) arr

(>>>.) :: BitArray Word8 -> Word8 -> BitArray Word8
arr >>>. n =
  let (start, end) = bounds arr
   in ixmap (0, end - start) (\i -> start + ((i - n) `mod` (end - start + 1))) arr

fromWord8 :: Word8 -> BitArray Word8
fromWord8 c = fromByteString (0, 7) (BS.singleton c)

toWord8 :: BitArray Word8 -> Word8
toWord8 bitArray = BS.head (toByteString bitArray)

instance Eq (BitArray Word8) where
  (/=) :: BitArray Word8 -> BitArray Word8 -> Bool
  a /= b = (bounds a /= bounds b) || or (a .^. b)

instance Ord (BitArray Word8) where
  compare :: BitArray Word8 -> BitArray Word8 -> Ordering
  a `compare` b = toByteString a `compare` toByteString b

fromList :: [Bool] -> BitArray Word8
fromList l = listArray (0, fromIntegral (List.length l - 1)) l

toList :: BitArray Word8 -> [Bool]
toList = elems

data Cmd = Cmd {control :: BitArray Word8, target :: BitArray Word8}
  deriving (Show, Eq, Ord)

instance Show (BitArray Word8) where
  show b = Text.Lazy.unpack $ format ("fromWord8 " % binPrefix 8) (toWord8 b)

type Circuit = V.Vector Cmd

runCmd :: Cmd -> BitArray Word8 -> BitArray Word8
runCmd (Cmd control target) state = if test (control `andNot` target) state then state .^. target else state

test :: BitArray Word8 -> BitArray Word8 -> Bool
test a b = and $ zipWith bIf a b
  where
    bIf True False = False
    bIf _ _ = True

andNot :: BitArray Word8 -> BitArray Word8 -> BitArray Word8
andNot = zipWith (\a b -> a && not b)

runCircuit :: Circuit -> BitArray Word8 -> BitArray Word8
runCircuit = V.foldr ((.) . runCmd) id

runCircuitReverse :: Circuit -> BitArray Word8 -> BitArray Word8
runCircuitReverse = V.foldr ((.) . runCmd) id . V.reverse

instance Arbitrary Cmd where
  arbitrary :: Gen Cmd
  arbitrary = do
    control <- arbitrary
    target <- arbitrary
    let filteredControl = zipWith (\c t -> c && not t) control target
    return $ Cmd filteredControl target

mkIndex :: Gen Word8
mkIndex = fromIntegral <$> chooseInt (0, 7)

instance Arbitrary (BitArray Word8) where
  arbitrary :: Gen (BitArray Word8)
  arbitrary = fromList <$> replicateM 8 arbitrary
  shrink :: BitArray Word8 -> [BitArray Word8]
  shrink = fmap fromList . List.filter ((== 8) . List.length) . shrink . elems

instance Arbitrary Circuit where
  arbitrary :: Gen Circuit
  arbitrary = V.fromList <$> arbitrary
  shrink :: Circuit -> [Circuit]
  shrink = fmap V.fromList . shrink . V.toList

noop :: Cmd
noop = Cmd (false (0, 7)) (false (0, 7))

encodeCommand :: Cmd -> [Bool]
encodeCommand (Cmd control target) = BitArray.elems control <> BitArray.elems target

decodeCommand :: [Bool] -> Cmd
decodeCommand l = Cmd (fromList control) (fromList target)
  where 
  (control, target) = List.splitAt 8 l
    
encodeCircuit :: Circuit -> [Bool]
encodeCircuit = List.concatMap encodeCommand . V.toList

decodeCircuit :: [Bool] -> Circuit
decodeCircuit = V.map decodeCommand . V.fromList . splitEvery 16
