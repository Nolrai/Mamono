{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
import GHC.Real
import Moo.GeneticAlgorithm
import Moo.GeneticAlgorithm.Binary
import Test.QuickCheck hiding ((.&.))
import Text.Show

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
toWord8 bitArray = List.sum $ List.zipWith (\i b -> if b then 2 ^ i else 0) [0 .. 7] (elems bitArray)

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

data ToggleCmd = ToggleCmd {control :: BitArray Word8, target :: BitArray Word8}
  deriving (Show, Eq, Ord)

instance Show (BitArray Word8) where
  show b = "fromList [" <> fmap (\x -> if x then '1' else '0') (elems b) <> "]"

data RotateCmd where
  RotateCmd :: Word8 -> RotateCmd
  deriving (Show, Eq, Ord)

data SwapCmd where
  SwapCmd :: Word8 -> Word8 -> SwapCmd
  deriving (Show, Eq, Ord)

data Cmd where
  Toggle :: ToggleCmd -> Cmd
  Rotate :: RotateCmd -> Cmd
  Swap :: SwapCmd -> Cmd
  deriving (Show, Eq, Ord)

type Circuit = V.Vector Cmd

runCmd :: Cmd -> BitArray Word8 -> BitArray Word8
runCmd (Toggle (ToggleCmd control target)) =
  \state -> if and (zipWith bIf control target) then state .^. target else state
  where
    bIf True False = False
    bIf _ _ = True
runCmd (Rotate (RotateCmd n)) = (<<<. n)
runCmd (Swap (SwapCmd i j)) = \state ->
  let (start, end) = bounds state
   in if end - start < 2
        then state -- the state is empty or has only one element
        else
          let i' = start + (i `mod` (end - start + 1))
              j' = start + (j `mod` (end - start + 1))
           in state // [(i', state !!! j'), (j', state !!! i')]

runCmdReverse :: Cmd -> BitArray Word8 -> BitArray Word8
runCmdReverse (Rotate (RotateCmd n)) = (>>>. n)
runCmdReverse c = runCmd c

runCircuit :: Circuit -> BitArray Word8 -> BitArray Word8
runCircuit = V.foldr ((.) . runCmd) id

runCircuitReverse :: Circuit -> BitArray Word8 -> BitArray Word8
runCircuitReverse = V.foldr ((.) . runCmdReverse) id . V.reverse

instance Arbitrary ToggleCmd where
  arbitrary :: Gen ToggleCmd
  arbitrary = do
    control <- arbitrary
    target <- arbitrary
    let filteredControl = zipWith (\c t -> c && not t) control target
    return $ ToggleCmd filteredControl target

mkIndex :: Gen Word8
mkIndex = fromIntegral <$> chooseInt (0, 7)

instance Arbitrary RotateCmd where
  arbitrary :: Gen RotateCmd
  arbitrary = RotateCmd <$> mkIndex

instance Arbitrary SwapCmd where
  arbitrary :: Gen SwapCmd
  arbitrary = do
    i <- mkIndex
    j <- mkIndex
    if i /= j
      then pure $ SwapCmd i j
      else arbitrary

instance Arbitrary Cmd where
  arbitrary :: Gen Cmd
  arbitrary = oneof [Toggle <$> arbitrary, Rotate <$> arbitrary, Swap <$> arbitrary]

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

exampleCommand :: Cmd
exampleCommand = Toggle (ToggleCmd (false (0, 7)) (false (0, 7)))

exampleCommandLength :: Int
exampleCommandLength = List.length $ encodeCommand exampleCommand

encodeCommand :: Cmd -> [Bool]
encodeCommand (Toggle (ToggleCmd control target)) = encodeGray' (0, 3) 0 <> BitArray.elems control <> BitArray.elems target
encodeCommand (Rotate (RotateCmd n)) = pad $ encodeGray' (0, 3) 1 <> encodeGray' (0, 7) n
encodeCommand (Swap (SwapCmd i j)) = pad $ encodeGray' (0, 3) 2 <> encodeGray' (0, 7) i <> encodeGray' (0, 7) j

pad l = List.take exampleCommandLength (l ++ List.repeat False)

encodeGray' :: (Word8, Word8) -> Word8 -> [Bool]
encodeGray' = encodeGray

decodeCommand :: [Bool] -> Cmd
decodeCommand l =
  case cmd of
    0 -> Toggle (ToggleCmd (fromList control) (fromList target))
    1 -> Rotate (RotateCmd (decodeGray (0, 7) index0))
    2 -> Swap (SwapCmd (decodeGray (0, 7) index0) (decodeGray (0, 7) index1))
    _ -> error "invalid command"
  where
    cmd :: Word8
    cmd = decodeGray (0, 3) cmdCode
    cmdCode, values, control, target, index0, index1 :: [Bool]
    (cmdCode, values) = List.splitAt 2 l
    (control, target) = List.splitAt 8 values
    (index0 : index1 : _) = splitEvery 3 values

encodeCircuit :: Circuit -> [Bool]
encodeCircuit = List.concatMap encodeCommand . V.toList

decodeCircuit :: [Bool] -> Circuit
decodeCircuit = V.map decodeCommand . V.fromList . splitEvery exampleCommandLength
