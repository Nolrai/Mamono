{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Circuit (Circuit, Cmd (..), ToggleCmd (..), RotateCmd (..), SwapCmd (..), runCmd, runCmdReverse, runCircuit, runCircuitReverse, isValid) where

import Codec.Image.PBM -- Portable bitmap monochrome 2D image format.
import Data.Array.BitArray -- Immutable bit arrays.
import Data.Array.BitArray.ByteString -- (De)serialization.
import Data.List (reverse)
import Data.Maybe
import GHC.Base
import GHC.Int
import GHC.Num
import GHC.Real
import Text.Show

(.&.) :: BitArray Int -> BitArray Int -> BitArray Int
(.&.) = zipWith (&&)

(.|.) :: BitArray Int -> BitArray Int -> BitArray Int
(.|.) = zipWith (||)

(.^.) :: BitArray Int -> BitArray Int -> BitArray Int
(.^.) = zipWith (/=)

(<<<.) :: BitArray Int -> Int -> BitArray Int
arr <<<. n =
  let (start, end) = bounds arr
   in ixmap (0, end - start) (\i -> start + ((i + n) `mod` (end - start + 1))) arr

(>>>.) :: BitArray Int -> Int -> BitArray Int
arr >>>. n =
  let (start, end) = bounds arr
   in ixmap (0, end - start) (\i -> start + ((i - n) `mod` (end - start + 1))) arr

data ToggleCmd = ToggleCmd {control :: BitArray Int, target :: BitArray Int}
  deriving (Show)

instance Show (BitArray Int) where
  show b = "fromList [" <> fmap (\x -> if x then '1' else '0') (elems b) <> "]"

isValid' :: ToggleCmd -> Bool
isValid' (ToggleCmd control target) = isNothing $ elemIndex True (control .&. target)

isValid :: Cmd -> Bool
isValid (Toggle c) = isValid' c
isValid _ = True

data RotateCmd where
  RotateCmd :: Int -> RotateCmd
  deriving (Show)

data SwapCmd where
  SwapCmd :: Int -> Int -> SwapCmd
  deriving (Show)

data Cmd
  = Toggle ToggleCmd
  | Rotate RotateCmd
  | Swap SwapCmd
  deriving (Show)

type Circuit = [Cmd]

runCmd :: Cmd -> BitArray Int -> BitArray Int
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

runCmdReverse :: Cmd -> BitArray Int -> BitArray Int
runCmdReverse (Rotate (RotateCmd n)) = (>>>. n)
runCmdReverse c = runCmd c

runCircuit :: Circuit -> BitArray Int -> BitArray Int
runCircuit = foldr ((.) . runCmd) id

runCircuitReverse :: Circuit -> BitArray Int -> BitArray Int
runCircuitReverse = foldr ((.) . runCmdReverse) id . reverse
