{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

module Circuit (Circuit, Cmd(..), ToggleCmd(..), RotateCmd(..), SwapCmd(..), runCmd, runCmdReverse) where

import Data.Array.BitArray             -- Immutable bit arrays.
import Data.Array.BitArray.ByteString  -- (De)serialization.
import Codec.Image.PBM                 -- Portable bitmap monochrome 2D image format.
import Text.Show
import GHC.Num
import GHC.Int
import GHC.Base
import Data.List (reverse)
import GHC.Real
import Data.Maybe

(.&.) :: BitArray Int -> BitArray Int -> BitArray Int
(.&.) = zipWith (&&)

(.|.) :: BitArray Int -> BitArray Int -> BitArray Int
(.|.) = zipWith (||)

(.^.) :: BitArray Int -> BitArray Int -> BitArray Int
(.^.) = zipWith (/=)

(<<<.) :: BitArray Int -> Int -> BitArray Int
arr <<<. n =
  let (start, end) = bounds arr in
  ixmap (0, end - start) (\i -> start + ((i + n) `mod` (end - start + 1))) arr

(>>>.) :: BitArray Int -> Int -> BitArray Int
arr >>>. n = 
  let (start, end) = bounds arr in
  ixmap (0, end - start) (\i -> start + ((i - n) `mod` (end - start + 1))) arr

data ToggleCmd =
    ToggleCmd { control :: BitArray Int, target :: BitArray Int}
    deriving (Show)

instance Show (BitArray Int) where
  show b = "fromList [" <> fmap (\x -> if x then '1' else '0') (elems b) <> "]"

isValid :: ToggleCmd -> Bool
isValid (ToggleCmd control target) = isNothing $ elemIndex True (control .&. target) 

data RotateCmd where 
  RotateCmd :: Int -> RotateCmd
  deriving (Show)

data SwapCmd where
  SwapCmd :: Int -> Int -> SwapCmd
  deriving (Show)

data Cmd =
    Toggle ToggleCmd
  | Rotate RotateCmd
  | Swap SwapCmd
  deriving (Show)

type Circuit = [Cmd]

runCmd :: Cmd -> BitArray Int -> BitArray Int
runCmd (Toggle (ToggleCmd control target)) = 
  \ state -> if and (zipWith bIf control target) then state .^. target else state
  where
  bIf True False = False
  bIf _ _ = True
runCmd (Rotate (RotateCmd n)) = (<<<. n)
runCmd (Swap (SwapCmd i j)) = \ state -> state // [(i, state !!! j), (j, state !!! i)]

runCmdReverse :: Cmd -> BitArray Int -> BitArray Int
runCmdReverse (Rotate (RotateCmd n)) = (>>>. n)
runCmdReverse c = runCmd c

runCircuit :: Circuit -> BitArray Int -> BitArray Int
runCircuit = foldr ((.) . runCmd) id 

runCircuitReverse :: Circuit -> BitArray Int -> BitArray Int
runCircuitReverse = foldr ((.) . runCmdReverse) id . reverse
