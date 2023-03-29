{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Circuit where

import Control.Applicative ((<$>))
import Control.Monad (guard, replicateM)
import Data.Bits
import Data.ByteString qualified as BS
import Data.Ix
import Data.List (reverse)
import Data.List qualified as List
import Data.Maybe
import Data.Text.Lazy qualified
import Data.Text.Lazy qualified as Text
import Data.Text.Lazy qualified as Text.Lazy
import Data.Vector qualified as V
import Data.Word
import Debug.Trace
import Formatting
import GHC.Base
import GHC.Int
import GHC.Num
import GHC.Real hiding ((%))
import Moo.GeneticAlgorithm
import Moo.GeneticAlgorithm.Binary
import Relude (Enum (..))
import Test.QuickCheck hiding ((.&.))
import Text.Show

data Cmd = Cmd {control :: Word8, target :: Word8}
  deriving (Eq, Ord)

instance Show Cmd where
  show (Cmd control target) = Data.Text.Lazy.unpack $ format (hex % " " % hex) control target

type Circuit = V.Vector Cmd

runCmd :: Cmd -> Word8 -> Word8
runCmd (Cmd control target) state = if test (control `andNot` target) state then state `xor` target else state

test :: Word8 -> Word8 -> Bool
test a b = a .&. b == a

andNot :: Word8 -> Word8 -> Word8
andNot a b = a .&. complement b

runCircuit :: Circuit -> Word8 -> Word8
runCircuit = V.foldr ((.) . runCmd) id

runCircuitReverse :: Circuit -> Word8 -> Word8
runCircuitReverse = V.foldr ((.) . runCmd) id . V.reverse

instance Arbitrary Cmd where
  arbitrary :: Gen Cmd
  arbitrary = do
    control <- arbitrary
    target <- arbitrary
    let filteredControl = control `andNot` target
    return $ Cmd filteredControl target

mkIndex :: Gen Word8
mkIndex = fromIntegral <$> chooseInt (0, 7)

instance Arbitrary Circuit where
  arbitrary :: Gen Circuit
  arbitrary = V.fromList <$> arbitrary
  shrink :: Circuit -> [Circuit]
  shrink = fmap V.fromList . shrink . V.toList

noop :: Cmd
noop = Cmd 0 0

encodeCommand :: Cmd -> [Bool]
encodeCommand (Cmd control target) = toBools control <> toBools target

toBools :: Word8 -> [Bool]
toBools x = map (x `testBit`) [0 .. 7]

fromBools :: [Bool] -> Word8
fromBools = foldr (\b a -> a `shiftL` 1 .|. (if b then 1 else 0)) 0

decodeCommand :: [Bool] -> Cmd
decodeCommand l = Cmd (fromBools control) (fromBools target)
  where
    (control, target) = List.splitAt 8 l

encodeCircuit :: Circuit -> [Bool]
encodeCircuit = List.concatMap encodeCommand . V.toList

decodeCircuit :: [Bool] -> Circuit
decodeCircuit = V.map decodeCommand . V.fromList . splitEvery 16
