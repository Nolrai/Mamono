{-# LANGUAGE NoImplicitPrelude #-}

module DualNumbers where

import Relude hiding (Dual)

data Dual a = Dual a a
  deriving (Eq, Show, Ord)

instance Num a => Num (Dual a) where
  Dual a b + Dual c d = Dual (a + c) (b + d)
  Dual a b - Dual c d = Dual (a - c) (b - d)
  Dual a b * Dual c d = Dual (a * c) (a * d + b * c)
  negate (Dual a b) = Dual (negate a) (negate b)
  abs (Dual a b) = Dual (abs a) (b * signum a)
  signum (Dual a b) = Dual (signum a) (b * signum a)
  fromInteger n = Dual (fromInteger n) 0

addEpsilon :: Num a => a -> Dual a
addEpsilon x = Dual x 1
