module Luhn.Internal 
  ( toDigits
  ,  doubleEveryOther -- don't export doubleEveryOther'
  ,  sumDigits
  ) where

toDigits :: Integer -> [Integer]
toDigits = go []
  where
  go xs n
    | n <= 0 = xs
    | otherwise = go (m:xs) d where (d, m) = divMod n 10

doubleEveryOther, doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x : xs) = x * 2 : doubleEveryOther' xs

doubleEveryOther' [] = []
doubleEveryOther' (x : xs) = x : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits
