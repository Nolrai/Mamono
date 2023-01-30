module Luhn (validate) where

import Luhn.Internal (toDigits, doubleEveryOther, sumDigits)

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits
