module LuhnSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Luhn
import Luhn.Internal

spec :: Spec
spec = do
  describe "validate" $ do
    it "true on numbers that pass the Luhn validation, false on those that dont" $ do
      1234567889 `shouldSatisfy` validate
      1234567887 `shouldNotSatisfy` validate
  
