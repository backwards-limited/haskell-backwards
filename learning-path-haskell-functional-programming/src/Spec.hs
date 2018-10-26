module Spec where

import Test.Hspec
import Test.QuickCheck
import Control.Exception

doubleIt x = x * 2

addTen x = x + 10

main :: IO ()
main = hspec $
  describe "Test" $
    it "executes" $ do
      print ((show . addTen . doubleIt) 5)
      1 `shouldBe` 1