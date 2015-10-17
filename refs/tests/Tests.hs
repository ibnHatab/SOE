module Main where

import Refs
import Test.Hspec


main = hspec $ do
  describe "Verify thet parsing produce correct data" $ do
    it "equals zero" $ do
      theSum <- getAtBatsSum "batting.csv"
      theSum `shouldBe` 4858210
