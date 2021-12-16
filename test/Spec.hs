{-# LANGUAGE QuasiQuotes #-}
module Main (main) where
import Test.Hspec
import Advent2021
import Day3
-- import Text.RawString.QQ (r)
import NeatInterpolation (text)
import qualified Data.Text as T

main :: IO ()
main = hspec $ do
  describe "day1" $ do
    it "solves the example input for part 1" $ do
      day1p1 test1 `shouldBe` "7"
    it "solves the example input for part 2" $ do
      day1p2 test1 `shouldBe` "5"

  describe "day2" $ do
    it "solves the example input for part 1" $ do
      day2p1 test2 `shouldBe` "150"
    it "solves the example input for part 2" $ do
      day2p2 test2 `shouldBe` "900"
  
  describe "day3" $ do
    it "solves the example input for part 1" $ do
      day3p1 test3 `shouldBe` "198"
    -- it "solves the example input for part 2" $ do
    --   day3p2 test3 `shouldBe` "3"

test1 :: String
test1 = T.unpack [text|
         199
         200
         208
         210
         200
         207
         240
         269
         260
         263
         |]

test2 :: String
test2 = T.unpack [text|
         forward 5
         down 5
         forward 8
         up 3
         down 8
         forward 2
         |]

test3 :: String
test3 = T.unpack [text|
  00100
  11110
  10110
  10111
  10101
  01111
  00111
  11100
  10000
  11001
  00010
  01010
  |]