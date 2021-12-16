{-# LANGUAGE QuasiQuotes #-}
module Main (main) where
import Test.Hspec
import Advent2021
import Day3
-- import Text.RawString.QQ (r)
import NeatInterpolation (text)
import qualified Data.Text as T
import Day4

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
    it "solves the oxygen generator rating for part 2" $ do
      oxygenGeneratorRatingFull test3 `shouldBe` 23
    it "solves the CO2 scrubber rating for part 2" $ do
      co2ScrubberRatingFull test3 `shouldBe` 10
    it "solves the example input for part 2" $ do
      day3p2 test3 `shouldBe` "230"
  
  describe "day4" $ do
    it "solves the example input for part 1" $ do
      day4p1 test4 `shouldBe` "4512"
    -- it "solves the example input for part 2" $ do
    --   day4p2 test4 `shouldBe` ""

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

test4 :: String
test4 = (++"\n") $ T.unpack [text|
  7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

  22 13 17 11  0
   8  2 23  4 24
  21  9 14 16  7
   6 10  3 18  5
   1 12 20 15 19

   3 15  0  2 22
   9 18 13 17  5
  19  8  7 25 23
  20 11 10 24  4
  14 21 16 12  6

  14 21 17 24  4
  10 16 15  9 19
  18  8 23 26 20
  22 11 13  6  5
   2  0 12  3  7

  |]