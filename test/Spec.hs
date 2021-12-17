{-# LANGUAGE QuasiQuotes #-}
module Main (main) where
import Test.Hspec
import Advent2021
import Day3
-- import Text.RawString.QQ (r)
import NeatInterpolation (text)
import qualified Data.Text as T
import Day4
import Day5
import Day6
import Day7
import Day8
import Day16
import Day17

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
    it "solves the example input for part 2" $ do
      day4p2 test4 `shouldBe` "1924"
  
  describe "day5" $ do
    it "solves the example input for part 1" $ do
      day5p1 test5 `shouldBe` "5"
    it "solves the example input for part 2" $ do
      day5p2 test5 `shouldBe` "12"
  
  describe "day6" $ do
    it "solves the example input for part 1" $ do
      day6p1 test6 `shouldBe` "5934"
    it "solves the example input for part 2" $ do
      day6p2 test6 `shouldBe` "26984457539"

  describe "day7" $ do
    it "solves the example input for part 1" $ do
      day7p1 test7 `shouldBe` "37"
    it "solves the example input for part 2" $ do
      day7p2 test7 `shouldBe` "168"

  describe "day8" $ do
    it "solves the example input for part 1" $ do
      day8p1 test8 `shouldBe` "26"
    it "solves the example input for part 2a" $ do
      day8p2 test8a `shouldBe` "5353"
    it "solves the example input for part 2" $ do
      day8p2 test8 `shouldBe` "61229"
  
  -- describe "day16" $ do
  --   it "solves the example input for part 1" $ do
  --     day16p1 test16 `shouldBe` ""
  --   -- it "solves the example input for part 2" $ do
  --   --   day16p2 test16 `shouldBe` ""

  describe "day17" $ do
    it "solves the example input for part 1" $ do
      day17p1 test17 `shouldBe` "45"
    it "solves the example input for part 2" $ do
      day17p2 test17 `shouldBe` "112"

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

test5 :: String
test5 = (++"\n") $ T.unpack [text|
0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2
|]

test6 :: String
test6 = (++"\n") $ T.unpack [text|3,4,3,1,2|]

test7 :: String
test7 = (++"\n") $ T.unpack [text|16,1,2,0,4,2,7,1,2,14|]


test8a  :: String
test8a = (++"\n") $ T.unpack [text|
 acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf
 |]

test8 :: String
test8 = (++"\n") $ T.unpack [text|
be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
|]

test16 :: String
test16 = (++"\n") $ T.unpack [text|
  |]

test17 :: String
test17 = (++"\n") $ T.unpack [text|
target area: x=20..30, y=-10..-5
  |]