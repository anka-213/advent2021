module Main (main) where
import Test.Hspec
import qualified Data.List as List
import Advent2021
-- import Text.RawString.QQ (r)

main :: IO ()
main = hspec $ do
  describe "hello" $ do
    it "begins with \"Hello\"" $ do
      List.isPrefixOf "Hello" hello `shouldBe` True
  describe "day1" $ do
    it "solves the example input for part 1" $ do
      day1p1 test1 `shouldBe` "7"
    it "solves the example input for part 2" $ do
      day1p2 test1 `shouldBe` "5"

test1 :: String
test1 = "\
         \199\n\
         \200\n\
         \208\n\
         \210\n\
         \200\n\
         \207\n\
         \240\n\
         \269\n\
         \260\n\
         \263\n\
         \"