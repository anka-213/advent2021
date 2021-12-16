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
    it "has a solution" $ do
      day1 test1 `shouldBe` "7"

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