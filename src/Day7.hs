{- |
Solutions for day 7 of Advent of Code 2021.
-}
module Day7
    ( day7p1
    , day7p2
    ) where
import Text.Megaparsec
import Data.Void (Void)

-- | Solution for day 7 part 1
day7p1 :: String -> String
day7p1 = show . solution1 . parseit

-- | Solution for day 7 part 2
day7p2 :: String -> String
day7p2 = show . solution2 . parseit

type Input = ()

type Parser = Parsec Void String

parseit :: String -> Input
parseit s = case parse (pInput <* eof) "input" s of
    Left err -> error $ errorBundlePretty err
    Right x -> x

pInput :: Parser Input
pInput = undefined

solution1 :: Input -> Int
solution1 = undefined

solution2 :: Input -> Int
solution2 = undefined