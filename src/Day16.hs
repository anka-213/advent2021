{- |
Solutions for day 16 of Advent of Code 2021.
-}
module Day16
    ( day16p1
    , day16p2
    ) where
import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char, newline)

-- | Solution for day 16 part 1
day16p1 :: String -> String
day16p1 = show . solution1 . parseit

-- | Solution for day 16 part 2
day16p2 :: String -> String
day16p2 = show . solution2 . parseit

type Input = [Int]

type Parser = Parsec Void String

parseit :: String -> Input
parseit s = case parse (pInput <* eof) "input" s of
    Left err -> error $ errorBundlePretty err
    Right x -> x

pInput :: Parser Input
pInput = decimal `sepBy1` char ',' <* newline

solution1 :: Input -> Int
solution1 = undefined

solution2 :: Input -> Int
solution2 = undefined