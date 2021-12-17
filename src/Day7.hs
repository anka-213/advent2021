{- |
Solutions for day 7 of Advent of Code 2021.
-}
module Day7
    ( day7p1
    , day7p2
    ) where
import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char, newline)
import Data.List (sort)

-- | Solution for day 7 part 1
day7p1 :: String -> String
day7p1 = show . solution1 . parseit

-- | Solution for day 7 part 2
day7p2 :: String -> String
day7p2 = show . solution2 . parseit

type Input = [Int]

type Parser = Parsec Void String

parseit :: String -> Input
parseit s = case parse (pInput <* eof) "input" s of
    Left err -> error $ errorBundlePretty err
    Right x -> x

pInput :: Parser Input
pInput = decimal `sepBy1` char ',' <* newline

solution1 :: Input -> Int
solution1 xs = let n = median xs in sum $ map (abs . (n -)) xs
-- solution1 xs = minimum [sum $ map (\x -> abs (x - n)) xs | n <- [minimum xs..maximum xs]]

median :: Ord a => [a] -> a
median = middleElement . sort

middleElement :: [a] -> a
middleElement xs = xs !! (length xs `div` 2)

-- mean :: [Int] -> Int
-- mean xs = sum xs `div` length xs

sumUpTo :: Int -> Int
sumUpTo n = (n * (n + 1)) `div` 2
-- sumUpTo n = sum [0..n]

solution2 :: Input -> Int
-- solution2 xs = let n = mean xs in sum $ map (sumUpTo . abs . (n -)) xs
solution2 xs = minimum [sum $ map (sumUpTo . abs . (n -)) xs | n <- [minimum xs..maximum xs]]