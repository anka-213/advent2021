{-# LANGUAGE TupleSections #-}
{- |
Solutions for day 6 of Advent of Code 2021.
-}
module Day6
    ( day6p1
    , day6p2
    ) where

import qualified Data.IntMap.Strict as M
import Text.Megaparsec
    ( MonadParsec(eof), parse, errorBundlePretty, sepBy1, Parsec )
import Data.Void ( Void )
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char, newline)

-- | Solution for day 6 part 1
day6p1 :: String -> String
day6p1 = show . solution1 . parseit

-- | Solution for day 6 part 2
day6p2 :: String -> String
day6p2 = show . solution2 . parseit

type Input = [Int]

step :: (Int, Int) -> [(Int, Int)]
step (0, n) = [(6, n), (8,n)]
step (day, n) = [(day - 1, n)]

stepAll :: M.IntMap Int -> M.IntMap Int
stepAll = M.fromListWith (+) . concatMap step . M.toList

type Parser = Parsec Void String
parseit :: String -> Input
parseit s = case parse (pInput <* eof) "input" s of
    Left err -> error $ errorBundlePretty err
    Right x -> x
-- parseit s | [(n, "")] <- readP_to_S parseInput s = n
--           | otherwise = error "parse error"

pInput :: Parser Input
pInput = decimal `sepBy1` char ',' <* newline


inputToState :: Input -> M.IntMap Int
inputToState = M.fromListWith (+) . fmap (, 1)

afterNDays :: Int -> Input -> Int
afterNDays n = (!! n) . map (sum . map snd . M.toList) . iterate stepAll . inputToState

-- solution1 :: Input -> Int
solution1 :: Input -> Int
solution1 = afterNDays 80

solution2 :: Input -> Int
solution2 = afterNDays 256