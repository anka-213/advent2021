{- |
Solutions for day 3 of Advent of Code 2021.
-}
module Day3
    ( day3p1
    , day3p2
    , oxygenGeneratorRatingFull
    , co2ScrubberRatingFull
    ) where
import Data.List (transpose, foldl', sort, group)

import Data.Bits
import Data.Bifunctor (first)
import Control.Monad (join)

-- | Solution for day 3 part 1
day3p1 :: String -> String
day3p1 = show . solution1 . parseit

-- | Solution for day 3 part 2
day3p2 :: String -> String
day3p2 = show . solution2 . parseit

type Input = [[Bit]]
data Bit = O | I
  deriving stock (Enum, Eq, Show, Ord)

parseBit :: Char -> Bit
parseBit '0' = O
parseBit '1' = I
parseBit x = error $ "Invalid bit: " ++ show x

parseit :: String -> Input
parseit = (map . map) parseBit . lines

solution1 :: Input -> Int
solution1 = timesComplement . map countFrequency . transpose

timesComplement :: [[(Int, Bit)]] -> Int
timesComplement x = fromBits (map mostCommon x) * fromBits (map leastCommon x)

mostCommon :: Ord a => [(Int, a)] -> a
mostCommon = snd . maximum

leastCommon :: Ord a => [(Int, a)] -> a
leastCommon = snd . minimum


fromBits :: [Bit] -> Int
fromBits = foldl' (\acc x -> acc `shiftL` 1 + fromEnum x) 0

solution2 :: Input -> Int
solution2 xs = oxygenGeneratorRating xs * co2ScrubberRating xs

countFrequency :: (Ord a) =>[a] -> [(Int, a)]
countFrequency = map (\xs -> (length xs, head xs)) . group . sort

-- | The first part of the solution for day 3 part 2
oxygenGeneratorRatingFull :: String -> Int
oxygenGeneratorRatingFull = oxygenGeneratorRating . parseit

oxygenGeneratorRating :: Input -> Int
oxygenGeneratorRating = fromBits . oxygenGeneratorRatingLoop . map (join (,))

oxygenGeneratorRatingLoop :: [([Bit], [Bit])] -> [Bit]
oxygenGeneratorRatingLoop [x] = snd x
oxygenGeneratorRatingLoop xs = oxygenGeneratorRatingLoop . map (first tail) $ filter (\x -> head (fst x) == commonBit) xs
  where commonBit = mostCommon . countFrequency $ map (head . fst) xs

-- | The second part of the solution for day 3 part 2
co2ScrubberRatingFull :: String -> Int
co2ScrubberRatingFull = co2ScrubberRating . parseit

co2ScrubberRating :: Input -> Int
co2ScrubberRating = fromBits . co2ScrubberRatingLoop . map (join (,))

co2ScrubberRatingLoop :: [([Bit], [Bit])] -> [Bit]
co2ScrubberRatingLoop [x] = snd x
co2ScrubberRatingLoop xs = co2ScrubberRatingLoop . map (first tail) $ filter (\x -> head (fst x) == rareBit) xs
  where rareBit = leastCommon . countFrequency $ map (head . fst) xs