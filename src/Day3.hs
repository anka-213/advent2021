{- |
Solutions for day 3 of Advent of Code 2021.
-}
module Day3
    ( day3p1
    , day3p2
    ) where
import Data.List (transpose, foldl')

import Data.Bits

-- | Solution for day 3 part 1
day3p1 :: String -> String
day3p1 = show . solution1 . parseit

-- | Solution for day 3 part 2
day3p2 :: String -> String
day3p2 = show . solution2 . parseit

type Input = [[Bit]]
data Bit = O | I
  deriving stock (Enum)

parseBit :: Char -> Bit
parseBit '0' = O
parseBit '1' = I
parseBit x = error $ "Invalid bit: " ++ show x

parseit :: String -> Input
parseit = (map . map) parseBit . lines

solution1 :: Input -> Int
solution1 = timesComplement . map (fromSign . sum) . transpose . map2 parity

timesComplement :: [Bit] -> Int
timesComplement x = fromBits x * fromBits (map neg x)

neg :: Bit -> Bit
neg O = I
neg I = O


fromBits :: [Bit] -> Int
fromBits = foldl' (\acc x -> acc `shiftL` 1 + fromEnum x) 0

fromSign :: Int -> Bit
fromSign x | x > 0 = I
           | x < 0 = O
           | otherwise = error "Invalid sign"

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 = map . map

parity :: Bit -> Int
parity O = -1
parity I = 1

solution2 :: Input -> Int
solution2 = undefined