{- |
Solutions for day 1 of Advent of Code 2021.
-}
module Day1
    ( day1p1
    ) where

-- | Solution for day 1 part 1
day1p1 :: String -> String
day1p1 = show . solution . map read . lines

-- | Count the number of increments in the list
solution :: [Int] -> Int
solution xs = length . filter id $ zipWith (<) xs (tail xs)
