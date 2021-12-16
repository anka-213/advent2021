{- |
Solutions for day 1 of Advent of Code 2021.
-}
module Day1
    ( day1p1
    , day1p2
    ) where

-- | Solution for day 1 part 1
day1p1 :: String -> String
day1p1 = show . solution1 . map read . lines

day1p2 :: String -> String
day1p2 = show . solution2 . map read . lines

-- | Count the number of increments in the list
solution1 :: [Int] -> Int
solution1 xs = length . filter id $ zipWith (<) xs (tail xs)

solution2 :: [Int] -> Int
solution2 xs = solution1 $ zipWith3 (\x y z -> x + y + z) xs (tail xs) (tail $ tail xs)