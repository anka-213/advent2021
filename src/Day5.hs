{- |
Solutions for day 5 of Advent of Code 2021.
-}
module Day5
    ( day5p1
    , day5p2
    ) where

-- | Solution for day 5 part 1
day5p1 :: String -> String
day5p1 = show . solution1 . parseit

-- | Solution for day 5 part 2
day5p2 :: String -> String
day5p2 = show . solution2 . parseit

type Input = ()

parseit :: String -> Input
parseit = undefined

solution1 :: Input -> Int
solution1 = undefined
solution2 :: Input -> Int
solution2 = undefined