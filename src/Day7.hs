{- |
Solutions for day 6 of Advent of Code 2021.
-}
module Day6
    ( day6p1
    , day6p2
    ) where

-- | Solution for day 6 part 1
day6p1 :: String -> String
day6p1 = show . solution1 . parseit

-- | Solution for day 6 part 2
day6p2 :: String -> String
day6p2 = show . solution2 . parseit

type Input = ()

parseit :: String -> Input
parseit = undefined

solution1 :: Input -> Int
solution1 = undefined
solution2 :: Input -> Int
solution2 = undefined