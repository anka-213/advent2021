{- |
Solutions for day 4 of Advent of Code 2021.
-}
module Day4
    ( day4p1
    , day4p2
    ) where

-- | Solution for day 4 part 1
day4p1 :: String -> String
day4p1 = show . solution1 . parseit

-- | Solution for day 4 part 2
day4p2 :: String -> String
day4p2 = show . solution2 . parseit

type Input = ()

parseit :: String -> Input
parseit = undefined

solution1 :: Input -> Int
solution1 = undefined
solution2 :: Input -> Int
solution2 = undefined