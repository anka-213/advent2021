{- |
Copyright: (c) 2021 Andreas Källberg
SPDX-License-Identifier: MIT
Maintainer: Andreas Källberg <anka.213@gmail.com>

See README for more info
-}

module Advent2021
       ( runDay
       , hello
       , day1p1
       ) where

import Day1 (day1p1)
import System.Environment (getArgs)


runDay :: IO ()
runDay = do
  [dayNr, partNr] <- map read <$> getArgs
  let solution = days !! (dayNr - 1) !! (partNr - 1)
  let inputFile = "inputs/day" ++ show dayNr ++ ".txt"
  input <- readFile inputFile
  putStrLn $ "Day " ++ show dayNr ++ " part " ++ show partNr ++ ": " ++ solution input
  pure ()

days :: [[String -> String]]
days = 
  [[day1p1]
  ]


hello :: String
hello = "Hello"