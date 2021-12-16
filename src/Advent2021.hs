{-# LANGUAGE LambdaCase #-}
{- |
Copyright: (c) 2021 Andreas Källberg
SPDX-License-Identifier: MIT
Maintainer: Andreas Källberg <anka.213@gmail.com>

See README for more info
-}

module Advent2021
       ( runDay
       , day1p1
       , day1p2
       , day2p1
       , day2p2
       ) where

import System.Environment (getArgs)
import System.Directory
import System.Process
import Day1 (day1p1, day1p2)
import Day2 (day2p1, day2p2)
import Day3 ( day3p1, day3p2 )
import Control.Monad (unless)
import System.Exit ( ExitCode(ExitFailure, ExitSuccess) )

-- | Run a day's solution
runDay :: IO ()
runDay = do
  [dayNr, partNr] <- map read <$> getArgs
  let solution = days !! (dayNr - 1) !! (partNr - 1)
  let inputFile = "inputs/day" ++ show dayNr ++ ".txt"
  exists <- doesFileExist inputFile
  unless exists $ downloadFile dayNr
  input <- readFile inputFile
  putStrLn $ "Day " ++ show dayNr ++ " part " ++ show partNr ++ ": " ++ solution input
  pure ()

downloadFile :: Int -> IO ()
downloadFile n = do
  result <- rawSystem "./fetchInput.sh" [show n]
  case  result of 
    ExitSuccess -> pure ()
    ExitFailure x -> error $ "Failed to download input file for day " ++ show n ++ " with exit code " ++ show x

-- | All solutions for all days
days :: [[String -> String]]
days = 
  [[day1p1, day1p2]
  ,[day2p1, day2p2]
  ,[day3p1, day3p2]
  ]
