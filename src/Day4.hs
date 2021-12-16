{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{- |
Solutions for day 4 of Advent of Code 2021.
-}
module Day4
    ( day4p1
    , day4p2
    ) where
import Text.Megaparsec (parse, Parsec, errorBundlePretty, sepBy1, endBy1)
import Data.Void (Void)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char, hspace1, hspace, newline)
import Data.List (transpose, partition)

-- | Solution for day 4 part 1
day4p1 :: String -> String
day4p1 = show . solution1 . parseit

-- | Solution for day 4 part 2
day4p2 :: String -> String
day4p2 = show . solution2 . parseit

type Input = ([Int], [BingoBoard])

newtype BingoBoard = BingoBoard
    { bingoBoard :: [[(Int,Bool)]]
    } deriving stock (Eq)

instance Show BingoBoard where
    show (BingoBoard b) = unlines $ map (unwords . map showCell) b

showCell :: (Int, Bool) -> String
showCell (n, True) = "X" ++ show n
showCell (n, False) = " " ++ show n

type Parser = Parsec Void String

parseit :: String -> Input
parseit s = case parse pInput "input" s of
    Left err -> error $ errorBundlePretty err
    Right x -> x

pInput :: Parser Input
pInput = (,) <$> pLine <* newline <* newline <*> pBoards

pLine :: Parser [Int]
pLine = decimal `sepBy1` char ','

pBoards :: Parser [BingoBoard]
pBoards = pBoard `sepBy1` newline

pBoard :: Parser BingoBoard
pBoard = BingoBoard <$> (hspace *> ((,False) <$> decimal) `sepBy1` hspace1) `endBy1` newline

solution1 :: Input -> Int
solution1 = head . winningScores

scoreBoard :: Int -> BingoBoard -> Int
scoreBoard n bb = (*n) $ sum . map fst . filter unmarked . concat $ bingoBoard bb

winningBoard :: BingoBoard -> Bool
winningBoard (BingoBoard bb) = any winningRow bb || winningCol bb || winningDiag bb

winningDiag :: [[(Int, Bool)]] -> Bool
winningDiag _ = False
-- winningDiag bb = winningRow (firstDiag bb) || winningRow (secondDiag bb)

-- firstDiag :: Show a => [[a]] -> [a]
-- firstDiag (xs@(_:_):xss) = head xs : firstDiag (map tail xss)
-- firstDiag [] = []
-- firstDiag xs = error $ "firstDiag: " ++ show xs

-- secondDiag :: Show a => [[a]] -> [a]
-- secondDiag = firstDiag . reverse

winningCol :: [[(Int, Bool)]] -> Bool
winningCol = any winningRow . transpose

winningRow :: [(Int,Bool)] -> Bool
winningRow = all marked

marked :: (a, b) -> b
marked = snd
unmarked :: (a, Bool) -> Bool
unmarked = not . snd

addRoll :: Int -> BingoBoard -> BingoBoard
addRoll n = BingoBoard . map (map (\(i,b) -> if i == n then (i,True) else (i,b))) . bingoBoard

-- findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
-- findMaybe f = listToMaybe . mapMaybe f


winningBoardOrder :: [Int] -> [BingoBoard] -> [(Int, BingoBoard)]
winningBoardOrder [] _ = []
winningBoardOrder (n : ns) boards = map (n,) winning ++ winningBoardOrder ns loosing
  where (winning, loosing) = partition winningBoard nextBoards
        nextBoards = map (addRoll n) boards

winningScores :: Input -> [Int]
winningScores = map (uncurry scoreBoard) . uncurry winningBoardOrder

solution2 :: Input -> Int
solution2 = last . winningScores
