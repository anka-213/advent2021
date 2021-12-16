{-# LANGUAGE TupleSections #-}
{- |
Solutions for day 4 of Advent of Code 2021.
-}
module Day4
    ( day4p1
    , day4p2
    ) where
import Text.Megaparsec (parse, Parsec, errorBundlePretty, sepBy1, some, MonadParsec (try), endBy1)
import Data.Void (Void)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char, space1, hspace1, hspace, newline)
import Data.List (find, transpose)
import Data.Maybe (listToMaybe, mapMaybe)

-- | Solution for day 4 part 1
day4p1 :: String -> String
day4p1 = show . solution1 . parseit

-- | Solution for day 4 part 2
day4p2 :: String -> String
day4p2 = show . solution2 . parseit

type Input = ([Int], [BingoBoard])

newtype BingoBoard = BingoBoard
    { bingoBoard :: [[(Int,Bool)]]
    } deriving stock (Show, Eq)

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
-- solution1 (rolls, boards) = scoreBoard . findMaybe (find winningBoard) $ scanl (flip $ map . addRoll) boards rolls
solution1 = uncurry solution1'

solution1' :: [Int] -> [BingoBoard] -> Int
solution1' [] _ = error "no winning board"
solution1' (n : ns) bbs
  | Just wb <- find winningBoard nextBoards = scoreBoard n wb
  | otherwise = solution1' ns nextBoards
  where nextBoards = map (addRoll n) bbs

scoreBoard :: Int -> BingoBoard -> Int
scoreBoard n bb = (*n) $ sum . map fst . filter unmarked . concat $ bingoBoard bb

winningBoard :: BingoBoard -> Bool
winningBoard (BingoBoard bb) = any winningRow bb || winningCol bb || winningDiag bb

winningDiag :: [[(Int, Bool)]] -> Bool
winningDiag bb = winningRow (firstDiag bb) || winningRow (secondDiag bb)

firstDiag :: [[a]] -> [a]
firstDiag (xs:xss) = head xs : firstDiag (tail xss)
firstDiag [] = []

secondDiag :: [[a]] -> [a]
secondDiag = firstDiag . reverse

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

solution2 :: Input -> Int
solution2 = undefined