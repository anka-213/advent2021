{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{- |
Solutions for day 5 of Advent of Code 2021.
-}
module Day5
    ( day5p1
    , day5p2
    ) where
import Text.Megaparsec
    ( optional, parse, errorBundlePretty, many, Parsec, MonadParsec (eof), sepBy1, endBy1 )
import Data.Void ( Void )
import Text.Megaparsec.Char.Lexer ( decimal )
import Text.Megaparsec.Char (char, newline, string)
import qualified Data.Map as Map

-- | Solution for day 5 part 1
day5p1 :: String -> String
day5p1 = show . solution1 . parseit

-- | Solution for day 5 part 2
day5p2 :: String -> String
day5p2 = show . solution2 . parseit

type Input = [Link]

data Link = Link
  { from :: Coord
  , to :: Coord
  } deriving stock (Show, Eq)

data Coord = Coord
  { horiz :: Int
  , vert :: Int
  } deriving stock (Show, Eq, Ord)

type Parser = Parsec Void String

parseit :: String -> Input
parseit s = case parse (pInput <* eof) "input" s of
    Left err -> error $ errorBundlePretty err
    Right x -> x

pInput :: Parser Input
pInput = pLine `endBy1` newline

pLine :: Parser Link
pLine = Link <$> pCoord <* string " -> " <*> pCoord

pCoord :: Parser Coord
pCoord = Coord <$> decimal <* char ',' <*> decimal

solution1 :: Input -> Int
solution1 = length . filter ((>1) . snd) . Map.toList . Map.fromListWith (+) . map (,1 :: Int) . concatMap allCoords


allCoords :: Link -> [Coord]
allCoords Link {from, to}
  | horiz from == horiz to =
    [ Coord (horiz from) i | i <- [vert from `min` vert to .. vert from `max` vert to] ]
  | vert from == vert to =
    [ Coord i (vert from) | i <- [horiz from `min` horiz to .. horiz from `max` horiz to] ]
  | otherwise = []

-- overlaps :: Link -> Link -> Bool
-- overlaps (Link (Coord xs1 ys1) (Coord xe1 ye1)) (Link (Coord xs2 ys2) (Coord xe2 ye2)) =
--   (xs1 <= xs2 && xs2 <= xe1) && (ys1 <= ys2 && ys2 <= ye1) ||
--   (xs1 <= xe2 && xe2 <= xe1) && (ys1 <= ye2 && ye2 <= ye1)

{-
 |--------|
    |--------|
 

-}

solution2 :: Input -> Int
solution2 = undefined
