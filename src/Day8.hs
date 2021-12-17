{- |
Solutions for day 8 of Advent of Code 2021.
-}
module Day8
    ( day8p1
    , day8p2
    ) where
import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Char
import Data.Bifunctor (second)
import Data.List (sortOn, groupBy)
import Data.Function (on)
import qualified Data.Map.Strict as Map
import Data.Foldable (foldlM)
import Data.Tuple (swap)
import Data.Maybe (maybeToList)
import Debug.Trace

-- | Solution for day 8 part 1
day8p1 :: String -> String
day8p1 = show . solution1 . parseit

-- | Solution for day 8 part 2
day8p2 :: String -> String
day8p2 = show . solution2 . parseit

type Input = [([String], [String])]

type Parser = Parsec Void String

parseit :: String -> Input
parseit s = case parse (pInput <* eof) "input" s of
    Left err -> error $ errorBundlePretty err
    Right x -> x

pInput :: Parser Input
pInput = pInputLine `endBy1` newline

pInputLine :: Parser ([String], [String])
pInputLine = (,) <$> pDigitSet <* string "| " <*> pFinalDigits

pDigitSet :: Parser [String]
pDigitSet = some letterChar `endBy1` hspace1

pFinalDigits :: Parser [String]
pFinalDigits = some letterChar `sepBy1` hspace1

solution1 :: Input -> Int
solution1 = length . easyDigits

easyDigits :: Input -> [String]
easyDigits = filter ((`elem`uniqueLengths).length) . concatMap snd

segmentLengths :: [[(Integer, Int)]]
segmentLengths = groupBy ((==) `on` snd) . sortOn snd $ map (second length) numbers

uniqueLengths :: [Int]
uniqueLengths = map snd uniqueLengthPairs

uniqueLengthPairs :: [(Integer, Int)]
uniqueLengthPairs = concatMap getUnique segmentLengths

getUnique :: [a] -> [a]
getUnique [x] = [x]
getUnique _ = []

numbers :: [(Integer, [Char])]
numbers =
  [(0,"abcefg")
  ,(1,"cf")
  ,(2,"acdeg")
  ,(3,"acdfg")
  ,(4,"bcdf")
  ,(5,"abdfg")
  ,(6,"abdefg")
  ,(7,"acf")
  ,(8,"abcdefg")
  ,(9,"abcdfg")
  ]

displayMap :: [(String, Integer)]
displayMap = map swap numbers

-- solution2 :: Input -> Int
-- solution2 = sum . map (uncurry solveLine)
solution2 = map (uncurry solveLine)

-- solveLine :: [String] -> [String] -> Int
-- solveLine = undefined
solveLine key _vals = solveKey key

-- solveKey :: [String] -> [(Char, Char)]
solveKey = fmap fst . foldlM solveStep ([], "abcdefgh")

solveStep :: ([(Char, Char)], [Char]) -> String -> [([(Char, Char)], [Char])]
solveStep (currentMap, unusedChars) str = do
  traceM $ "solveStep start: " ++ show currentMap ++ " " ++ show unusedChars ++ " " ++ show str
  (newMap,newUnused) <- foldlM solveWord (currentMap, unusedChars) str
  -- traceM $ "solveStep mid: " ++ show (newMap, newUnused)
  _n <- maybeToList $ (`lookup` displayMap) =<< traverse (`lookup` newMap) str
  traceM $ "solveStep success: " ++ show (newMap, newUnused)
  return (newMap, newUnused)

solveWord :: ([(Char, Char)], [Char]) -> Char -> [([(Char, Char)], String)]
-- solveWord (currMap, unusedChars) [] = return (currMap, unusedChars)
solveWord (currMap, unusedChars) x = do
  case x `lookup` currMap of
    Just _y -> pure (currMap, unusedChars)
    Nothing -> do
      (y, newUnused) <- select unusedChars
      return (currMap ++ [(x, y)], newUnused)

select :: [a] -> [(a,[a])]
select [] = []
select (x:xs) = (x,xs) : fmap (second (x:)) (select xs)

