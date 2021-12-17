{- |
Solutions for day 8 of Advent of Code 2021.
-}
module Day8
    ( day8p1
    , day8p2
    ) where
import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char
    ( string, hspace1, letterChar, newline )
import Data.Bifunctor (second)
import Data.List (sortOn, groupBy, delete, sort, foldl')
import Data.Function (on)
import Data.Foldable (foldlM)
import Data.Tuple (swap)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import Control.Monad (guard)
import qualified Data.EnumSet as ES

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
easyDigits = filter isEasy . concatMap snd

segmentLengths :: [[(Int, Int)]]
segmentLengths = groupBy ((==) `on` snd) . sortOn snd $ map (second length) numbers

uniqueLengths :: [Int]
uniqueLengths = map snd uniqueLengthPairs

uniqueLengthPairs :: [(Int, Int)]
uniqueLengthPairs = concatMap getUnique segmentLengths

getUnique :: [a] -> [a]
getUnique [x] = [x]
getUnique _ = []

numbers :: [(Int, [Char])]
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

type Charset = ES.EnumSet Char

numbers' :: [(Int,Charset)]
numbers' = map (second ES.fromList) numbers

displayMap :: M.Map Charset Int
displayMap = M.fromList $ map swap numbers'

solution2 :: Input -> Int
-- solution2 = sum . map (uncurry solveLine)
solution2 = sum . map (uncurry solveLine)

solveLine :: [String] -> [String] -> Int
-- solveLine = undefined
-- solveLine key _vals = solveKey key
solveLine key = digitsToNumber . map (lookupDigit $ solveKey key)

digitsToNumber :: [Int] -> Int
digitsToNumber = foldl' (\acc x -> acc * 10 + x) 0

lookupDigit :: M.Map Char Char -> String -> Int
lookupDigit myMap str =
  fromJust $ (`M.lookup` displayMap) . ES.fromList =<< traverse (`M.lookup` myMap) str

score :: String -> (Bool, Int)
score xs = (not $ isEasy xs, length xs)

isEasy :: String -> Bool
isEasy = (`elem` uniqueLengths) . length

-- solveKey :: [String] -> [(Char, Char)]
-- solveKey :: [String] -> [M.Map Char Char]
solveKey :: [String] -> M.Map Char Char
solveKey = fst . head . foldlM solveStep (M.empty, []) . sortOn score

solveStep :: (M.Map Char Char, [Int]) -> String -> [(M.Map Char Char, [Int])]
solveStep (currentMap, ns) str = do
  -- traceM $ "solveStep start: " ++ show currentMap ++ " " ++ show ns ++ " " ++ show str
  (n, chars) <- filter ((length str ==) . length . snd) numbers
  -- traceM $ "solveStep mid1: " ++ show (n, chars)
  -- (newMap, newUnused) <- foldlM tryInsert (currentMap, unusedChars) $ zip str chars
  (newMap,"") <- foldlM solveWord (currentMap, chars) str
  -- traceM $ "solveStep mid2: " ++ show (newMap, newUnused)
  -- _n <- maybeToList $ (`M.lookup` displayMap) . sort =<< traverse (`M.lookup` newMap) str
  -- traceM $ "solveStep success: " ++ show (newMap, newUnused) ++ " " ++ show _n
  return (newMap, n:ns)

-- tryInsert :: (M.Map Char Char, [Char]) -> (Char, Char) -> [(M.Map Char Char, [Char])]
-- tryInsert (currMap, unusedChars) (k, v) = do
--   let oldValue = M.lookup k currMap
--   guard $ isNothing oldValue || oldValue == Just v
--   return (M.insert k v currMap, unusedChars)


solveWord :: (M.Map Char Char, [Char]) -> Char -> [(M.Map Char Char, String)]
-- solveWord (currMap, unusedChars) [] = return (currMap, unusedChars)
solveWord (currMap, unusedChars) x = do
  case x `M.lookup` currMap of
    Just y -> do
      guard $ y `elem` unusedChars
      return (currMap, delete y unusedChars)
    Nothing -> do
      (y, newUnused) <- select unusedChars
      return (M.insert x y currMap, newUnused)

select :: [a] -> [(a,[a])]
select [] = []
select (x:xs) = (x,xs) : fmap (second (x:)) (select xs)

