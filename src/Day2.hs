{- |
Solutions for day 2 of Advent of Code 2021.
-}
module Day2
    ( day2p1
    , day2p2
    ) where

-- | Solution for day 2 part 1
day2p1 :: String -> String
day2p1 = show . solution1 . map (parse . words) . lines

-- | Solution for day 2 part 2
day2p2 :: String -> String
day2p2 = show . solution2 . map (parse . words) . lines

data Direction = Fwd | Up | Down 
  deriving stock (Show, Eq)

parse :: [String] -> (Direction, Int)
parse [dir, n] = (readDir dir, read n)
parse _ = error "Invalid input"

readDir :: String -> Direction
readDir "forward" = Fwd
readDir "up"      = Up
readDir "down"    = Down
readDir _         = error "Invalid direction"


-- | Count the number of increments in the list
solution1 :: [(Direction, Int)] -> Int
solution1 = uncurry (*) . foldl updatePos initialPos

-- Positions are (horizontal, depth)
type Pos = (Int, Int)

updatePos :: Pos -> (Direction, Int) -> Pos
updatePos (i, j) (Fwd, n) = (i + n, j)
updatePos (i, j) (Up, n) = (i, j - n)
updatePos (i, j) (Down, n) = (i, j + n)

initialPos :: Pos
initialPos = (0, 0)

data State = State 
  { horiz :: Int
  , depth :: Int
  , aim :: Int
}

initialState :: State
initialState = State 0 0 0

solution2 :: [(Direction, Int)] -> Int
solution2 = posProd . foldl updateState initialState

posProd :: State -> Int
posProd st = horiz st * depth st 

updateState :: State -> (Direction, Int) -> State
updateState st (Fwd, n) = st { horiz = horiz st + n , depth = depth st + n * aim st }
updateState st (Up, n) = st { aim = aim st - n }
updateState st (Down, n) = st { aim = aim st + n }

