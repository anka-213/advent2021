{-# LANGUAGE NamedFieldPuns #-}
{- |
Solutions for day 17 of Advent of Code 2021.
-}
module Day17
    ( day17p1
    , day17p2
    ) where
import Text.Megaparsec hiding (Pos, State)
import Data.Void (Void)
import Text.Megaparsec.Char.Lexer (decimal, signed)
import Text.Megaparsec.Char (newline, string)
import Data.List (find)
import Data.Maybe (isJust)
import Debug.Trace (trace, traceM)
import Control.Monad (guard)

-- | Solution for day 17 part 1
day17p1 :: String -> String
-- day17p1 = unlines . map show . solution1 . parseit
day17p1 = show . solution1 . parseit

-- | Solution for day 17 part 2
day17p2 :: String -> String
day17p2 = show . solution2 . parseit

type Input = Area

data Area = Area
    { xRange :: Range
    , yRange :: Range
    } deriving stock (Show, Eq)

data Range = Range Int Int
  deriving stock (Show, Eq)


xMax :: Area -> Int
xMax (Area (Range _ x) _) = x

-- xMin :: Area -> Int
-- xMin (Area (Range x _) _) = x

yMin :: Area -> Int
yMin (Area _ (Range y _)) = y

-- yMax :: Area -> Int
-- yMax (Area _ (Range _ y)) = y

type Parser = Parsec Void String

parseit :: String -> Input
parseit s = case parse (pInput <* optional newline <* eof) "input" s of
    Left err -> error $ errorBundlePretty err
    Right x -> x

pNumber :: Parser Int
pNumber = signed (pure ()) decimal

pInput :: Parser Input
pInput = Area <$ string "target area: x=" <*> pRange <* string ", y=" <*> pRange

pRange :: Parser Range
pRange = Range <$> pNumber <* string ".." <*> pNumber

inRange :: Range -> Int -> Bool
inRange (Range x1 x2) x = x1 <= x && x <= x2

data Pos = Pos { px :: Int, py :: Int }
  deriving stock (Show, Eq)

inArea :: Area -> Pos -> Bool
inArea (Area rx ry) (Pos x y) = inRange rx x && inRange ry y

data Vel = Vel { vx :: Int, vy :: Int }
  deriving stock (Show, Eq)

data State = State
    { pos :: Pos
    , vel :: Vel
    } deriving stock (Show, Eq)

updateState :: State -> State
updateState State {pos=Pos{px,py}, vel=Vel{vx,vy}} =
     State
      { pos = Pos (px + vx ) (py + vy )
      , vel = Vel (vx - signum vx) (vy - 1)
      }

canLand :: Area -> State -> Bool
canLand area
        State {pos=Pos{px,py}}
        = px <= xMax area && py >= yMin area

hasLanded :: Area -> State -> Bool
hasLanded area State{pos=Pos{px,py}} = inArea area (Pos px py)


isSolution :: Area -> State -> Bool
isSolution area state = isJust . find (hasLanded area) $ states area state

states :: Area -> State -> [State]
states area state = takeWhile (canLand area) $ iterate updateState state

{-

step 1: x+=vx; y+=vy;
step 2: vx = vx - signum vx; vy = vy - signum vy;
step 3: vy = vy - 1; 

-}

mkState :: Vel -> State
mkState = State (Pos 0 0)

-- tooHighXVel :: Area -> [State] -> Bool
-- tooHighXVel area = isJust . find (\State{pos} -> px pos > xMax area && py pos > yMax area)

solution1 :: Input -> Int
-- solution1 area = [trace (showStates area vel) (i,j) | i <- [0..20], j <- [0..20], let vel = Vel i j, isSolution area (mkState vel)]
solution1 = maximum . map snd . calcSolutions

calcSolutions :: Area -> [((Int, Int), Int)]
calcSolutions area = do
    yVel <- [yMin area.. -yMin area]
    -- traceM $ "yVel: " ++ show yVel
    (xVel, sts) <- xVels area yVel
    -- traceM $ "xVel: " ++ show xVel
    guard $ isSolution area (mkState (Vel xVel yVel))
    -- traceM $ unlines $ map simpleShow sts
    let yTop = maximum $ map (py . pos) sts
    -- traceM $ "yTop: " ++ show yTop
    pure  ((xVel, yVel), yTop)

xVels :: Area -> Int -> [(Int, [State])]
-- xVels area yVel = takeWhile (not . tooHighXVel area . snd) [(i, states area (mkState vel)) | i <- [0..xMax area], let vel = Vel i yVel]
xVels area yVel = [(i, states area (mkState vel)) | i <- [0..xMax area], let vel = Vel i yVel]


-- showStates :: Area -> Vel -> String
-- showStates area vel = unlines . map simpleShow $ states area $ mkState vel

-- simpleShow :: State -> String
-- simpleShow State {pos=Pos{px,py}, vel=Vel{vx,vy}} = show px ++ "," ++ show py ++ "  " ++ show vx ++ "," ++ show vy



solution2 :: Input -> Int
solution2 = length . calcSolutions