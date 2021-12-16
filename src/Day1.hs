module Day1
    ( day1
    ) where

-- | Solution for day 1
day1 :: String -> String
day1 = show . solution . map read . lines

solution :: [Int] -> Int
solution xs = length . filter id $ zipWith (<) xs (tail xs)
