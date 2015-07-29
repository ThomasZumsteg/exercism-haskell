module Series (digits, slices, largestProduct) where

import Data.Char (digitToInt)

digits :: String -> [Int]
digits = map digitToInt

slices :: Int -> String -> [[Int]]
slices n l 
  | length l < n || n == 0 = [[1]]
  | otherwise = [ slice i n $ digits l | i <- [0..(length l - n)] ]
  where 
    slice start items = take items . drop start

largestProduct :: Int -> String -> Int
largestProduct n = maximum . map (foldl1 (*)) . slices n
