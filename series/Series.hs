module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n l
  | length l < n = []
  | otherwise = slice l : (slices n remainer)
  where
    slice = map digitToInt . take n
    remainer = tail l
    