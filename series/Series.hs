module Series (slices) where

import Data.Char (digitToInt)
import Data.List (tails)

slices :: Int -> String -> [[Int]]
slices n l = map (map digitToInt . take n) sliceList
  where sliceList = take (length l - n + 1) $ tails l
