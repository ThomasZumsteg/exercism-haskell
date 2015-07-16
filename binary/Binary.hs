module Binary (toDecimal) where

import Data.List (foldl')

toDecimal :: String -> Int
toDecimal bin 
  | any (\x -> not ((x == '1') || (x == '0'))) bin = 0
  | otherwise = foldl' binaryAdder 0 bin
  where
    binaryAdder acc x
      | x == '1' = 1 + 2 * acc
      | x == '0' = 0 + 2 * acc
