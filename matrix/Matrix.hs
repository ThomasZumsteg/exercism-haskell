module Matrix (
  Matrix,
  row, column,
  rows, cols,
  shape,
  transpose, reshape,
  flatten,
  fromString, fromList,
  ) where

import qualified Data.Vector as V

data Matrix a = Matrix { 
  cells :: V.Vector a,
  rows :: Int,
  cols :: Int
  } deriving (Show, Eq)

fromString :: Read a => String -> Matrix a
fromString = fromList . map (map read . words) . lines

fromList :: [[a]] -> Matrix a
fromList m =
  let
    c = length $ head m
    r = length m
    v = V.fromList $ concat m
  in
    Matrix v r c

row :: Int -> Matrix a -> V.Vector a
row r m = V.take (cols m) $ snd $ V.splitAt (r * (cols m)) $ cells m

column :: Int -> Matrix a -> V.Vector a
column c m = 
  let xs = cells m
  in case drop (c - 1)  of
  (y:ys) -> y : column (cols m) ys
  [] -> []


columns :: Matrix a -> Int
columns = undefined

shape :: Matrix a -> (Int, Int)
shape m = (rows m, columns m)

transpose = undefined

reshape = undefined

flatten = undefined
