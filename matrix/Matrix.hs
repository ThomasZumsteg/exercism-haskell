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
fromString = fromList . map readLine . lines

readLine :: Read a => String -> [a]
readLine l = case reads l of
  [] -> []
  (e, es): _ -> e : readLine es

fromList :: [[a]] -> Matrix a
fromList [] = Matrix V.empty 0 0
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
column c m = V.fromList $ takeNth (c + 1) $ V.toList $ cells m
  where
    takeNth n a = case drop (n - 1) a of
      (y:ys) -> y : takeNth (cols m) ys
      [] -> []

shape :: Matrix a -> (Int, Int)
shape m = (rows m, cols m)

transpose :: Matrix a -> Matrix a
transpose m = fromList $ [V.toList $ column (c - 1) m | c <- [1..(cols m)]]

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (r, c) (Matrix v _ _) = Matrix v r c

flatten :: Matrix a -> V.Vector a
flatten = cells
