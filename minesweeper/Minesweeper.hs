module Minesweeper (annotate) where

import Data.List (foldl')
import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate m = map (\(r, row) -> map (\(c, s) -> findMines r c s) $ enum row) $ enum m
  where
    findMines r c e 
      | e == ' ' = 
        case countMines r c m of
          0 -> ' '
          n -> intToDigit n
      | otherwise = e

enum :: [a] -> [(Int, a)]
enum = zip [0..]

countMines :: Int -> Int -> [String] -> Int
countMines r c minefield = 
  let
    mine = '*'
    adjacent = concat $ map (slice (c-1) (c+1)) $ slice (r-1) (r+1) minefield
  in
    foldl' (\acc s -> if s == mine then succ acc else acc) 0 adjacent

slice :: Int -> Int -> [a] -> [a]
slice from to =
  let items = to + 1 - (if from >= 0 then from else 0)
  in take items . drop from