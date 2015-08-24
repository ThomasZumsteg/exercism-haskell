module Connect (resultFor, Color(Black, White), crosses, makeGraph, makeBoard, getNeighbors) where

import qualified Data.Vector as V
import Data.Maybe (fromJust)

data Color = Black | White deriving (Eq, Show)

type Pos = (Int, Int)
type Board = V.Vector (V.Vector Char)
type Graph = [(Pos, [Pos])]

resultFor :: [String] -> Maybe Color
resultFor _ = Nothing

crosses :: Board -> Pos -> Bool
crosses _ _ = undefined
--crosses  pos@(x, y)
--  | y == lastLine = True
--  | [] == neighbors = False
--  | otherwise = any () 
--  where
--    lastLine = V.length board - 1

makeGraph :: [String] -> Char -> [(Pos, [Pos])]
makeGraph b c = filter (not . null . snd) $
  [((x,y), getNeighbors (makeBoard b) (x,y)) | 
  x <- [0 .. (length $ head b) - 1], y <- [0 .. (length b) - 1], 
  b !! y !! x == c]

makeBoard :: [String] -> Board
makeBoard = V.fromList . map V.fromList

getNeighbors :: Board -> Pos -> [Pos]
getNeighbors board (x, y) = [(x + x', y + y') 
  | (x', y') <- [(0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1)]
  , Just c == vElem board ((x + x'), (y + y'))]
  where
    c = fromJust $ vElem board (x, y)

vElem :: Board -> Pos -> Maybe Char
vElem b (x, y) = b V.!? y >>= (V.!? x)

mod2d :: Board -> Pos -> Char -> Board
mod2d b (x,y) v = b V.// [(y, b V.! y V.// [(x, v)])]

