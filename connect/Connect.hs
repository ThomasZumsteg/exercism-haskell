module Connect (resultFor, Color(Black, White), getNeighbors, makeBoard) where

import qualified Data.Vector as V
import Control.Monad ((>>=))

data Color = Black | White deriving (Eq, Show)

type Pos = (Int, Int)
type Board = V.Vector (V.Vector Char)

resultFor :: [String] -> Maybe Color
resultFor _  = Nothing

makeBoard :: [String] -> Board
makeBoard = V.fromList . map V.fromList

crosses :: Char -> Board -> Pos -> Bool
crosses c board pos@(x, y)
  | y + 1 >= V.length board = True
  | [] == neighbors = False
  | otherwise = any (crosses c $ mod2d x y '#') neighbors
  where
    neighbors = getNeighbors c board pos
    mod2d n m v = board V.// [(m, board V.! m V.// [(n, v)])]

getNeighbors :: Char -> Board -> Pos -> [Pos]
getNeighbors c board (x, y) = [(x', y') 
  | x' <- [x-1..x+1], y' <- [y-1..y+1], 
  (x', y') /= (x, y), Just c == vElem x' y']
  where
    vElem n m = board V.!? n >>= (V.!? m)
