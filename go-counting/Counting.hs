module Counting (Color(..), territories, territoryFor, makeBoard, grouper) where

import qualified Data.Set as Set
import Data.List (foldl', foldl1', partition, find)

data Color = Black | White deriving (Eq, Ord, Show)

type Coord = (Int, Int)

territories :: [[Char]] -> [(Set.Set Coord, Maybe Color)]
territories lineBoard = map (findOwners board) $ foldl' grouper [] spaces
  where
    board = makeBoard lineBoard
    isSpace = (Nothing ==) . fst
    spaces = map snd $ filter isSpace board

grouper :: [Set.Set Coord] -> Coord -> [Set.Set Coord]
grouper acc pos
  | null con = (Set.singleton pos) : acc
  | otherwise = (Set.insert pos $ Set.unions con) : uncon
  where
    (con, uncon) = partition (any (isAdjacent pos)) acc

isAdjacent :: Coord -> Coord -> Bool
isAdjacent (c, r) (c', r') = (abs(c - c') + abs(r - r')) == 1

territoryFor :: [[Char]] -> Coord -> Maybe (Set.Set Coord, Maybe Color)
territoryFor board coord = find (Set.member coord . fst) territoryList
  where
    territoryList = territories board

findOwners :: [(Maybe Color, Coord)] -> Set.Set Coord -> (Set.Set Coord, Maybe Color)
findOwners board set 
  | null neighbors = (set, Nothing)
  | otherwise = (set, foldl1' sameColor neighbors)
  where
    neighbors = map fst $ filter (\(color, cord) -> color /= Nothing && any (isAdjacent cord) set) board
    sameColor acc c = if acc == c then acc else Nothing

makeBoard :: [[Char]] -> [(Maybe Color, Coord)]
makeBoard board = [(color e, (c, r)) | 
  (r, row) <- enumerate board, 
  (c, e) <- enumerate row ]
  where
    enumerate = zip [1..]
    color color' = case color' of
      'B' -> Just Black
      'W' -> Just White
      _ -> Nothing
