module Connect (resultFor, Color(Black, White), buildGraph, neighbors) where

data Color = Black | White deriving (Eq, Show)

type Pos = (Int, Int)
type Graph = [Pos]

resultFor :: [String] -> Maybe Color
resultFor board 
  | connected startX [] graphX finishX = Just Black
  | connected startO [] graphO finishO = Just White
  | otherwise = Nothing
  where
    lastRow = length board - 1
    lastCol = (length $ head board) - 1
    graphX = buildGraph 'X' board
    graphO = buildGraph 'O' board
    startX = filter ((0==) . snd) graphX
    startO = filter ((0==) . fst) graphO
    finishX = filter ((lastCol==) . snd) graphX
    finishO = filter ((lastRow==) . fst) graphO

buildGraph :: Char -> [String] -> Graph
buildGraph c board = [ (rowNum, colNum) | 
      (rowNum, row) <- enumerate board,
      (colNum, char) <- enumerate row,
      char == c]
  where
    enumerate = zip [0..]

neighbors :: Pos -> Graph -> Graph
neighbors (x, y) = filter isNeighbor
  where
    isNeighbor (x', y') = ((x-x'), (y-y')) `elem` nearBy
    nearBy = [(-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0)]

connected :: [Pos] -> [Pos] -> [Pos] -> [Pos] -> Bool
connected [] _ _ _ = False
connected _ _ _ [] = False
connected (p:ps) seen graph finished
  | p `elem` finished = True
  | otherwise = connected (ps ++ pNeighbors) (p:seen) graph finished
  where
    pNeighbors = filter (not . flip elem seen) $ neighbors p graph
