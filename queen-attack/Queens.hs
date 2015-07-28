module Queens (boardString, canAttack) where

type Queen = (Int, Int)

board :: String
board = unlines $ replicate 8 $ row
  where row = unwords $ replicate 8 "_"

boardString :: Maybe Queen -> Maybe Queen -> String
boardString white black = insert 'B' black $ insert 'W' white $ board

insert :: Char -> Maybe Queen -> String -> String
insert _ Nothing = id
insert c (Just (x, y)) = pack . modify c x y . unpack
  where
    pack = unlines . map (unwords . map (:[]))
    unpack = map (map head . words) . lines

modify :: a -> Int -> Int -> [[a]] -> [[a]]
modify c x y l =
  let
    row = head $ drop x l
    new_row = (take y row) ++ [c] ++ (drop (y + 1) row)
  in
    (take x l) ++ [new_row] ++ (drop (x + 1) l)

canAttack :: Queen -> Queen -> Bool
canAttack (black_x, black_y) (white_x, white_y) = diagonal || straight
  where
    diagonal = abs (black_x - white_x) == abs (black_y - white_y) 
    straight = black_x == white_x || black_y == white_y 
