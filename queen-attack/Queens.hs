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
modify item x y list =
  let
    (first, (row:rest)) = splitAt x list
    (row_first, (_:row_last)) = splitAt y row
  in first ++ [row_first ++ [item] ++ row_last] ++ rest

canAttack :: Queen -> Queen -> Bool
canAttack (black_x, black_y) (white_x, white_y) = diagonal || straight
  where
    diagonal = abs (black_x - white_x) == abs (black_y - white_y) 
    straight = black_x == white_x || black_y == white_y 
