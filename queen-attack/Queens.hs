module Queens (boardString, canAttack) where

import Data.List (foldl')

type Queen = (Int, Int)

board :: [String]
board = [ 
  "_ _ _ _ _ _ _ _\n",
  "_ _ _ _ _ _ _ _\n",
  "_ _ _ _ _ _ _ _\n",
  "_ _ _ _ _ _ _ _\n",
  "_ _ _ _ _ _ _ _\n",
  "_ _ _ _ _ _ _ _\n",
  "_ _ _ _ _ _ _ _\n",
  "_ _ _ _ _ _ _ _\n" 
  ]

boardString :: Maybe Queen -> Maybe Queen -> String
boardString Nothing Nothing = concat board
boardString Nothing (Just black) = concat $ insert 'B' black board
boardString (Just white) Nothing = concat $ insert 'W' white board
boardString (Just white) (Just black) = concat $ insert 'W' white $ insert 'B'black board

insert :: Char -> Queen -> [String] -> [String]
insert q (x, y) b = foldl' mod_line [] b
  where
    mod_line acc line
      | length acc /= x = acc ++ [line] 
      | otherwise = acc ++ [foldl' update_line "" line]
      where
        update_line acc' char
          | length acc' /= 2 * y = acc' ++ [char]
          | otherwise = acc' ++ [q]

canAttack :: Queen -> Queen -> Bool
canAttack (black_x, black_y) (white_x, white_y) = diagonal || straight
  where
    diagonal = abs (black_x - white_x) == abs (black_y - white_y) 
    straight = black_x == white_x || black_y == white_y 
