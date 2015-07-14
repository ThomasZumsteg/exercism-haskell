module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)

scoreLetter :: Char -> Int
scoreLetter l
  | elem l' "AEIOULNRST" = 1
  | elem l' "DG"         = 2
  | elem l' "BCMP"       = 3
  | elem l' "FHVMWY"     = 4
  | elem l' "K"          = 5
  | elem l' "JX"         = 8
  | elem l' "QZ"         = 10
  | otherwise            = 0
  where l' = toUpper l

scoreWord :: String -> Int
scoreWord = sum . map scoreLetter