module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)
import Data.Set (member, fromList)

scoreLetter :: Char -> Int
scoreLetter l
  | within "AEIOULNRST" = 1
  | within "DG"         = 2
  | within "BCMP"       = 3
  | within "FHVMWY"     = 4
  | within "K"          = 5
  | within "JX"         = 8
  | within "QZ"         = 10
  | otherwise           = 0
  where within = member (toUpper l) . fromList

scoreWord :: String -> Int
scoreWord = sum . map scoreLetter