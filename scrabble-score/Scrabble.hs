module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)
import qualified Data.Map as Map

scoreLetter :: Char -> Int
scoreLetter l = Map.findWithDefault 0 (toUpper l) letter_scores
  where
    letter_scores = Map.fromList $ concatMap func [
      (1, "AEIOULNRST"),
      (2, "DG"),
      (3, "BCMP"),
      (4, "FHVWY"),
      (5, "K"),
      (8, "JX"),
      (10, "QZ")]
      where
        func (score, letters) = map (\l' -> (l', score)) letters

scoreWord :: String -> Int
scoreWord = sum . map scoreLetter