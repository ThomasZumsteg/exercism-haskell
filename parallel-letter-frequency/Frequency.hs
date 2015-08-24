module Frequency (frequency) where

import qualified Data.Map.Strict as M
import Data.Text (Text, foldl', toLower)
import Data.Char (isLetter)

frequency :: Int -> [Text] -> M.Map Char Int
frequency _ = M.unionsWith (+) . map countLetters

countLetters :: Text -> M.Map Char Int
countLetters = foldl' incrementMap M.empty . toLower
  where
    incrementMap m e
      | e `M.member` m = M.adjust succ e m
      | isLetter e = M.insert e 1 m
      | otherwise = m
