module WordCount (wordCount) where

import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Data.Char (isAlphaNum, toLower)

wordCount :: String -> Map.Map String Int
wordCount = foldl' (flip counter) Map.empty . split (not . isAlphaNum) . map toLower
  where counter x = Map.insertWith (+) x 1

split :: (Char -> Bool) -> String -> [String]
split p sentence = case dropWhile p sentence of
  "" -> []
  s' -> word : split p s''
    where (word, s'') = break p s'
