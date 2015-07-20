module Atbash (encode) where

import Data.Char (ord, chr, toLower, isAlphaNum, isAlpha, isAscii)

encode :: String -> String
encode = wordSize 5 . map atbash . filter valid
  where
    valid c = isAlphaNum c && isAscii c
    atbash c
      | isAlpha c = chr (ord 'a' + ord 'z' - ord (toLower c))
      | otherwise = c

wordSize :: Int -> String -> String
wordSize n s = if null gs then g else g ++ " " ++ wordSize n gs
  where (g, gs) = splitAt n s