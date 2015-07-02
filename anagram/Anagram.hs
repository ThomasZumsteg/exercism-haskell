module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor word word_list = filter (anagram word) word_list

anagram :: String -> String -> Bool
anagram word1 word2
  | lowerCase word1 == lowerCase word2 = False
  | otherwise = letters word1 == letters word2
  where
    lowerCase = map toLower
    letters = sort . lowerCase
