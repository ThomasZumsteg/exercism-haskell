module Hexadecimal (hexToInt) where

import Data.List (foldl')

hexToInt :: String -> Int
hexToInt h = case foldl' hexAdder (Just 0) h of
    Nothing -> 0
    Just n -> n
    where hexAdder acc c = (+) <$> (fmap (*16) acc) <*> toHex c

toHex :: Char -> Maybe Int
toHex c = case c of
    '0' -> Just  0
    '1' -> Just  1
    '2' -> Just  2
    '3' -> Just  3
    '4' -> Just  4
    '5' -> Just  5 
    '6' -> Just  6
    '7' -> Just  7
    '8' -> Just  8
    '9' -> Just  9
    'a' -> Just 10
    'b' -> Just 11
    'c' -> Just 12
    'd' -> Just 13
    'e' -> Just 14
    'f' -> Just 15
    _   -> Nothing
