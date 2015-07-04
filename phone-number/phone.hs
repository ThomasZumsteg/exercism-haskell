module Phone (areaCode, number, prettyPrint) where

import Data.Char (isDigit)

import Text.Printf (printf)

number :: String -> String
number n
  | length digits == 10 = digits
  | length digits == 11 && head digits == '1' = tail digits
  | otherwise = "0000000000"
  where digits = filter isDigit n

areaCode :: String -> String
areaCode = take 3 . number

prettyPrint :: String -> String
prettyPrint num = printf "(%s) %s-%s" area prefix postfix
  where
    digits = number num
    slice m n = take n . drop m
    area = areaCode digits
    prefix = slice 3 3 digits
    postfix = slice 6 4 digits

