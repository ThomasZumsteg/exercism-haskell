module Luhn (
  checkDigit,
  addends,
  checksum,
  isValid,
  create
  ) where

checkDigit :: Integer -> Integer
checkDigit = flip mod 10

addends :: Integer -> [Integer]
addends = reverse . zipWith ($) (cycle [id, double]) . map makeNum . reverse . show
  where
    makeNum n = read [n] :: Integer
    double n
      | n <= 4 = 2 * n
      | otherwise = 2 * n - 9

checksum :: Integer -> Integer
checksum = flip mod 10 . sum . addends

isValid :: Integer -> Bool
isValid = (==0) . checksum  

create :: Integer -> Integer
create n =
  let n' = 10 * n
  in n' + ( 10 - checksum n') 