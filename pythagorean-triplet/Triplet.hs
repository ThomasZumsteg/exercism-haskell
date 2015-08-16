module Triplet (mkTriplet, isPythagorean, pythagoreanTriplets) where

import Data.List (sort)

data PythagoreanTriplet = PythagoreanTriplet Int Int Int deriving (Show, Eq)

mkTriplet :: Int -> Int -> Int -> PythagoreanTriplet
mkTriplet a b c =
  let a' : b' : c' : _ = sort [a,b,c]
  in PythagoreanTriplet a' b' c'

isPythagorean :: PythagoreanTriplet -> Bool
isPythagorean (PythagoreanTriplet a b c) = sqr a + sqr b == sqr c
  where
    sqr = (^(2 :: Integer))

pythagoreanTriplets :: Int -> Int -> [PythagoreanTriplet]
pythagoreanTriplets minNum maxNum = 
  let
    triplets = [mkTriplet a b c | 
      a <- [minNum .. maxNum],
      b <- [a .. maxNum],
      c <- [b .. maxNum] ]
  in
    filter isPythagorean triplets