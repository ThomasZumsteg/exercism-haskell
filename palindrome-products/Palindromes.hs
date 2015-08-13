module Palindromes (
  largestPalindrome,
  smallestPalindrome
) where

import Data.List (find)

largestPalindrome :: (Show a, Integral a) => a -> a -> (a, [(a, a)])
largestPalindrome minFact maxFact =
  let
    factors =
      [ [ (x, y) | y <- [maxFact, maxFact - 1 .. x] ] |
      x <- [maxFact, maxFact - 1 .. minFact] ]
  in
    findPal factors

smallestPalindrome :: (Show a, Integral a) => a -> a -> (a, [(a, a)])
smallestPalindrome minFact maxFact =
  let 
    factors = 
      [ [ (x, y) | y <- [minFact .. x] ]
      x <- [minFact .. maxFact] ]
  in
    findPal factors

findFactors :: (Integral a) => a -> a -> a -> [(a, a)]
findFactors num minFact maxFact = 
  let
    stop = min maxFact (floor $ (sqrt :: Double -> Double) $ fromIntegral num)
    smallestFact =  [y | y <- [ minFact .. stop ], 0 == mod num y]
  in [ (x, div num x) | x <- smallestFact, maxFact >= div num x]

findPal :: (Integral a, Show a) => [[(a, a)]] -> a
findFirstPal nums = case find isPal nums of
  Just n -> n
  Nothing -> 0
  where
    isPal num = (show num) == (reverse $ show num)
