module Palindromes (
  largestPalindrome,
  smallestPalindrome
) where

largestPalindrome :: (Show a, Integral a) => a -> a -> (a, [(a, a)])
largestPalindrome minFact maxFact =
  let
    factors = [ x * y |
        x <- [minFact .. maxFact],
        y <- [x .. maxFact] ]
    biggest = maximum $ filter isPal factors
  in
    (biggest, findFactors biggest minFact maxFact)

smallestPalindrome :: (Show a, Integral a) => a -> a -> (a, [(a, a)])
smallestPalindrome minFact maxFact =
  let 
    factors = [ x * y | 
      x <- [minFact .. maxFact],
      y <- [x .. maxFact] ]
    smallest = minimum $ filter isPal factors
  in
    (smallest, findFactors smallest minFact maxFact)

isPal :: (Show a) => a -> Bool
isPal n = (show n) == (reverse $ show n)

findFactors :: (Integral a) => a -> a -> a -> [(a, a)]
findFactors num minFact maxFact = 
  let
    stop = min maxFact (floor $ (sqrt :: Double -> Double) $ fromIntegral num)
    smallestFact =  [y | y <- [ minFact .. stop ], 0 == mod num y]
  in [ (x, div num x) | x <- smallestFact, maxFact >= div num x]
