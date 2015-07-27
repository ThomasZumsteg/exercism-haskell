module Squares (
  sumOfSquares, 
  squareOfSums,
  difference
) where

sumOfSquares :: Integral a => a -> a 
sumOfSquares n = n * (n + 1) * (2 * n + 1) `div` 6

squareOfSums :: Integral a => a -> a 
squareOfSums n = (sq n) * sq (n + 1) `div` 4 
  where sq m = m ^ (2::Int)

difference :: Integral a => a -> a 
difference n = (squareOfSums n) - (sumOfSquares n) 