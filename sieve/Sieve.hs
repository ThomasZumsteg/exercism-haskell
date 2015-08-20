module Sieve (primesUpTo) where

primesUpTo :: Integer -> [Integer]
primesUpTo n = takeWhile (<=n) primes

primes :: [Integer]
primes = 2 : filter isPrime [3, 5..]

isPrime :: Integer -> Bool
isPrime n = not $ any (\x -> mod n x == 0) divisors
  where
    s = floor $ (sqrt :: Double -> Double) $ fromIntegral n
    divisors = takeWhile (<= s) primes