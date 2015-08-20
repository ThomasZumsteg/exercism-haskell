module Sieve (primesUpTo) where

primesUpTo :: Integer -> [Integer]
primesUpTo n = takeWhile (<=n) primes

primes :: [Integer]
primes = 2 : filter isPrime [3, 5..]
  where
    isPrime p = all ((/=) 0 . mod p) $ takeWhile (\n -> n * n <= p) primes
