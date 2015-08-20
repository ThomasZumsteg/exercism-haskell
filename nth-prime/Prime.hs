module Prime (nth) where

nth :: Int -> Int
nth = (primes !!) . pred

primes :: [Int]
primes = 2 : filter isPrime [3, 5..]
  where
    isPrime p = all ((/=) 0 . mod p) $ takeWhile (\n -> n*n <= p) primes
    