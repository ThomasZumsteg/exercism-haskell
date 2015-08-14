module Prime (nth) where

nth :: Int -> Int
nth = (primes !!) . pred

primes :: [Int]
primes = 2 : filter isPrime [3,5..]

isPrime :: Int -> Bool
isPrime n = not $ any (\x -> mod n x == 0) divisors
  where
    s = floor $ (sqrt :: Double -> Double) $ fromIntegral n
    divisors = takeWhile (<= s) primes