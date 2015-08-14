module Prime (nth, minus) where

nth :: Int -> Int
nth = (primes !!) . pred

primes :: [Int]
primes = 2 : sieve [3,5..]
  where
    sieve (p:xs) = p : sieve (minus xs [p*p, p*(p+1)..])

minus :: Ord a => [a] -> [a] -> [a]
minus [] _ = []
minus xs [] = xs
minus l1@(x:xs) l2@(y:ys)
  | x > y = minus l1 ys
  | x < y = x : minus xs l2
  | otherwise = minus xs l2