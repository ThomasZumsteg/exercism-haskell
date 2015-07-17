module PrimeFactors (primeFactors) where

primeFactors :: Int -> [Int]
primeFactors n = divs n [2..max_n]
  where
    max_n = floor $ sqrt $ (fromIntegral n :: Double)
    
divs :: Int -> [Int] -> [Int]
divs 1 _ = []
divs n [] = [n]
divs n facts@(f:fs)
  | 0 == mod n f = f : divs (quot n f) facts
  | otherwise = divs n fs