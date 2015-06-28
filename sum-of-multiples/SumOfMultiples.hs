module SumOfMultiples (sumOfMultiples, sumOfMultiplesDefault) where

sumOfMultiplesDefault :: Integer -> Integer
sumOfMultiplesDefault n = sumOfMultiples [3, 5] n

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples divisors num = sum $ filter (isDivisible divisors) [1..(num-1)]
	where isDivisible d n = any (\x -> n `mod` x == 0) d
