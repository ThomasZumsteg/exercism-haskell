module Triangle (row, triangle) where

triangle :: Integral a => [[a]]
triangle = [row x | x <- [1..]]

row :: Integral a => a -> [a]
row n = map (binomial (n-1)) [0..(n-1)]

binomial :: Integral a => a -> a -> a
binomial _ 0 = 1
binomial 0 _ = 0
binomial n k = binomial (n-1) (k-1) * n `div` k 