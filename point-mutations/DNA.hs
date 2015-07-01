module DNA (hammingDistance) where

hammingDistance :: (Eq a) => [a] -> [a] -> Int
hammingDistance xs ys = sum $ zipWith equal xs ys
  where equal x y | x == y = 0 | otherwise = 1