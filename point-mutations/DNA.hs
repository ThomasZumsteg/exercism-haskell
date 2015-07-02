module DNA (hammingDistance) where

hammingDistance :: (Eq a) => [a] -> [a] -> Int
hammingDistance xs ys = sum $ zipWith equal xs ys
  where equal x y = if x == y then 0 else 1