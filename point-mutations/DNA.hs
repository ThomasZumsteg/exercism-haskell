module DNA (hammingDistance) where

hammingDistance :: [Char] -> [Char] -> Int
hammingDistance xs ys = foldl (\acc n -> acc + equal n) 0 (zip xs ys)
  where equal (x, y) | x == y = 0 | otherwise = 1