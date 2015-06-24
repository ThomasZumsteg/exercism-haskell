module Sublist (Sublist(..), sublist) where
  data Sublist = Equal | Sublist | Superlist | Unequal deriving (Show, Eq)

  sublist::(Eq a) => [a] -> [a] -> Sublist
  sublist xs ys
    | xs == ys = Equal
    | xs `inside` ys = Sublist
    | ys `inside` xs = Superlist
    | otherwise = Unequal

  inside::(Eq a) => [a] -> [a] -> Bool
  xs `inside` ys
    | len_xs > len_ys = False
    | xs == take len_xs ys = True
    | otherwise = xs `inside` tail ys
    where len_xs = length xs
          len_ys = length ys