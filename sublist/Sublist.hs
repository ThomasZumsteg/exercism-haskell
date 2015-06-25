module Sublist (Sublist(..), sublist) where
import Data.List (isInfixOf)

data Sublist = Equal | Sublist | Superlist | Unequal deriving (Show, Eq)

sublist :: (Eq a) => [a] -> [a] -> Sublist
sublist xs ys
  | xs == ys = Equal
  | xs `isInfixOf` ys = Sublist
  | ys `isInfixOf` xs = Superlist
  | otherwise = Unequal