module Triangle (TriangleType(..), triangleType) where

import Data.List (sort)

data TriangleType = Equilateral | Isosceles | Scalene | Illogical deriving (Eq, Show)

triangleType :: Int -> Int -> Int -> TriangleType
triangleType a b c  
  | a' + b' <= c' || a' <= 0  = Illogical
  | a' == c'                  = Equilateral
  | a' == b' || b' == c'      = Isosceles
  | otherwise                 = Scalene
  where
    (a':b':c':_) = sort [a,b,c]