module School (School, sorted, add, empty, grade) where

import qualified Data.Map as Map
import Data.List (sort)

type School = Map.Map Int [String]

empty :: School
empty = Map.empty

sorted :: School ->  [(Int, [String])]
sorted = sort . Map.toList . Map.map sort

add :: Int -> String -> School -> School
add g s = Map.insertWith (++) g [s]

grade :: Int -> School -> [String]
grade g school = case Map.lookup g school of
  Just n -> n
  Nothing -> []
