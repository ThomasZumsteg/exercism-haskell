module School (School, sorted, add, empty, grade) where

import qualified Data.Map as Map
import Data.List (sort)

data School = School { getSchool :: Map.Map Int [String] }

empty :: School
empty = School Map.empty

sorted :: School ->  [(Int, [String])]
sorted = Map.toAscList . Map.map sort . getSchool

add :: Int -> String -> School -> School
add g s = School . Map.insertWith (++) g [s] . getSchool

grade :: Int -> School -> [String]
grade g = Map.findWithDefault [] g . getSchool
