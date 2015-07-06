module School (School, empty, grade, add, sorted) where
import qualified Data.Map as Map
import qualified Data.Set as Set

newtype School = School { newSchool :: Map.Map Int (Set.Set String) }

empty :: School
empty = School Map.empty

sorted :: School -> [(Int, [String])]
sorted = map (\(g, s) -> (g, Set.toAscList s)) . Map.toAscList . newSchool

grade :: Int -> School -> [String]
grade g = Set.toAscList . Map.findWithDefault Set.empty g . newSchool

add :: Int -> String -> School -> School
add g s =
  School . Map.insertWith Set.union g (Set.singleton s) . newSchool