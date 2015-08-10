module CustomSet (
  CustomSet,
  insert,
  member,
  empty,
  size,
  isSubsetOf,
  fromList,
  toList,
  undefined,
  isDisjointFrom,
  difference,
  delete,
  intersection,
  null,
  union
) where

import qualified Data.List as List
import Data.List (foldl')
import Prelude hiding (null)

data CustomSet a = CustomSet { toList :: [a] } deriving (Eq)

instance (Show a, Ord a) => Show (CustomSet a) where
  show set = "fromList " ++ show (toList set)

insert :: (Eq a, Ord a) => a -> CustomSet a -> CustomSet a
insert item set
  | member item set = set
  | otherwise = CustomSet $ List.insert item $ toList set

delete :: (Eq a) => a -> CustomSet a -> CustomSet a
delete item set = CustomSet $ List.delete item $ toList set

member :: (Eq a) => a -> CustomSet a -> Bool
member i = elem i . toList 

empty :: CustomSet a
empty = CustomSet []

null :: (Eq a) => CustomSet a -> Bool
null set = set == empty

size :: CustomSet a -> Int
size = length . toList 

fromList :: (Eq a, Ord a) => [a] -> CustomSet a
fromList = foldl' (\acc i -> insert i acc) empty

isDisjointFrom :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> Bool
isDisjointFrom set_a set_b = empty == intersection set_a set_b 

difference :: (Eq a) => CustomSet a -> CustomSet a -> CustomSet a
difference set remove = foldl' (\acc i -> delete i acc) set (toList remove)

isSubsetOf :: (Ord a, Eq a) => CustomSet a -> CustomSet a -> Bool
isSubsetOf sub super = super == union sub super

union :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> CustomSet a
union set = foldl' (\acc a -> insert a acc) set . toList 

intersection :: (Eq a, Ord a) => CustomSet a -> CustomSet a -> CustomSet a
intersection set = foldl' add empty . toList
  where
    add s i = if member i set then insert i s else s
