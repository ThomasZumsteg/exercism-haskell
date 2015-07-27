module BST (
    bstLeft,
    bstRight,
    bstValue,
    singleton,
    insert,
    fromList,
    toList
  ) where

import Data.Maybe (isNothing, fromJust)
import Data.List (foldl')

data Node a = Node { 
    bstValue :: a,
    bstLeft  :: Maybe (Node a), 
    bstRight :: Maybe (Node a)
  } deriving (Show, Eq)

singleton :: a -> Node a
singleton v = Node v Nothing Nothing

insert :: (Ord a) => a -> Node a -> Node a
insert v n@(Node value left right)
  | v <= value = n { bstLeft = 
    if isNothing left
    then Just (singleton v)
    else fmap (insert v) left }
  | otherwise = n { bstRight = 
    if isNothing right 
    then Just (singleton v)
    else fmap (insert v) right }

fromList :: (Ord a) => [a] -> Node a
fromList (x:xs) = foldl' (flip insert) (singleton x) xs
fromList [] = error "No empty lists"

toList :: Node a -> [a]
toList (Node value left right) = 
  let
    left_list  = if isNothing left  then [] else fromJust $ fmap toList left
    right_list = if isNothing right then [] else fromJust $ fmap toList right
  in
    left_list ++ [value] ++ right_list
