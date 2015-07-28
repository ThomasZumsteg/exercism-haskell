module BST (
    bstLeft,
    bstRight,
    bstValue,
    singleton,
    insert,
    fromList,
    toList
  ) where

import Data.List (foldl')

data Node a = Node { 
    bstLeft  :: Maybe (Node a), 
    bstRight :: Maybe (Node a),
    bstValue :: a
  } deriving (Show, Eq)

singleton :: a -> Node a
singleton = Node Nothing Nothing

insert :: (Ord a) => a -> Node a -> Node a
insert v n@(Node left right value) =
  if v <= value
    then n { bstLeft  = add_node left }
    else n { bstRight = add_node right }
  where
    add_node = Just . maybe (singleton v) (insert v)

fromList :: (Ord a) => [a] -> Node a
fromList (x:xs) = foldl' (flip insert) (singleton x) xs
fromList [] = error "No empty lists"

toList :: Node a -> [a]
toList (Node left right value) = 
  let
    left_list  = maybe [] toList left
    right_list = maybe [] toList right
  in left_list ++ [value] ++ right_list
