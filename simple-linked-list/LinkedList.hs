module LinkedList where

data Link a = Nil | Link { 
  datum :: a,
  next :: Link a
} deriving (Show)

nil :: Link a
nil = Nil

new :: a -> Link a -> Link a
new d n = Link d n

isNil :: Link a -> Bool
isNil Nil = True
isNil _ = False

toList :: Link a -> [a]
toList Nil = []
toList (Link d n) = d : toList n

fromList :: [a] -> Link a
fromList = foldr new Nil

reverseLinkedList :: Link a -> Link a
reverseLinkedList = fromList . reverse . toList 
