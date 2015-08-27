module Zipper (
    BinTree(..),
    Zipper,

    fromTree,
    toTree,

    value,
    left,
    right,
    up,

    setValue,
    setLeft,
    setRight
) where

import Data.List (foldl')

-- | A binary tree.
data BinTree a = BT { 
    btValue :: a                 -- ^ Value
  , btLeft  :: Maybe (BinTree a) -- ^ Left child
  , btRight :: Maybe (BinTree a) -- ^ Right child
} deriving (Eq, Show)

-- | A zipper for a binary tree.
data Zipper a = Zipper {
    getTrail :: [Branch a],
    getTree :: BinTree a 
} deriving (Show, Eq)
data Branch a = 
    LeftBranch a (Maybe (BinTree a)) | 
    RightBranch a (Maybe (BinTree a))
    deriving (Show, Eq)

-- | Get a zipper focussed on the root node.
fromTree :: BinTree a -> Zipper a
fromTree = Zipper []

-- | Get the complete tree from a zipper.
toTree :: Zipper a -> BinTree a
toTree (Zipper steps smallTree) = foldl' buildTree smallTree steps
    where
        buildTree tree step = case step of
            LeftBranch  v rt -> BT v (Just tree) rt 
            RightBranch v lt -> BT v lt (Just tree)

-- | Get the value of the focus node.
value :: Zipper a -> a
value = btValue . getTree

-- | Get the left child of the focus node, if any.
left :: Zipper a -> Maybe (Zipper a)
left (Zipper _ (BT _ Nothing _)) = Nothing
left (Zipper trail (BT v (Just leftTree) rightTree)) =
    Just $ Zipper ((LeftBranch v rightTree):trail) leftTree

-- | Get the right child of the focus node, if any.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper _ (BT _ _ Nothing)) = Nothing
right (Zipper trail (BT v leftTree (Just rightTree))) =
    Just $ Zipper ((RightBranch v leftTree):trail) rightTree

-- | Get the parent of the focus node, if any.
up :: Zipper a -> Maybe (Zipper a)
up (Zipper [] _) = Nothing
up (Zipper (b:bs) tree) = case b of
    LeftBranch v rightTree -> Just $ Zipper bs $ BT v (Just tree) rightTree
    RightBranch v leftTree -> Just $ Zipper bs $ BT v leftTree (Just tree)

-- | Set the value of the focus node.
setValue :: a -> Zipper a -> Zipper a
setValue v (Zipper trail tree) = Zipper trail $ modify tree
    where
        modify (BT _ lt rt) = BT v lt rt

-- | Replace a left child tree.
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft lt (Zipper trail tree) = Zipper trail $ modify tree
    where
        modify (BT v _ rt) = BT v lt rt

-- | Replace a right child tree.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight rt (Zipper trail tree) = Zipper trail $ modify tree
    where
        modify (BT v lt _) = BT v lt rt
