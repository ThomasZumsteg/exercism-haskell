module POV (Graph(..), fromPOV, tracePathBetween) where

data Graph a = Graph { _value :: a, _leaves :: [Graph a] } deriving (Show, Eq)
data Crumb a = Crumb a [Graph a] [Graph a] deriving (Show, Eq)
data Zipper a = Zipper { _node :: (Graph a), _trail :: [Crumb a]}

fromPOV :: (Eq a) => a -> Graph a -> Maybe (Graph a)
fromPOV goal graph@(Graph value leaves@(l:ls)) 
  | goal == value = Just graph
  | null leaves = Nothing
  | isJust $ fromPOV l = Just l { _leaves = (Graph v ls):(_leaves leaf) }
  | otherwise = fromPOV goal $ Graph v ls

tracePathBetween :: (Eq a) => a -> a -> Graph a -> Maybe [a]
tracePathBetween x y g = Nothing 

emptyZipper :: Graph a -> Zipper a
emptyZipper g = Zipper g []