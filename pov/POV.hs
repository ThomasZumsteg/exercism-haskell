module POV (Graph(..), fromPOV, tracePathBetween) where

data Graph a = Graph { _value :: a, _leaves :: [Graph a] } deriving (Show, Eq)
data Crumb a = Crumb a [Graph a] [Graph a] deriving (Show, Eq)
data Zipper a = Zipper { _node :: (Graph a), _trail :: [Crumb a]} deriving (Show, Eq)

fromPOV :: (Eq a) => a -> Graph a -> Maybe (Graph a)
fromPOV val graph = rebuildTree <$> (findValue val $ emptyZipper graph)

tracePathBetween :: (Eq a) => a -> a -> Graph a -> Maybe [a]
tracePathBetween from to graph = buildPath <$> (toZipper graph >>= (findValue from))
  where
    toZipper g = emptyZipper <$> fromPOV to g

buildPath :: Zipper a -> [a]
buildPath (Zipper (Graph v _) crumbs) = v:steps
  where
    steps = map (\(Crumb step _ _) -> step) crumbs

emptyZipper :: Graph a -> Zipper a
emptyZipper g = Zipper g []

findValue :: (Eq a) => a -> Zipper a -> Maybe (Zipper a)
findValue val zipp@(Zipper (Graph v leaves) crumbs)
  | val == v = Just zipp
  | not $ (null leaves || null down) = down 
  | not (null crumbs || null lefts) = findValue val $ goLeft zipp
  | otherwise = Nothing
  where
    down = findValue val $ goDown zipp
    (Crumb _ lefts _) = head crumbs

rebuildTree :: Zipper a -> Graph a
rebuildTree (Zipper graph@(Graph val leaves) crumbs)
  | null crumbs = graph
  | otherwise = Graph val (leaves ++ [newLeaf])
  where
    (c:cs) = crumbs
    newLeaf = rebuildTree $ Zipper (crumbToGraph c) cs

crumbToGraph :: Crumb a -> Graph a
crumbToGraph (Crumb val lefts rights) = Graph val ((reverse rights) ++ lefts)

goLeft :: Zipper a -> Zipper a
goLeft (Zipper node ((Crumb parent (left:lefts) rights):crumbs)) =
  let crumb = Crumb parent lefts (node:rights)
  in Zipper left (crumb:crumbs)
goLeft _ = error "Can't go left"

goDown :: Zipper a -> Zipper a
goDown (Zipper (Graph node (leaf:leaves)) crumbs) = 
  let crumb = Crumb node leaves []
  in Zipper leaf (crumb:crumbs)
goDown _ = error "Can't go down"

-- Included for completness

goUp :: Zipper a -> Zipper a
goUp (Zipper node ((Crumb parent lefts rights):crumbs)) =
  let parentNode = Graph parent (lefts ++ [node] ++ (reverse rights))
  in Zipper parentNode crumbs
goUp _ = error "Can't go up"

goRight :: Zipper a -> Zipper a
goRight (Zipper node ((Crumb parent lefts (right:rights)):crumbs)) =
  let crumb = Crumb parent (node:lefts) rights
  in Zipper right (crumb:crumbs)
goRight _ = error "Can't go right"
