module Accumulate (accumulate) where

accumulate::(Ord a) => ( a -> b ) -> [a] -> [b]
accumulate _ [] = []
accumulate f (x:xs) = [ f x ] ++ accumulate f xs