module Strain (keep, discard) where

keep :: (a -> Bool) -> [a] -> [a]
keep test xs = [x | x <- xs, test x]

discard :: (a -> Bool) -> [a] -> [a]
discard test xs = keep (not . test) xs