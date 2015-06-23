module DNA (toRNA) where

toRNA::String -> String
toRNA [] = []
toRNA "G" = "C"
toRNA "C" = "G"
toRNA "T" = "A"
toRNA "A" = "U"
toRNA (c:cs) = toRNA [c] ++ toRNA cs