module DNA (toRNA) where

toRNA::String -> String
toRNA "G" = "C"
toRNA "C" = "G"
toRNA "U" = "A"
toRNA "A" = "T"