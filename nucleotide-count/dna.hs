module DNA (count, nucleotideCounts) where
import qualified Data.Map as Map

count :: Char -> String -> Integer
count char dna 
  | not $ validChar char = 0
  | not $ all validChar dna = 0
  | otherwise = foldl (\acc x -> if x == char then 1 + acc else acc) 0 dna

validChar :: Char -> Bool
validChar c = (c `elem` "GTAC") || error("invalid nucleotide '" ++ [c] ++ "'")

nucleotideCounts :: String -> Map.Map Char Integer
nucleotideCounts dna 
  | not $ all validChar dna = Map.empty
  | otherwise = Map.fromList [(c, count c dna) | c <- "GTAC"]
