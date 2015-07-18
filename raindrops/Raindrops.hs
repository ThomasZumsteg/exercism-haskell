module Raindrops (convert) where

convert :: Int -> String
convert r = case rain_factors r of
  [] -> show r
  f -> concat $ map noises f
  where
    rain_factors n = filter (\x -> 0 == mod n x) [3, 5, 7]
    noises n = case n of
      3 -> "Pling"
      5 -> "Plang"
      7 -> "Plong"
      _ -> "Error"