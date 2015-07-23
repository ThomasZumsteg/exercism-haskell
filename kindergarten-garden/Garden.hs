module Garden (
  garden,
  defaultGarden,
  lookupPlants,
  Plant(..)
) where

import qualified Data.Map.Strict as Map
import Data.List.Split (chunksOf, splitOn)
import Data.List (transpose, sort)

data Plant = 
  Radishes 
  | Clover
  | Grass
  | Violets
  deriving (Show, Eq)

type Student = String
type Garden = Map.Map Student [Plant]

garden :: [Student] -> String -> Garden
garden s g = Map.fromList $ zip s' g'
  where
    s' = sort s
    g' = map (map plantMap) $ map concat $ transpose $ 
         map (chunksOf 2) $ splitOn "\n" g
      where 
        plantMap p = case p of
          'R' -> Radishes
          'C' -> Clover
          'G' -> Grass
          'V' -> Violets
          _ -> error $ show p ++ " is not a valid plant"

defaultGarden :: String -> Garden
defaultGarden = garden defaultStudents
  where
    defaultStudents = [ 
        "Alice",
        "Bob",
        "Charlie",
        "David",
        "Eve",
        "Fred",
        "Ginny",
        "Harriet",
        "Ileana",
        "Joseph",
        "Kincaid", 
        "Larry"
      ]

lookupPlants :: Student -> Garden -> [Plant]
lookupPlants s g = case Map.lookup s g of
  Just plants -> plants
  Nothing -> error $ show s ++ " is not in this garden"
