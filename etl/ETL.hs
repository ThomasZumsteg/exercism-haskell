module ETL (transform) where

import qualified Data.Map.Strict as Map
import Data.List (foldl')
import Data.Char (toLower)

type Old = Map.Map Int [String] 
type New = Map.Map String Int

transform :: Old -> New
transform = Map.foldrWithKey' (\k -> flip (addValKeysToMap k)) Map.empty

addValKeysToMap :: Int -> New -> [String] -> New
addValKeysToMap v = foldl' (\m k -> Map.insert (lower k) v m)
  where lower = map toLower
