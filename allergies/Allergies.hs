module Allergies (Allergen(..), isAllergicTo, allergies) where

import Data.Bits

data Allergen = Eggs 
  | Peanuts 
  | Shellfish 
  | Strawberries 
  | Tomatoes 
  | Chocolate 
  | Pollen 
  | Cats
  deriving (Show, Enum, Eq, Bounded)

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo a n = 0 /= (n .&. mask)
  where mask = shift 1 $ fromEnum a

allergies :: Int -> [Allergen]
allergies n = filter (flip isAllergicTo n) [(minBound::Allergen)..]