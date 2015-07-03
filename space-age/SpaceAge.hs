module SpaceAge(Planet(..), ageOn) where

data Planet = Earth 
  | Mercury 
  | Venus 
  | Mars 
  | Jupiter 
  | Saturn 
  | Uranus 
  | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet s = s / (earth_seconds * 
  case planet of
    Earth -> 1.0
    Mercury -> 0.2408467
    Venus -> 0.61519726
    Mars -> 1.8808158
    Jupiter -> 11.862615
    Saturn -> 29.447498
    Uranus -> 84.016846
    Neptune -> 164.79132
  )
  where earth_seconds = 31557600
