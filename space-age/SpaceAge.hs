module SpaceAge(Planet(..), ageOn) where

data Planet = Earth 
  | Mercury 
  | Venus 
  | Mars 
  | Jupiter 
  | Saturn 
  | Uranus 
  | Neptune

earth_seconds :: Float
earth_seconds = 31557600

ageOn :: Planet -> Float -> Float
ageOn Earth s = s / earth_seconds
ageOn Mercury s = s / (0.2408467 * earth_seconds)
ageOn Venus s = s / (0.61519726 * earth_seconds)
ageOn Mars s = s / (1.8808158 * earth_seconds)
ageOn Jupiter s = s / (11.862615 * earth_seconds)
ageOn Saturn s = s / (29.447498 * earth_seconds)
ageOn Uranus s = s / (84.016846 * earth_seconds)
ageOn Neptune s = s / (164.79132 * earth_seconds)
