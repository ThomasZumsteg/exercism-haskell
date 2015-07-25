module Robot (
    Bearing(..),
    Robot,
    mkRobot,
    coordinates,
    simulate,
    bearing,
    turnRight,
    turnLeft
  ) where

import Data.List (foldl')

data Bearing = North | East | South | West 
  deriving (Show, Eq, Enum, Bounded)

data Robot = Robot { 
    bearing :: !Bearing,
    coord_x :: !Int,
    coord_y :: !Int 
  } deriving (Show, Eq)

turn :: (Bounded a, Enum a) => Int -> a -> a
turn s t =
  let max_t = fromEnum ( maxBound `asTypeOf` t ) + 1
  in toEnum (mod (s + fromEnum t) max_t)

coordinates :: Robot -> (Int, Int)
coordinates (Robot _ x y) = (x, y)

turnLeft :: Bearing -> Bearing
turnLeft = turn (-1)

turnRight :: Bearing -> Bearing
turnRight = turn (1)

mkRobot :: Bearing -> (Int, Int) -> Robot
mkRobot b (x, y) = Robot b x y

simulate :: Robot -> String -> Robot
simulate = foldl' commands
  where
    commands r@(Robot b x y) i = case i of
      'R' -> Robot (turnRight b) x y
      'L' -> Robot (turnLeft b) x y
      'A' -> advance r
      i' -> error $ show i' ++ " is not a valid command" 

advance :: Robot -> Robot
advance (Robot b x y) = case b of
  East -> Robot b (x + 1) y
  West -> Robot b (x - 1) y
  North -> Robot b x (y + 1)
  South -> Robot b x (y - 1)
