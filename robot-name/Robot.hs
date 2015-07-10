module Robot (robotName, mkRobot, resetName) where

import Control.Concurrent 
import System.Random (randomRIO)

data Robot = Robot { robotNameVar :: MVar String }

mkRobot :: IO Robot
mkRobot = do
  name <- generateName
  name' <- newMVar name
  let r = Robot( name' )
  return r

resetName :: Robot -> IO ()
resetName r = do
  newName <- generateName
  _ <- swapMVar (robotNameVar r) newName
  return ()

robotName :: Robot -> IO String
robotName = readMVar . robotNameVar

generateName :: IO String
generateName = sequence [letter, letter, number, number, number]
  where
    number = randomRIO('0', '9')
    letter = randomRIO('A', 'Z')