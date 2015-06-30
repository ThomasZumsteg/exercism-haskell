module Bob (responseFor) where

import Data.Char (isLower, isSpace, isUpper)

responseFor :: String -> String
responseFor what
  | all isSpace what = "Fine. Be that way!"
  | any isUpper what && not(any isLower what) = "Whoa, chill out!"
  | last what == '?' = "Sure."
  | otherwise = "Whatever."
