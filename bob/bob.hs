module Bob (responseFor) where

import Data.Char (isLower, isSpace, isUpper)

responseFor :: String -> String
responseFor what
  | nothing what  = "Fine. Be that way!" 
  | shouting what = "Whoa, chill out!"
  | question what = "Sure."
  | otherwise = "Whatever."

  where
    shouting w = filter isUpper w /= [] && not(any isLower w)
    nothing w = all isSpace w
    question w
      | isSpace $ last w = question $ init w
      | otherwise = last w == '?'