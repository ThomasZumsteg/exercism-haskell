module Bob (responseFor) where

import Data.Char (isLower, isSpace, isUpper)

responseFor :: String -> String
responseFor what
  | nothing what  = "Fine. Be that way!" 
  | shouting what = "Whoa, chill out!"
  | question what = "Sure."
  | otherwise = "Whatever."

  where
    nothing w = all isSpace w
    question w
      | isSpace $ last w = question $ init w
      | otherwise = last w == '?'
    shouting w = hasUppercase w && not(hasLowercase w)
      where
        hasUppercase = not . null . filter isUpper
        hasLowercase = any isLower