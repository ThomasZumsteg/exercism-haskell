module PigLatin (translate) where

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord s =
  let (start, end@(e:es)) = span (not . vowel) s
  in if e == 'u' && last start == 'q'
    then es ++ start ++ [e] ++ "ay"
    else end ++ start ++ "ay"
  where
    vowel = flip elem "aeiou"
