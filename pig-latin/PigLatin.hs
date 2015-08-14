module PigLatin (translate) where

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord w = 
  let (start, end) = splitter w
  in concat [end, start, "ay"]

splitter :: String -> (String, String)
splitter [] = ("", "")
splitter ('q':'u':cs) = 
  let (start, end) = splitter cs
  in ("qu" ++ start, end)
splitter w@(c:cs)
  | elem c "aeiouy" = ([], w)
  | otherwise = 
  let (start, end) = splitter cs
  in (c:start, end)
