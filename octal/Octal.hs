module Octal (showOct, readOct) where

showOct :: Integral a => a -> String
showOct decNum = build (divMod decNum 8) ""
  where
    build (q, r) acc = case q of
      0 -> acc'
      _ -> build (divMod q 8) acc'
      where 
        acc' = c : acc
        c = toEnum $ fromEnum '0' + fromIntegral r

readOct :: Integral a => String -> a
readOct s = build s 0
  where
    build "" acc = acc
    build (c:cs) acc = build cs acc'
      where
        acc' = acc * 8 + fromIntegral (fromEnum c - fromEnum '0') 