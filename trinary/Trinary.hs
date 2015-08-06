module Trinary (showTri, readTri) where

import Data.List (elemIndex)

showTri :: Integral a => a -> String
showTri = showBase "012"

showBase :: Integral a => String -> a -> String
showBase chars n = build (divMod n base) ""
  where
    base = fromIntegral $ length chars
    build (q, r) acc = case q of
      0 -> acc'
      _ -> build (divMod q base) acc'
      where
        acc' = (chars !! (fromIntegral r)) : acc

readTri :: Integral a => String -> a
readTri s = case readBase "012" (Just 0) s of
  Just n -> fromIntegral n
  Nothing -> 0

readBase :: String -> Maybe Int -> String -> Maybe Int
readBase _ acc [] = acc
readBase chars acc (c:cs) = readBase chars acc' cs
  where
    acc' = (+) <$> (((*) $ length chars) <$> acc) <*> (elemIndex c chars)
