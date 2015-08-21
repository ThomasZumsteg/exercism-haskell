module Cipher (caesarEncode, caesarDecode, caesarEncodeRandom) where

import System.Random (randomRIO)

caesarEncode :: String -> String -> String
caesarEncode keyString = map shift . zip (cycle key)
  where
    key = map (flip (-) a . fromEnum) keyString

a :: Int
a = fromEnum 'a'

shift :: (Int, Char) -> Char
shift (k, v) = toEnum $ a + mod (fromEnum v + k - a) 26

caesarDecode :: String -> String -> String
caesarDecode keyString = map shift . zip (cycle key)
  where
    key = map ((-) a . fromEnum) keyString

caesarEncodeRandom :: String -> IO (String, String)
caesarEncodeRandom plainText = do
  key <- sequence [randomRIO('a', 'z') | _ <- plainText]
  let cipherText = caesarEncode key plainText
  return (key, cipherText)
