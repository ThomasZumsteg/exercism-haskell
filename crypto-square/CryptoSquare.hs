module CryptoSquare ( 
  normalizePlaintext,
  squareSize,
  plaintextSegments,
  ciphertext,
  normalizeCiphertext) where

import Data.Char (isAlphaNum, toLower)
import Data.List.Split (chunksOf)
import Data.List (transpose)

normalizePlaintext :: String -> String
normalizePlaintext = map toLower . filter isAlphaNum

squareSize :: String -> Int
squareSize text = ceiling $ sqrt $ (fromIntegral chars :: Double)
  where chars = length $ normalizePlaintext text

plaintextSegments :: String -> [String]
plaintextSegments text = chunksOf (squareSize text) (normalizePlaintext text)

ciphertext :: String -> String
ciphertext = concat . transpose . plaintextSegments

normalizeCiphertext :: String -> String
normalizeCiphertext = unwords . transpose . plaintextSegments