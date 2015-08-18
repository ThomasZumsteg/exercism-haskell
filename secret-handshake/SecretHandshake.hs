{-# LANGUAGE FlexibleInstances #-}
module SecretHandshake (handshake) where

import Numeric (showIntAtBase)
import Data.Char (intToDigit)

class Code a where toRBin :: a -> String

instance Code Int where toRBin n = reverse $ showIntAtBase 2 intToDigit n ""
instance Code String where toRBin = reverse

handshake :: (Code a) => a -> [String]
handshake c = foldl
  (\acc (bit, action) -> if bit == '1' then action acc else acc) []
  $ zip (toRBin c) steps
  where
    steps = [
      flip (++) ["wink"],
      flip (++) ["double blink"],
      flip (++) ["close your eyes"],
      flip (++) ["jump"],
      reverse ]