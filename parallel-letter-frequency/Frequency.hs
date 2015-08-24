module Frequency (frequency) where

import qualified Data.Map.Strict as M
import Data.Text (Text, foldl', toLower, filter)
import Data.Char (isLetter)
import Prelude hiding (filter)
import Control.Parallel.Strategies

frequency :: Int -> [Text] -> M.Map Char Int
frequency workers = M.unionsWith (+) . map countLetters . flip using strategy
  where strategy = parBuffer workers rseq

countLetters :: Text -> M.Map Char Int
countLetters = foldl' increment M.empty . toLower . filter isLetter
  where increment m e = M.insertWith (+) e 1 m
