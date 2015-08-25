module Sgf (parseSgf) where

import qualified Data.Map as Map
import Data.Text (Text, pack)
import Data.Tree
import Text.Parsec

type SgfNode = Map.Map Text [Text]

parseSgf :: Text -> Maybe (Tree (SgfNode))
parseSgf = toMaybe . parse parser ""
  where
    toMaybe = either (const Nothing) (\n -> Just $ Node n [])

parser :: Parsec Text () SgfNode
parser = between (string "(;") (char ')') $ option Map.empty keyValue

keyValue :: Parsec Text () SgfNode
keyValue = do
  key <- char ';' *> many upper
  value <- between (char '[') (char ']') $ many letter
  return $ Map.fromList [(pack key, [pack value])]