module Sgf (parseSgf) where

import qualified Data.Map as Map
import Control.Applicative hiding ((<|>), many)
import Data.Text (Text, pack)
import Data.Tree
import Text.Parsec
import Data.Char (isSpace)

type SgfNode = Map.Map Text [Text]

parseSgf :: Text -> Maybe (Tree (SgfNode))
parseSgf = toMaybe . parse tree ""
  where
    toMaybe = either (const Nothing) Just

tree :: Parsec Text () (Tree SgfNode)
tree = do
  n <- char '(' *> many1 node 
  trees <- many tree <* char ')'
  return $ makeSfgTree n trees

node :: Parsec Text () SgfNode
node = char ';' *> (Map.fromList <$> many properties)

properties :: Parsec Text () (Text, [Text])
properties = do
  key <- many1 upper
  vals <- many1 val
  return (pack key, vals)

val :: Parsec Text () Text
val = char '[' *> worker [] False
  where
    worker acc bs = do
        c <- anyChar
        case c of
            ']'  | not bs    -> return . pack . reverse $ acc
            '\\' | not bs    -> worker acc True
            '\n' | bs        -> worker acc False
            _    | isSpace c -> worker (' ' : acc) False
            _                -> worker (c : acc) False

makeSfgTree :: [SgfNode] -> [(Tree SgfNode)] -> Tree SgfNode
makeSfgTree [] _ = error "Needs a root node"
makeSfgTree [n] trees = Node n trees
makeSfgTree (n:ns) trees = Node n [makeSfgTree ns trees] 