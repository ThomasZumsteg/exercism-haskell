module WordProblem (answer) where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)


answer :: String -> Maybe Integer
answer question = case parse makeTokens "" question of
  Right (start : ops) = Just $ foldl (\acc (op, num) -> op acc num) start ops
  Left _ -> Nothing

lexer :: GenTokenParser String u Data.Functor.Identity.Identity
lexer = P.makeTokenParser haskellDef

makeTokens :: 
makeTokens =
  do
    string "What is "
    start <- P.integer lexer
    char ' '
    opers <- many1 operations
    char '?'
    return (read start, opers)

operations ::
operations =
  do
    op <- operator
    char ' '
    num <- P.integer lexer
    return (op, num)

operator :: (Integral a, Stream s m Char) => ParsecT s u m (a -> a -> a)
operator =
      try (string "plus" >> return (+))
  <|> try (string "minus" >> return (-))
  <|> try (string "multiplied by" >> return (*))
  <|> try (string "divided by" >> return div)
