{-# LANGUAGE FlexibleContexts #-}
module WordProblem (answer) where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Data.Functor.Identity (Identity)

type Operator = (Integer -> Integer -> Integer) 

answer :: String -> Maybe Integer
answer question = case parse makeTokens "" question of
  (Right (start, ops)) -> Just $ foldl (\acc (op, num) -> op acc num) start ops
  Left _ -> Nothing

lexer :: P.GenTokenParser String u Data.Functor.Identity.Identity
lexer = P.makeTokenParser haskellDef

makeTokens :: ParsecT
  String
  u
  Data.Functor.Identity.Identity
  (Integer, [(Operator, Integer)])
makeTokens = 
  do
    _ <- string "What is "
    start <- P.integer lexer
    opers <- many operations
    _ <- char '?'
    return (start, opers)

operations :: ParsecT
  String
  u
  Data.Functor.Identity.Identity
  (Operator, Integer)
operations = 
  do
    op <- operator
    _ <- char ' '
    num <- P.integer lexer
    return (op, num)

operator :: (Integral a, Stream s m Char) => ParsecT s u m Operator
operator =
      try (string "plus" >> return (+))
  <|> try (string "minus" >> return (-))
  <|> try (string "multiplied by" >> return (*))
  <|> try (string "divided by" >> return div)
