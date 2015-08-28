{-# LANGUAGE OverloadedStrings #-}
module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , formatStack
  , empty
  ) where

import qualified Data.Text as T
import qualified Data.Text.Read as R
import qualified Data.Map as Map
import Data.Either (isRight)
import Data.Char
import Data.List (foldl')

data ForthState = ForthState {
  getStack :: [Int],
  getWords :: Map.Map T.Text Operation
}

type Operation = ForthState -> Either ForthError ForthState

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord T.Text
     deriving (Show, Eq)

empty :: ForthState
empty = ForthState [] defaultWords

defaultWords :: Map.Map T.Text Operation
defaultWords = Map.fromList $ map (\(f,s) -> (T.pack f, s)) [
  ("+", plus),
  ("-", minus),
  ("/", divide),
  ("*", multiply),
  ("dup", dup),
  ("drop", drop'),
  ("swap", swap),
  ("over", over) ]

dup :: Operation
dup fs@(ForthState { getStack = stack } ) 
  | null stack = Left StackUnderflow
  | otherwise = Right $ fs { getStack = a:a:as }
  where 
    a:as = stack

drop' :: Operation
drop' (ForthState stack knownWords)
  | null stack = Left StackUnderflow
  | otherwise = Right $ ForthState stack' knownWords
  where
    stack' = tail stack

swap :: Operation
swap (ForthState stack knownWords) 
  | length stack < 2 = Left StackUnderflow
  | otherwise = Right $ ForthState stack' knownWords
  where 
    a:b:cs = stack
    stack' = b:a:cs

over :: Operation
over (ForthState stack knownWords) 
  | length stack < 2 = Left StackUnderflow
  | otherwise = Right $ ForthState stack' knownWords
  where 
    a:b:cs = stack
    stack' = b:a:b:cs

plus :: Operation
plus (ForthState stack knownWords) 
  | length stack < 2 = Left StackUnderflow
  | otherwise = Right $ ForthState stack' knownWords
  where 
    a:b:cs = stack
    stack' = (a+b):cs

minus :: Operation
minus (ForthState stack knownWords) 
  | length stack < 2 = Left StackUnderflow
  | otherwise = Right $ ForthState stack' knownWords
  where 
    a:b:cs = stack
    stack' = (b - a):cs

divide :: Operation
divide (ForthState stack knownWords) 
  | length stack < 2 = Left StackUnderflow
  | a == 0 = Left DivisionByZero
  | otherwise = Right $ ForthState stack' knownWords
  where
    a:b:cs = stack
    stack' = (div b a):cs

multiply :: Operation
multiply (ForthState stack knownWords) 
  | length stack < 2 = Left StackUnderflow
  | otherwise = Right $ ForthState stack' knownWords
  where
    a:b:cs = stack
    stack' = (a * b):cs

formatStack :: ForthState -> T.Text
formatStack = T.pack . unwords . map show . reverse . getStack

evalText :: T.Text -> ForthState -> Either ForthError ForthState
evalText text fs@(ForthState stack knownWords)
  | T.null text || T.null word = Right fs
  | word `Map.member` knownWords = oper fs >>= evalText remainer
  | isRight number = let Right (n, _) = number
      in evalText remainer $ fs { getStack = n:stack }
      -- FIXME: add function to add words to known words
  | otherwise = Left $ UnknownWord word
  where 
    isSep c = isControl c || isSpace c
    (word, remainer) = T.break isSep $ T.dropWhile isSep $ T.toLower text
    number = R.signed R.decimal word
    oper = (Map.!) knownWords word