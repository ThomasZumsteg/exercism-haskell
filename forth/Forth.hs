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
import Data.Maybe (isJust, fromJust)
import Data.Either (isRight)
import Data.Char
import Data.List (foldl')

type Value = Int

data ForthState = ForthState {
  getStack :: [Value],
  getDefs :: Map.Map T.Text Operation,
  currentCommand :: T.Text
}

type Operation = ForthState -> Either ForthError ForthState

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord T.Text
     deriving (Show, Eq)

empty :: ForthState
empty = ForthState [] defaultWords T.empty

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

dupTwice :: Operation
dupTwice fs@(ForthState { getStack = stack } ) 
  | null stack = Left StackUnderflow
  | otherwise = Right $ fs { getStack = a:a:a:as }
  where 
    a:as = stack

drop' :: Operation
drop' fs@(ForthState { getStack = stack })
  | null stack = Left StackUnderflow
  | otherwise = Right $ fs { getStack = stack' }
  where
    stack' = tail stack

swap :: Operation
swap fs@(ForthState { getStack = stack }) 
  | length stack < 2 = Left StackUnderflow
  | otherwise = Right $ fs { getStack = stack' }
  where 
    a:b:cs = stack
    stack' = b:a:cs

over :: Operation
over fs@(ForthState { getStack = stack }) 
  | length stack < 2 = Left StackUnderflow
  | otherwise = Right $ fs { getStack = stack' }
  where 
    a:b:cs = stack
    stack' = b:a:b:cs

plus :: Operation
plus fs@(ForthState { getStack = stack }) 
  | length stack < 2 = Left StackUnderflow
  | otherwise = Right $ fs { getStack = stack' }
  where 
    a:b:cs = stack
    stack' = (a+b):cs

minus :: Operation
minus fs@(ForthState { getStack = stack }) 
  | length stack < 2 = Left StackUnderflow
  | otherwise = Right $ fs { getStack = stack' }
  where 
    a:b:cs = stack
    stack' = (b - a):cs

divide :: Operation
divide fs@(ForthState { getStack = stack })
  | length stack < 2 = Left StackUnderflow
  | a == 0 = Left DivisionByZero
  | otherwise = Right $ fs { getStack = stack' }
  where
    a:b:cs = stack
    stack' = (div b a):cs

multiply :: Operation
multiply fs@(ForthState { getStack = stack }) 
  | length stack < 2 = Left StackUnderflow
  | otherwise = Right $ fs { getStack = stack' }
  where
    a:b:cs = stack
    stack' = (a * b):cs

formatStack :: ForthState -> T.Text
formatStack = T.pack . unwords . map show . reverse . getStack

evalText :: T.Text -> ForthState -> Either ForthError ForthState
evalText text fs@(ForthState stack knownWords command)
  | (T.null text || T.null word) && T.null command = Right fs
  | isJust oper = (fromJust oper) fs >>= evalText remainer
  | word == T.pack ":" = newWordFS >>= evalText newWordText
  | isJust number = evalText remainer $ fs { getStack = (fromJust number):stack }
  | otherwise = Left $ UnknownWord word
  where 
    (word, remainer) = getWord text
    number = readNumber word
    (newWordText, newWordFS) = addToWords remainer fs
    oper = Map.lookup word knownWords

readNumber :: T.Text -> Maybe Value
readNumber word = case R.signed R.decimal word of
  Right (n, remainer) | T.null remainer -> Just n
  _ -> Nothing

getWord :: T.Text -> (T.Text, T.Text)
getWord = T.break isSep . T.dropWhile isSep . T.toLower
  where
    isSep c = isControl c || isSpace c

getWords :: T.Text -> [T.Text]
getWords text 
  | T.null text = []
  | otherwise = word:getWords remainer
  where (word, remainer) = getWord text

addToWords :: T.Text -> ForthState -> (T.Text, Either ForthError ForthState)
addToWords text fs@(ForthState { getDefs = knownWords } )
  | ';' /= T.head remainer = (text, Left InvalidWord)
  | isJust newOp = (T.tail remainer, Right $ fs { getDefs = Map.insert (T.pack "dup-twice") (fromJust newOp) knownWords })
  where
    (definition, remainer) = T.breakOn (T.pack ";") text
    (newWord: ops) = getWords remainer
    newOp = makeOpFromList ops knownWords

makeOpFromList :: [T.Text] -> Map.Map T.Text Operation -> Maybe Operation
makeOpFromList textOps definitions =
  let ops = map (\op -> Map.lookup op definitions) textOps
  in foldl' (>>=) (Just (Right . id)) ops
