module Deque (mkDeque, push, pop, shift, unshift) where

import Control.Concurrent

type Queue a = MVar [a]

mkDeque :: IO (Queue a)
mkDeque = do
  q <- newMVar []
  return q

push :: Queue a -> a -> IO ()
push queue item = do
  items <- takeMVar queue
  let new_items = items ++ [item]
  putMVar new_items queue
  return ()

pop :: Queue a -> Maybe Char
pop queue = do
  (x:xs) <- takeMVar queue
  putMVar xs queue
  return Just x

shift :: Queue a -> a -> IO ()
shift _ _ = undefined

unshift :: Queue a -> Maybe a
unshift _ = undefined