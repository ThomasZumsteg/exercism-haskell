module Deque (mkDeque, push, pop, shift, unshift) where

import Control.Concurrent

newtype Queue a = Queue { get_items :: MVar [a] }

mkDeque :: IO (Queue a)
mkDeque = do
  mvar <- newMVar []
  let q = Queue mvar
  return q

push :: Queue a -> a -> IO ()
push queue item = do
  let mvar = get_items queue
  items <- takeMVar mvar
  let new_items = items ++ [item]
  putMVar mvar new_items
  return ()

pop :: Queue a -> IO (Maybe a)
pop queue = do
  let mvar = get_items queue
  items <- takeMVar mvar
  let 
    (x, xs) = case items of
      [] -> (Nothing, [])
      i -> (Just $ last i, init i)
  putMVar mvar xs
  return x

unshift :: Queue a -> a -> IO ()
unshift queue item = do
  let mvar = get_items queue
  items <- takeMVar mvar
  let new_items = item : items
  putMVar mvar new_items
  return ()

shift :: Queue a -> IO (Maybe a)
shift queue = do
  let mvar = get_items queue
  items <- takeMVar mvar
  let 
    (x, xs) = case items of
      [] -> (Nothing, [])
      i -> (Just $ head i, tail i)
  putMVar mvar xs
  return x