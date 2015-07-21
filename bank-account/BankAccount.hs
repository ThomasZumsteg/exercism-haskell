module BankAccount (BankAccount, openAccount, 
  closeAccount, getBalance, incrementBalance) where

import Control.Concurrent
import Prelude

newtype BankAccount = BankAccount { getVal :: MVar (Maybe Int) }

openAccount :: IO BankAccount
openAccount = BankAccount <$> newMVar (Just 0)

closeAccount :: BankAccount -> IO ()
closeAccount = flip putMVar Nothing . getVal

getBalance :: BankAccount -> IO (Maybe Int)
getBalance =  takeMVar . getVal

incrementBalance :: BankAccount -> Int -> IO (Maybe Int)
incrementBalance acct amount = do
  let b = getVal acct
  bal <- fmap (amount +) <$> takeMVar b
  putMVar b bal
  return bal