module BankAccount (BankAccount, openAccount, 
  closeAccount, getBalance, incrementBalance) where

import Control.Concurrent

newtype BankAccount = BankAccount { accessAccout :: MVar (Maybe Int) }

openAccount :: IO BankAccount
openAccount = BankAccount <$> newMVar (Just 0)

closeAccount :: BankAccount -> IO ()
closeAccount a = a >>= swapMVar (accessAccout a) Nothing

getBalance :: BankAccount -> IO (Maybe Int)
getBalance = readMVar . accessAccout

incrementBalance :: BankAccount -> Int -> IO (Maybe Int)
incrementBalance acct delta = do
  let b = accessAccout acct
  newBal <- fmap (delta +) <$> takeMVar b
  putMVar b newBal
  return newBal
