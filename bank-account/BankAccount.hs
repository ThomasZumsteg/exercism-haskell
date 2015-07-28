module BankAccount (BankAccount, openAccount, 
  closeAccount, getBalance, incrementBalance) where

import Control.Concurrent
import Control.DeepSeq

newtype BankAccount = BankAccount { accessAccout :: MVar (Maybe Int) }

openAccount :: IO BankAccount
openAccount = do
  v <- newMVar (Just 0)
  let b = BankAccount v
  return b

closeAccount :: BankAccount -> IO ()
closeAccount acct = do
  let a = accessAccout acct
  _ <- swapMVar a Nothing
  return ()

getBalance :: BankAccount -> IO (Maybe Int)
getBalance acct = do
  let a = accessAccout acct
  readMVar a

incrementBalance :: BankAccount -> Int -> IO (Maybe Int)
incrementBalance acct delta = do
  let a = accessAccout acct
  bal <- takeMVar a
  let newBal = (+ delta) <$> bal
  putMVar a $!! newBal
  return newBal
