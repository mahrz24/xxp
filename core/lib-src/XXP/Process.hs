module XXP.Process ( fork
                   , wait
                   , customProc
                   , ExitCode(..)
                   ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Proxy

import System.Process
import System.Exit

import XXP.State
import XXP.Logging

newtype Wait a = Wait (TVar (Maybe a))

fork :: IO a -> IO (Wait a)
fork m = do
  w <- atomically (newTVar Nothing)
  forkIO (m >>= atomically . writeTVar w . Just)
  return (Wait w)

wait :: Wait a -> IO a
wait (Wait w) = atomically $ do
  r <- readTVar w
  case r of
    Just a -> return a
    Nothing -> retry

customProc :: String -> String -> [String] -> XXP ExitCode
customProc dir p args = do
  st <- get
  liftIO $ do
    (_, Just hOut, Just hErr, hProc) <- createProcess (proc p args)
      { std_out = CreatePipe
      , std_err = CreatePipe
      , cwd = Just dir
      }
    oid <- fork $ runProxy $ hGetLineS hOut >-> logD' NOTICE p st
    eid <- fork $ runProxy $ hGetLineS hErr >-> logD' ERROR (p ++ ": error") st
    wait oid
    wait eid
    waitForProcess hProc

