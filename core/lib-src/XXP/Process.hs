module XXP.Process ( forkIOinXXP
                   , wait
                   , customProc
                   , customProc'
                   , ExitCode(..)
                   ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Proxy

import System.Process
import System.Exit
import System.IO

import XXP.State
import XXP.Logging

newtype Wait a = Wait (TVar (Maybe a))

forkIOinXXP :: IO a -> XXP (Wait a)
forkIOinXXP m = do
  w <- liftIO $ atomically (newTVar Nothing)
  liftIO $ forkIO (m >>= atomically . writeTVar w . Just)
  return (Wait w)

wait :: Wait a -> XXP a
wait (Wait w) = liftIO $ atomically $ do
  r <- readTVar w
  case r of
    Just a -> return a
    Nothing -> retry

customProc' ::    String
               -> String
               -> String
               -> [String]
               -> (Handle -> XXP ())
               -> XXP ExitCode
customProc' dir p pd args f = do
  st <- get
  (Just hIn, Just hOut, Just hErr, hProc) <- liftIO $
    createProcess (proc p args)
    { std_out = CreatePipe
    , std_err = CreatePipe
    , std_in = CreatePipe
    , cwd = Just dir
    }
  oid <- forkIOinXXP $ 
    runProxy $ hGetLineS hOut >-> logD' NOTICE pd st
  eid <- forkIOinXXP $ 
    runProxy $ hGetLineS hErr >-> logD' ERROR (pd ++ ": error") st
    
  f hIn
  
  wait oid
  wait eid
  liftIO $ waitForProcess hProc

customProc :: String -> String -> [String] -> XXP ExitCode
customProc dir p args = customProc' dir p p args closeIn
  where closeIn hIn = liftIO $ hClose hIn

