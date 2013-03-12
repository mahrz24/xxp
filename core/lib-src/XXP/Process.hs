module XXP.Process ( fork
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

customProc' ::    String
               -> String
               -> String
               -> [String]
               -> (Handle -> Handle -> Handle -> IO ())
               -> XXP ExitCode
customProc' dir p pd args f = do
  st <- get
  liftIO $ do
    (Just hIn, Just hOut, Just hErr, hProc) <- createProcess (proc p args)
      { std_out = CreatePipe
      , std_err = CreatePipe
      , std_in = CreatePipe
      , cwd = Just dir
      }
    oid <- fork $ runProxy $ hGetLineS hOut >-> logD' NOTICE pd st
    eid <- fork $ runProxy $ hGetLineS hErr >-> logD' ERROR (pd ++ ": error") st
    f hIn hOut hErr
    wait oid
    wait eid
    waitForProcess hProc

customProc :: String -> String -> [String] -> XXP ExitCode
customProc dir p args = customProc' dir p p args closeIn
  where closeIn hIn _ _ = hClose hIn

