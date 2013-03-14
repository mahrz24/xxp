module XXP.Logging ( log
                   , loggerLevel
                   , throwOnLeft
                   , throwXXP
                   , logD'
                   , logD
                   , writeLogFile
                   , Priority(..)
                   , ErrorCall(..)
                   ) where

import Prelude hiding (log)

import Control.Proxy
import Control.Exception.Lifted

import Data.Maybe

import System.Log
import System.Log.Logger
import System.FilePath

import XXP.State
import XXP.Experiment

log :: Priority -> String -> XXP ()
log l m = do
  st <- get
  liftIO $ logM ("xxp." ++ experimentName (identifier st)) l m

loggerLevel = fromMaybe NOTICE . getLevel

throwOnLeft _ (Right v) = return v
throwOnLeft f (Left e)  = liftIO $ throwIO (f e)

throwXXP e = liftIO $ (throwIO e)

logD :: (Proxy p) => Priority -> XPState -> () -> Consumer p String IO r
logD pr = logD' pr "binary"

logD' :: (Proxy p)
         => Priority
         -> String
         -> XPState
         -> ()
         -> Consumer p String IO r
logD' pr ln st () = runIdentityP $ forever $ do
  a <- request ()
  lift $ logM ("xxp." ++ experimentName (identifier st) ++ "." ++ ln)
    pr (ln ++ ": " ++ a)



writeLogFile n c = do
  st <- get
  liftIO $ writeFile (logLocation (loggingState st) </> n)
      c
