module XXP.Util ( ifJust
                , fatalCatch
                , fatalCatch'
                , decodeOrError
                , removeIfExists
                ) where

import Prelude hiding (log)

import XXP.State
import XXP.Logging
import XXP.Experiment

import Control.Exception.Lifted

import Data.Aeson
import Data.Maybe

import System.Exit
import System.Directory
import System.FilePath
import System.IO.Error hiding (catch)

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

ifJust :: Monad m => (a -> m ()) -> Maybe a -> m ()
ifJust = maybe (return ())

fatalCatch :: String -> XXP a -> XXP a
fatalCatch s f = catch f (\e -> do log ERROR $ s ++
                                     show (e :: SomeException)
                                   st <- get
                                   liftIO $ removeIfExists (logLocation (loggingState st) </> "running")
                                   liftIO $ exitWith (ExitFailure 1))

fatalCatch' :: String -> XXP a -> XXP a
fatalCatch' s f = catch f (\e -> do log ERROR $ s ++
                                      show (e :: SomeException)
                                    st <- get
                                    liftIO $ removeDirectoryRecursive (logLocation (loggingState st))
                                    liftIO $ exitWith (ExitFailure 1))
decodeOrError f j =
  throwOnLeft (\s -> ErrorCall $ "JSON parsing error in file: " ++ f) $
   eitherDecode j
