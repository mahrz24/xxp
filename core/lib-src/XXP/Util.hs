module XXP.Util ( ifJust
                , fatalCatch
                ) where

import Prelude hiding (log)

import XXP.State
import XXP.Logging

import Control.Exception.Lifted

import Data.Maybe

import System.Exit

ifJust :: Monad m => (a -> m ()) -> Maybe a -> m ()
ifJust = maybe (return ())

fatalCatch :: String -> XXP a -> XXP a
fatalCatch s f = catch f (\e -> do log ERROR $ s ++
                                     show (e :: SomeException)
                                   liftIO $ exitWith (ExitFailure 1))
