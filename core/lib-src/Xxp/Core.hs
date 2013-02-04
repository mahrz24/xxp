module Xxp.Core 
       ( runXXP
       , loadConfiguration
       , spawn
       ) where

import Control.Monad.Trans.State
import Control.Monad
import Control.Monad.IO.Class

data ST = ST Int

type XXP = StateT ST IO

initialState = ST 0

loadConfiguration :: XXP ()
loadConfiguration = do put $ ST 1
                       return ()

spawn :: XXP ()
spawn = do (ST x) <- get
           liftIO $ putStrLn (show x)
           return ()

runXXP :: XXP () -> IO ()
runXXP xp = liftM fst $ runStateT (wrapExperiment xp) initialState

wrapExperiment :: XXP () -> XXP ()
wrapExperiment xp = do
  -- Get Arguments
  -- Get Timestamp
  -- Get UUID
  -- Load log.json
  xp
  -- Clean up