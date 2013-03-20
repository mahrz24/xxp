{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}

module XXP.Core(runXXP) where

import Prelude hiding (log)

import Control.Monad
import Control.Exception.Lifted
import Control.Monad.Trans.State.Strict

import System.Log.Logger (errorM)
import System.FilePath
import System.Exit

import XXP.Experiment
import XXP.State
import XXP.Logging
import XXP.Setup
import XXP.Util
import XXP.Process

wrapExperiment :: XXP () -> XXP ()
wrapExperiment xp = do
  st <- get
  fatalCatch' "Error during setup: "
    (do -- Create the required directories
        setupDirectories
        -- Setup logging
        setupLogging)
  log NOTICE "Preparing"
  log DEBUG "Loading configuration"
  -- Load the configuration
  fatalCatch' "Error while loading configuration: " loadConfiguration
  log DEBUG "Saving configuration"
  -- Everything setup? Then save the configuration
  fatalCatch "Error while saving configuration: " saveIdentifierAndConfig
  log NOTICE "Starting"
  fatalCatch "Error while running experiment: " xp
  log NOTICE "Cleanup"
  -- Clean up
  -- Copy contents of run directory
  log NOTICE "Done"
  -- Write success to log directory (this is what the log viewer uses)
  writeLogFile "success" (show True)
  -- Public Functions

runXXP :: XXP () -> IO ()
runXXP xp = do
  -- Initialize the state
  state <- catch initialState
           (\e -> do errorM "xxp.core" $
                       "Initialization error: "
                       ++ show (e :: SomeException)
                     exitWith (ExitFailure 1))
  -- Run the experiment
  catch (liftM fst $ runStateT (wrapExperiment xp) state)
    (\e -> do errorM "xxp.core" $ "Exiting with error: "
                ++ show (e :: SomeException)
              exitWith (ExitFailure 1))
