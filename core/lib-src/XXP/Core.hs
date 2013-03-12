{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}

module XXP.Core(runXXP) where

import Prelude hiding (log)

import Control.Monad
import Control.Exception.Lifted

import Control.Applicative

import Control.Proxy

import Data.Dynamic
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Time.Clock
import Data.Time.Format
import Data.UUID.V1
import qualified Data.UUID as UUID

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as T
import Data.Text (Text)

import Text.Read (readEither, readMaybe)

import System.Environment
import System.FilePath
import System.Directory
import System.Log.Logger

import System.Locale
import System.Process
import System.IO
import System.Exit

import HSH

import Network

import XXP.Experiment
import XXP.State
import XXP.Logging
import XXP.Setup
import XXP.Util
import XXP.Process

wrapExperiment :: XXP () -> XXP ()
wrapExperiment xp = do
  st <- get
  fatalCatch "Error during setup: "
    (do -- Create the required directories
        setupDirectories
        -- Setup logging
        setupLogging)
  log NOTICE "Preparing"
  log DEBUG "Loading configuration"
  -- Load the configuration
  fatalCatch "Error while loading configuration: " loadConfiguration
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
  liftIO $ writeFile (logLocation (loggingState st) </> "success") (show True)
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
