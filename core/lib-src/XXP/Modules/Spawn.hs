{-# LANGUAGE RecordWildCards #-}
module XXP.Modules.Spawn ( spawn
                         , spawnWithMPI
                         , defaultMPIConfig
                         , MPIConfig(..)
                         ) where

import Prelude hiding (log)

import Control.Proxy
import Control.Lens

import Data.Maybe
import Data.List
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSC

import System.FilePath
import System.Directory
import System.Process
import System.IO

import XXP.Experiment
import XXP.State
import XXP.Logging
import XXP.Process
import XXP.IPC

data MPIConfig = MPIConfig { bridgeCommand :: FilePath
                           , execCommand :: FilePath
                           , instances :: Maybe Int
                           } deriving (Show, Read, Eq)

defaultMPIConfig = MPIConfig { bridgeCommand = "mpibridge"
                             , execCommand = "mpiexec"
                             , instances = Nothing
                             }

instanceArg :: Maybe Int -> [String]
instanceArg = maybe [] (\i -> ["-n", show i])

cmdHandler :: Handle -> CommandHandler
cmdHandler h (DAT clientData) = do
  liftIO $ hPutStrLn h clientData
  return ACK

cmdHandler _ (RQF name) = do
  dataFileName <- addDataFile $ name
  st <- get
  return (STR $ ".." </> dataLogLocation (dataState st) </> dataFileName)

addDataFile :: String -> XXP String
addDataFile name = do
  st <- get
  let fullName = intercalate "." [ name
                                 , show $ length $ dataLogFiles $ dataState st
                                 , "dat"]
  put st{ dataState = (dataState st)
          { dataLogFiles =  fullName : (dataLogFiles $ dataState st) } }
  return fullName

spawnWithMPI :: String -> MPIConfig -> XXP ()
spawnWithMPI binary MPIConfig{..} = do
  st <- get
  -- Copy the binary used
  liftIO $ copyFile ("build" </> binary) ((logLocation $ loggingState st) </> "binary")
  exitCode <- customProc "run" execCommand
    (instanceArg instances ++ [ bridgeCommand
                              , (".." </> "build" </> binary)
                              , ".." </> configPath st
                              , ".." </> dataLogLocation (dataState st)
                              ])
  writeLogFile "exit" (show exitCode)


configPath st = (logLocation $ loggingState st) </> "config.json"

spawn :: String -> XXP ()
spawn binary = do
  dataFileName <- addDataFile "main"
  st <- get
  let dataFilePath = (dataLogLocation (dataState st) </> dataFileName)
  dataFile <- liftIO $ openFile dataFilePath WriteMode
  -- Copy the binary used
  liftIO $ copyFile ("build" </> binary) ((logLocation $ loggingState st) </> "binary")
  -- Write debug mode info
  writeLogFile "debug" (show $ debugMode . identifier $ st)
  -- Should we start the debugger?
  let binPath = (".." </> "build" </> binary)
  -- Start the server from which data logs are received
  exitCode <- withIPC $ \ipc -> (do let binArgs = [ "s"
                                                  , socketName ipc
                                                  , ".." </> configPath st
                                               ]
                                    if (gdb . identifier $ st) then
                                      (customProc' "run"
                                       "gdbserver"
                                       "gdb"
                                       "server"
                                       ([ "localhost:2486"
                                       , binPath  
                                       ] ++ binArgs)
                                       (serverHandler ipc
                                        (cmdHandler dataFile))
                                      )
                                      else (customProc' "run"
                                       binPath
                                       "binary"
                                       "error"
                                       binArgs
                                       (serverHandler ipc
                                        (cmdHandler dataFile))))

  liftIO $ hClose dataFile
                         
  writeLogFile "exit" (show exitCode)

