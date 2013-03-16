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
  exitCode <- customProc "run" execCommand
    (instanceArg instances ++ [ bridgeCommand
                              , (".." </> "build" </> binary)
                              , BSC.unpack (encode $ experimentConfig st)
                              , ".." </> dataLogLocation (dataState st)
                              ])
  writeLogFile "exit" (show exitCode)

spawn :: String -> XXP ()
spawn binary = do
  dataFileName <- addDataFile "main"
  st <- get
  let dataFilePath = (dataLogLocation (dataState st) </> dataFileName)
  dataFile <- liftIO $ openFile dataFilePath WriteMode

  writeLogFile "debug" (show $ debugMode . identifier $ st)

  -- Start the server from which data logs are received
  exitCode <- withIPC $ \ipc -> (customProc' "run"
                                 (".." </> "build" </> binary)
                                 "binary"
                                 [ "s"
                                 , socketName ipc
                                 , BSC.unpack (encode $ experimentConfig st)
                                 ]
                                 (serverHandler ipc
                                  (cmdHandler dataFile))
                                )

  liftIO $ hClose dataFile
                         
  writeLogFile "exit" (show exitCode)

