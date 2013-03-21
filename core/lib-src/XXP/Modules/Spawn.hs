{-# LANGUAGE RecordWildCards #-}
module XXP.Modules.Spawn ( spawn
                         , spawnWithMPI
                         , defaultMPIConfig
                         , MPIConfig(..)
                         ) where

import Prelude hiding (log)

import Control.Proxy
import Control.Lens
import Control.Monad

import Data.Maybe
import Data.List
import Data.Aeson
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSC

import System.FilePath
import System.Directory
import System.Process
import System.IO
import System.FilePath.Glob

import XXP.Experiment
import XXP.State
import XXP.Logging
import XXP.Process
import XXP.IPC
import XXP.Util
import qualified XXP.PostProcessing.Logs as Logs

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

-- No blocking needed unless mpi bridge is used
cmdHandler _ (BLK _) = do return ACK
cmdHandler _ (UBL _) = do return ACK

cmdHandler _ (PIP sink logID dataID) =
  do log DEBUG $ "Seting up pipe " ++ sink ++
       " with log " ++ logID ++ " and data-id " ++ dataID
     lg <- liftIO $ Logs.loadLog $ "log" </> logID
     let dd = Logs.dataDir lg
     contents <- liftIO $ liftM (head . fst) $
                 globDir [compile $ dataID ++ "*"] dd
     liftIO $ putStrLn $ show contents
     h <- liftIO $ openFile (head contents) ReadMode 
     let pipeInfo = (h, tail contents)
     st <- get
     put $ st { pipes = Map.insert sink pipeInfo (pipes st) }
     log DEBUG $ "Pipe setup: " ++ sink
     return ACK

cmdHandler _ (EOF sink) = 
  do log DEBUG $ "Check for eof " ++ sink
     st <- get
     let (h, files) = fromJust $ Map.lookup sink (pipes st)
     eof <- liftIO $ hIsEOF h
     if eof then
       (if length files > 0 then
         return $ STR "1"
          else return ACK)
       else return $ STR "1"

cmdHandler _ (RQD sink lines) =
  do log DEBUG $ "Request data on " ++ sink
     let n = (read lines)::Int
     st <- get
     let (h, files) = fromJust $ Map.lookup sink (pipes st)
     eof <- liftIO $ hIsEOF h 
     h' <- if eof then
             do liftIO $ hClose h
                if length files > 0 then do
                  hn <- liftIO $ openFile (head files) ReadMode
                  let pipeInfo = (hn, tail files)
                  put $ st { pipes = Map.adjust (const pipeInfo) sink
                                     (pipes st)
                           }
                  return hn
                  else liftIO $ throwIO $ ErrorCall "eof pipe reached"
           else return h
     ls <- forM (take n $ repeat $ h') (liftIO . hGetLine)
     return $ STR $ intercalate "\n" ls

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
  writeLogFile "running" (show True)
  st <- get
  -- Copy the binary used
  liftIO $ copyFile ("build" </> binary) ((logLocation $ loggingState st) </> "binary")
  exitCode <- customProc "run" execCommand
    (instanceArg instances ++ [ bridgeCommand
                              , (".." </> "build" </> binary)
                              , ".." </> configPath st
                              , ".." </> dataLogLocation (dataState st)
                              ])
  liftIO $ removeIfExists (logLocation (loggingState st) </> "running")
  writeLogFile "exit" (show exitCode)


configPath st = (logLocation $ loggingState st) </> "config.json"

spawn :: String -> XXP ()
spawn binary = do
  dataFileName <- addDataFile "main"
  writeLogFile "running" (show True)
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
  liftIO $ removeIfExists (logLocation (loggingState st) </> "running")
  writeLogFile "exit" (show exitCode)

