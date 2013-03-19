{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module XXP.PostProcessing.Logs where

import Control.Monad

import Data.Aeson hiding (Success)
import qualified Data.ByteString.Lazy as BS

import System.Exit
import System.FilePath
import System.Directory

import qualified XXP.Experiment as XP
import XXP.Experiment (Identifier)
import XXP.Modules.HPC
import XXP.Util

data DataLocation = Local
                  | Linked FilePath
                    -- (jobid, fetched?, config)
                  | Remote (String, Bool, HPCConfig) deriving (Show, Eq)

data ExperimentExit = Success
                    | Fail
                    | BinaryFail ExitCode
                    | Running deriving (Show, Eq)

data Log = Log { logDir :: FilePath
               , identifier :: Identifier
               , experimentDataLocation :: DataLocation
               , experimentExit :: ExperimentExit
               } deriving (Show, Eq)



loadLog :: FilePath -> IO Log
loadLog logDir = do
  identifier <- (decodeOrError "id.json") =<<
                BS.readFile (logDir </> "id.json")
  isRemote <- doesFileExist (logDir </> "jobid")
  (experimentDataLocation, experimentExit) <- if(isRemote) then remoteLoad
                                              else localLoad
  return Log {..}
  where remoteLoad =
          (do jobID <- readFile (logDir </> "jobid")
              hpc <- (decodeOrError "hpc.json") =<<
                     BS.readFile (logDir </> "hpc.json")
              ee <- exitState
              isFetched <- doesFileExist "exit"
              return (Remote (jobID, isFetched, hpc), ee))
        localLoad =
          (do isLinked <- doesFileExist (logDir </> "data.link")
              edl <- if isLinked then
                       (do path <- readFile (logDir </> "data.link")
                           return $ Linked path)
                     else return Local
              isDone <- doesFileExist "success"
              ee <- exitState
              return (edl,ee))
        exitState =
          (do isExited <- doesFileExist "exit"
              isSuccess <- doesFileExist "success"
              isRunning <- doesFileExist "running"
              if (isRunning) then
                return Running
                else if (isExited && isSuccess) then
                       do exc <- liftM read $ readFile (logDir </> "exit")
                          if exc == ExitSuccess then
                            return Success
                            else return $ BinaryFail exc
                     else return Fail)

loadLogs :: [FilePath] -> IO Log
loadLogs = undefined

loadAllLogs :: IO [Log]
loadAllLogs = undefined

loadAllLogsWhere :: (Log -> Bool) -> IO [Log]
loadAllLogsWhere = undefined

findLog :: String -> IO FilePath
findLog = undefined

findLogs :: String -> IO [FilePath]
findLogs = undefined

removeAllLogs :: IO ()
removeAllLogs = undefined

removeLog :: Log -> IO ()
removeLog = undefined

removeLogs :: [Log] -> IO ()
removeLogs = undefined

failed :: Log -> Bool
failed = undefined

success :: Log -> Bool
success = undefined

running :: Log -> Bool
running = undefined

remote :: Log -> Bool
remote = undefined

tagMatches :: String -> Log -> Bool
tagMatches = undefined

experimentMatches :: String -> Log -> Bool
experimentMatches = undefined

anyMatches :: String -> Log -> Bool
anyMatches = undefined



