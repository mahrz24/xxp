{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module XXP.PostProcessing.Logs where

import Control.Monad
import Control.Exception

import Data.List
import Data.Aeson hiding (Success)
import qualified Data.ByteString.Lazy as BS

import System.Exit
import System.FilePath
import System.Directory
import System.FilePath.Glob

import System.IO

import Text.Printf

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
               , marked :: Bool
               , identifier :: Identifier
               , experimentDataLocation :: DataLocation
               , dataSize :: Integer
               , experimentExit :: ExperimentExit
               } deriving (Show, Eq)

{- From Haskell Beginners -}
filesize :: FilePath -> IO Integer
filesize path = catch (withFile path ReadMode hFileSize)
                ((const (return 0)) :: (IOException -> IO Integer))

getVisible = filter (`notElem` [ "." , ".." ])

ds :: FilePath -> IO Integer
ds path = do
    contents <- getDirectoryContents path `catch` ((const (return [])) :: (IOException -> IO [String]))
    let visibles = getVisible contents
    let path' = clrSlash path
    a <- (liftM sum) $ sequence $ map (\p -> filesize (path' </> p))
         visibles -- size of a current dir
    (liftM ((+a) . sum)) $ mapM (\p -> ds (path' </> p)) visibles
      -- current + children
      
clrSlash     = reverse . dropWhile (\c -> c =='/' || c == '\\') . reverse

shred [] = []
shred ss = (take 3 ss) : (shred (drop 3 ss))

prettyNum = concat . intersperse " " . (map reverse) . reverse . shred . reverse

units :: Integer -> [Integer]
units 0 = [0]
units x = x : units' x
    where
      units' 0 = []
      units' x = y : units' y
          where
            y = round (fromIntegral x / 1024)

prefix = ["B", "K", "M", "G", "T", "P", "E", "Z", "Y"]

tagged x = (map (prettyNum . show) $ units x) `zip` prefix

printSize x width = printf "%*s %s" width (fst one) (snd one)
  where one = head $ dropWhile (\p -> length (fst p) > width) $ tagged x

{- End From Haskell Beginners -}



loadLog :: FilePath -> IO Log
loadLog logDir = do
  identifier <- (decodeOrError "id.json") =<<
                BS.readFile (logDir </> "id.json")
  isRemote <- doesFileExist (logDir </> "jobid")
  (experimentDataLocation, experimentExit) <- if(isRemote) then remoteLoad
                                              else localLoad
  marked <- doesFileExist (logDir </> "marked")
  dataSize <- ds (logDir </> "data/")
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
              isDone <- doesFileExist (logDir </> "success")
              ee <- exitState
              return (edl,ee))
        exitState =
          (do isExited <- doesFileExist (logDir </> "exit")
              isSuccess <- doesFileExist (logDir </> "success")
              isRunning <- doesFileExist (logDir </> "running")
              if (isRunning) then
                return Running
                else if (isExited && isSuccess) then
                       do exc <- liftM read $ readFile (logDir </> "exit")
                          if exc == ExitSuccess then
                            return Success
                            else return $ BinaryFail exc
                     else return Fail)

loadLogs :: [FilePath] -> IO [Log]
loadLogs logDirs = mapM loadLog logDirs

loadAllLogs :: IO [Log]
loadAllLogs = do contents <- getDirectoryContents "log"
                 let relevant = map ((++) "log/")
                                $ filter (`notElem` [".", ".."]) contents
                 dirs <- filterM doesDirectoryExist relevant
                 mapM loadLog relevant

loadLogsWhere :: (Log -> Bool) -> IO [Log]
loadLogsWhere f = liftM (filter f) loadAllLogs

findLog :: String -> IO Log
findLog p = (findLogs p) >>= (\ls -> return $ head ls)

findLogs :: String -> IO [Log]
findLogs p = loadLogs =<< liftM (head . fst)
                (globDir [compile $ "*" ++ p ++ "*"] "log")

sortLogs :: [Log] -> [Log]
sortLogs = sortBy cmpDate
  where cmpDate lga lgb = compare (XP.timestamp $ identifier lgb)
                            (XP.timestamp $ identifier lga)

removeAllLogs :: IO ()
removeAllLogs = do contents <- getDirectoryContents "./log"
                   let relevant = map ((++) "log/")
                                  $ filter (`notElem` [".", ".."]) contents
                   dirs <- filterM doesDirectoryExist relevant
                   mapM_ removeDirectoryRecursive dirs

removeLog :: Log -> IO ()
removeLog lg = removeDirectoryRecursive $ logDir lg

unmarkLog :: Log -> IO ()
unmarkLog lg = removeIfExists ((logDir lg) </> "marked")

markLog :: Log -> IO ()
markLog lg = writeFile ((logDir lg) </> "marked") ""

failed :: Log -> Bool
failed lg = case (experimentExit lg) of
  Fail -> True
  BinaryFail _ -> True
  _ -> False

success :: Log -> Bool
success lg = (experimentExit lg) == Success

running :: Log -> Bool
running lg = (experimentExit lg) == Running

remote :: Log -> Bool
remote lg = case (experimentDataLocation lg) of
  Remote _ -> True
  _ -> False

pmatch s = match (compile $ "*" ++ s ++ "*") 

tagMatches :: String -> Log -> Bool
tagMatches s lg = pmatch s (XP.tag $ identifier $ lg)

idMatches :: String -> Log -> Bool
idMatches s lg = pmatch s (XP.uniqueID' $ identifier $ lg)

experimentMatches :: String -> Log -> Bool
experimentMatches s lg = pmatch s (XP.experimentName $ identifier $ lg)

anyMatches :: String -> Log -> Bool
anyMatches s lg = (tagMatches s lg)
                  || (experimentMatches s lg)
                  || (idMatches s lg)



