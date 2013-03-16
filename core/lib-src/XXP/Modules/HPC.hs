{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveDataTypeable #-}
module XXP.Modules.HPC ( hpcSpawn
                       , HPCConfig(..)
                       ) where

import Control.Monad

import Data.List.Split
import Data.Aeson
import Data.Data 
import Data.Generics
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.ByteString.Char8 as BSS

import Text.Hastache 
import Text.Hastache.Context 

import System.FilePath
import System.Directory

import HSH

import XXP.Experiment
import XXP.State
import XXP.Logging
import XXP.Process
import XXP.IPC
import XXP.Modules.Shell

data HPCConfig = HPCConfig { submissionCommand :: String
                           , jobFileTemplate :: FilePath
                           , remoteDataDir :: FilePath
                           , remoteBinDir :: FilePath
                           , bridgeDir :: FilePath
                           , userName :: String
                           , headnodeServer :: String
                           } deriving (Show, Read, Eq)

data JobFile = JobFile { jobname :: String
                       , ld_path :: String
                       , bridge_dir :: String
                       , working_dir :: String
                       , data_dir :: String
                       , log_dir :: String
                       , binary :: String
                       , config :: String
                       } deriving (Data, Typeable)
                 
hpcSpawn :: String -> HPCConfig -> XXP ()
hpcSpawn bin HPCConfig{..} = do
  st <- get
  -- Prepare the job.sh file
  let dataFilePath = remoteDataDir </>
                     (logLocation (loggingState st)) </> "data"
      logFilePath = remoteDataDir </>
                     (logLocation (loggingState st))
      localLogPath = logLocation (loggingState st)
      localBinaryPath = "build" </> bin
      binaryPkgPath = remoteBinDir </> uniqueID st
      configPath = binaryPkgPath </> "config.json"
      libPath = binaryPkgPath
      bundlePath = (localLogPath </> "bundle")
      jobFile = JobFile { jobname = shortID st
                        , config = configPath 
                        , data_dir = dataFilePath
                        , log_dir = logFilePath
                        , working_dir = binaryPkgPath
                        , bridge_dir = bridgeDir
                        , ld_path = libPath
                        , binary = bin
                        }
                
  res <- liftIO $ hastacheFile shellConfig jobFileTemplate
         (mkGenericContext jobFile)
  writeLogFile jobFileTemplate $ BSC.unpack $ res
  -- Create the bundle
  liftIO $ createDirectoryIfMissing True bundlePath
  liftIO $ copyFile localBinaryPath (bundlePath </> bin)
  rawLibs <- shellLines $ "ldd " ++ localBinaryPath
  let libs = filter noTab $ map (\x -> head $ splitOn " ("
                                        $ secondOrHead (splitOn "=> " x))
              $ rawLibs
  forM libs (\lib -> liftIO $ copyFile lib $ bundlePath </> (takeFileName lib))
  liftIO $ copyFile (localLogPath </> "config.json")
    (bundlePath </> "config.json")
  runDir <- liftIO $ pwd
  liftIO $ cd bundlePath
  shellExec $ "tar -czf ../bundle.tgz *"
  liftIO $ cd runDir
-- Create the job_prep.sh
  res <- liftIO $ hastacheStr shellConfig jobPrep
         (mkGenericContext jobFile)
  writeLogFile "job_prep.sh" $ BSC.unpack $ res
  liftIO $ writeFile "start_job.sh" (jobStart localLogPath)
  return ()
    where secondOrHead xs = if length xs == 1 then
                              head xs else xs !! 1
          noTab xs = (head xs /= '\t')
          remoteLoc = userName ++ "@" ++ headnodeServer ++ ":" ++ remoteBinDir
          jobPrep = BSS.unlines [ "#!/bin/sh -f"
                                , "mkdir -p {{data_dir}}"
                                , "mkdir -p {{working_dir}}"
                                , "mv bundle.tgz {{working_dir}}"
                                , "mv job.sh {{working_dir}}"
                                , "cd {{working_dir}}"
                                , "tar -xzf bundle.tgz"
                                , "rm bundle.tgz"
                                ]
          jobStart l = unlines
                       [ "#!/bin/sh -f"
                       , "scp " ++ l </> "job.sh " ++ remoteLoc
                       , "scp " ++ l </> "job_prep.sh " ++ remoteLoc
                       , "scp " ++ l </> "bundle.tgz" ++ remoteLoc
                       ]
                    
shellConfig :: MuConfig IO
shellConfig = defaultConfig { muEscapeFunc = emptyEscape }
  
