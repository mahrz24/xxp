{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
module XXP.Modules.HPC ( hpcSpawn
                       , HPCConfig(..)
                       ) where

import Data.Aeson
import Data.Data 
import Data.Generics
import qualified Data.ByteString.Lazy.Char8 as BSC

import Text.Hastache 
import Text.Hastache.Context 

import System.FilePath

import XXP.Experiment
import XXP.State
import XXP.Logging
import XXP.Process
import XXP.IPC

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
                       , working_dir :: String
                       , data_dir :: String
                       , log_dir :: String
                       , bridge_dir :: String
                       , binary :: String
                       , config :: String
                       } deriving (Data, Typeable)
                 
hpcSpawn :: String -> HPCConfig -> XXP ()
hpcSpawn bin HPCConfig{..} = do
  st <- get
  let dataFilePath = remoteDataDir </>
                     (logLocation (loggingState st)) </> "data"
      logFilePath = remoteDataDir </>
                     (logLocation (loggingState st))
      binaryPkgPath = remoteBinDir </> uniqueID st
      configPath = binaryPkgPath </> "config.json"
      libPath = binaryPkgPath </> "lib"
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
  return ()

shellConfig :: MuConfig IO
shellConfig = defaultConfig { muEscapeFunc = emptyEscape }
  
