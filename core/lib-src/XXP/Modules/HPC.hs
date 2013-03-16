{-# LANGUAGE RecordWildCards, DeriveDataTypeable #-}
module XXP.Modules.HPC ( hpcSpawn
                       , HPCConfig(..)
                       ) where

import Data.Data 
import Data.Generics 

import Text.Hastache 
import Text.Hastache.Context 

import XXP.Experiment
import XXP.State
import XXP.Logging
import XXP.Process
import XXP.IPC

data HPCConfig = HPCConfig { submissionCommand :: String
                           , jobFileTemplate :: FilePath
                           , remoteDataDir :: FilePath
                           , remoteBinDir :: FilePath
                           , userName :: String
                           , headnodeServer :: String
                           } deriving (Show, Read, Eq)

data JobFile = JobFile { jobname :: String
                       , ldd_path :: String
                       , working_dir :: String
                       } deriving (Data, Typeable)
                 
hpcSpawn :: String -> HPCConfig -> XXP ()
hpcSpawn binary HPCConfig{..} = do
  let jobFile = JobFile { jobname = "test"
                        , ldd_path = "test"
                        , working_dir = "test"
                        }
  res <- liftIO $ hastacheFile defaultConfig jobFileTemplate
         (mkGenericContext jobFile) 
  return ()
