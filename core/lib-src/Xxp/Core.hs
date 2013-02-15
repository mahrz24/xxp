module Xxp.Core 
       ( runXXP
       , loadConfiguration
       , spawn
       ) where

import Control.Monad.Trans.State
import Control.Monad
import Control.Monad.IO.Class

import Data.Maybe
import Data.List
import Data.Time.Clock
import Data.UUID.V1
import qualified Data.UUID as UUID

import Text.JSON

import System.Environment
import System.FilePath
import System.Log.Logger

data LoggingState = LoggingState { dataLogLocation :: Maybe FilePath
                                 , mirrorDataLogsInRunLogs :: Bool
                                 , fileLogLevel :: Priority
                                   -- Passed from xxp binary 
                                   -- concerns the runtime log level 
                                   -- for stdout
                                 , consoleLogLevel :: Priority
                                   -- Keep track of all data log files 
                                 , dataLogFiles :: [String]
                                 } deriving (Show, Eq)
                    
data ST = ST { experimentName :: String
             , tag :: String
             , uuid :: String
             , timestamp ::  UTCTime
             , loggingState :: LoggingState
             , experimentConfig :: String
             } deriving (Show,Eq)

type XXP = StateT ST IO

initialState :: IO ST
initialState = do
  time <- getCurrentTime
  name <- getProgName
  args <- getArgs
  gduuid <- nextUUID
  let l = LoggingState { dataLogLocation = Nothing
                      , mirrorDataLogsInRunLogs = False
                      , fileLogLevel = INFO
                      , consoleLogLevel = WARNING
                      , dataLogFiles = []
                      }
  return $ ST { experimentName = name
              , tag = args !! 0
              , uuid = fromMaybe "" (fmap UUID.toString gduuid)
              , timestamp = time
              , loggingState = l
              , experimentConfig = ""
              }

loadConfiguration :: XXP ()
loadConfiguration = return ()

spawn :: XXP ()
spawn = return ()

runXXP :: XXP () -> IO ()
runXXP xp = do 
  state <- initialState
  liftM fst $ runStateT (wrapExperiment xp) state

wrapExperiment :: XXP () -> XXP ()
wrapExperiment xp = do
  st <- get
  liftIO $ putStrLn (show st)
  -- Get Arguments
  -- Get Timestamp
  -- Get UUID
  -- Load log.json
  xp
  -- Clean up