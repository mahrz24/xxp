{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Xxp.Core 
       ( runXXP
       , loadConfiguration
       , spawn
       ) where



import Control.Monad.Trans.State
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import Control.Applicative

import Data.Dynamic
import Data.Maybe
import Data.List
import Data.Time.Clock
import Data.Time.Format
import Data.UUID.V1
import qualified Data.UUID as UUID

import Data.Aeson
import qualified Data.ByteString.Lazy as By
import Data.ByteString.Lazy (ByteString)

import Text.Read (readEither, readMaybe)

import System.Environment
import System.FilePath
import System.Directory
import System.Log.Logger
import System.Locale

data LoggingState = LoggingState { dataLogLocation :: Maybe FilePath
                                 , fileLogLevel :: Priority
                                   -- Passed from xxp binary 
                                   -- concerns the runtime log level 
                                   -- for stdout
                                 , consoleLogLevel :: Priority
                                   -- Keep track of all data log files 
                                 , dataLogFiles :: [String]
                                 , logLocation :: FilePath
                                 } deriving (Show, Eq)
                    

instance FromJSON LoggingState where
     parseJSON (Object v) = do
       fileLogLevel <- either fail return =<< readEither <$> v .: "fileLogLevel"
       dataLogLocation <- v .:? "dataLogLocation"
       return $ LoggingState { consoleLogLevel = NOTICE
                             , dataLogFiles = []
                             , logLocation = ""
                             , ..
                             }
     -- A non-Object value is of the wrong type, so fail.
     parseJSON _          = mzero
                                            
data Identifier = Identifier { experimentName :: String
                             , tag :: String
                             , uuid :: String
                             , timestamp ::  UTCTime
                               -- Used to determine wheather the actual experiment 
                               -- binaries need to be recompiled
                             , debugMode :: Bool
                             } deriving (Show, Read, Eq)

instance FromJSON Identifier where
     parseJSON (Object v) = do
       experimentName <- v .: "experimentName"
       tag <- v .: "tag"
       uuid <- v .: "uuid"
       timestamp <- maybe (fail "Time parse error") return 
                    =<< (parseTime defaultTimeLocale "%Y%m%d%H%M%S") <$> v .: "timestamp"
       debugMode <- v .: "debugMode"
       return $ Identifier { .. }
     -- A non-Object value is of the wrong type, so fail.
     parseJSON _          = mzero

instance ToJSON Identifier where
  toJSON idf@Identifier{..} = object [ "experimentName" .= experimentName
                                     , "tag" .= tag
                                     , "uuid" .= uuid
                                     , "timestamp" .= (formatTime defaultTimeLocale "%Y%m%d%H%M%S" timestamp)
                                     , "debugMode" .= debugMode
                                     ]

data XPState = XPState { identifier :: Identifier
                       , loggingState :: LoggingState
                       , experimentConfig :: Value
                       } deriving (Show,Eq)

type XXP = StateT XPState IO

loggerLevel = (fromMaybe NOTICE) . getLevel


throwOnLeft _ (Right v) = return v
throwOnLeft f (Left e)  = throwIO (f e)

uniqLoc t u = 
  (formatTime defaultTimeLocale "%Y%m%d%H%M%S" t) ++ 
  fromMaybe "" (fmap UUID.toString u)

initialState :: IO XPState
initialState = do
  -- Get the experiment parameters
  time <- getCurrentTime
  name <- getProgName
  args <- getArgs
  gduuid <- nextUUID
  -- Get the logging state
  -- Console level via arguments
  let lvl = fromMaybe NOTICE (readMaybe $ args !! 0) 
  -- Read the logging configuration json
  logjson <- By.readFile "log.json"
  logstate <- throwOnLeft ErrorCall $ eitherDecode logjson
     
  let logstate' = logstate { consoleLogLevel = lvl
                           , dataLogFiles = []
                           , logLocation = uniqLoc time gduuid
                           }
          
  let idf = Identifier { experimentName = name
                       , tag = args !! 1
                       , uuid = fromMaybe "" (fmap UUID.toString gduuid)
                       , timestamp = time
                       , debugMode = fromMaybe False (readMaybe $ args !! 4)
                       }
            
  -- Create the experiment state
  return $ XPState { identifier = idf
                   , loggingState = logstate'
                   , experimentConfig = Null
                   }

ifJust :: Monad m => (a -> m ()) -> Maybe a -> m ()
ifJust f m = maybe (return ()) f m

setupDirectories :: XXP ()
setupDirectories = do
  state <- get
  liftIO $ do
    let ls = loggingState state
    let logDir =  "log" </> logLocation ls
    createDirectoryIfMissing True "log"
    createDirectory logDir
    ifJust (\d -> do 
               let dataDir = d </> logLocation ls
               createDirectoryIfMissing True d
               createDirectory dataDir
               writeFile (logDir </> "data.link") dataDir
           ) (dataLogLocation ls)
      
saveIdentifierAndConfig :: XXP ()
saveIdentifierAndConfig = do
  state <- get
  liftIO $ do
    let ls = loggingState state
    let logDir =  "log" </> logLocation ls
    By.writeFile (logDir </> "id.json") (encode $ identifier state)

loadConfiguration :: XXP ()
loadConfiguration = do
  liftIO $ do
    args <- getArgs
    putStrLn (show args)

-- Public Methods

spawn :: XXP ()
spawn = return ()

runXXP :: XXP () -> IO ()
runXXP xp = do 
  -- Initialize the state
  state <- initialState
  -- Run the experiment
  liftM fst $ runStateT (wrapExperiment xp) state

wrapExperiment :: XXP () -> XXP ()
wrapExperiment xp = do
  -- Create the required directories
  setupDirectories
  -- Load the configuration
  loadConfiguration
  -- Everything setup? Then save the configuration 
  saveIdentifierAndConfig
  xp
  -- Clean up
  -- Copy contents of run directory
  st <- get
  liftIO $ putStrLn $ logLocation $ loggingState st