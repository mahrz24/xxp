{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}

module Xxp.Core 
       ( runXXP
       , loadConfiguration
       , gitCommitWithBranch
       , cmake
       , spawn
       ) where

import Prelude hiding (log)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Monad.Trans.State.Strict
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception.Lifted
import Control.Applicative

import Control.Concurrent
import Control.Proxy

import Data.Dynamic
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Time.Clock
import Data.Time.Format
import Data.UUID.V1
import qualified Data.UUID as UUID

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as T
import Data.Text (Text)

import Text.Read (readEither, readMaybe)

import System.Environment
import System.FilePath
import System.Directory
import System.Log.Logger
import System.Log.Formatter
import qualified System.Log.Handler as LH
import System.Log.Handler.Simple
import System.Locale
import System.Process
import System.IO
import System.Exit

import Control.Lens.Plated
import Data.Traversable (traverse)

import HSH

data LoggingState = LoggingState { externalDataLogLocation :: Maybe FilePath
                                 , fileLogLevel :: Priority
                                   -- Passed from xxp binary 
                                   -- concerns the runtime log level 
                                   -- for stdout
                                 , consoleLogLevel :: Priority
                                 , logLocation :: FilePath
                                 } deriving (Show, Eq)
                    
data DataState = DataState { dataLogLocation :: FilePath
                           , dataLogFiles :: [FilePath]
                           } deriving (Show, Eq)

instance FromJSON LoggingState where
     parseJSON (Object v) = do
       fileLogLevel <- either fail return =<< readEither <$> v .: "fileLogLevel"
       externalDataLogLocation <- v .:? "externalDataLogLocation"
       return $ LoggingState { consoleLogLevel = NOTICE
                             , logLocation = ""
                             , ..
                             }
     -- A non-Object value is of the wrong type, so fail.
     parseJSON _          = mzero
                                            
data Identifier = Identifier { experimentName :: String
                             , tag :: String
                             , uuid :: String
                             , timestamp ::  UTCTime
                               -- Used to test whether the current 
                               -- experiment binaries need to be recompiled
                             , debugMode :: Bool
                             } deriving (Show, Read, Eq)

instance FromJSON Identifier where
     parseJSON (Object v) = do
       experimentName <- v .: "experimentName"
       tag <- v .: "tag"
       uuid <- v .: "uuid"
       timestamp <- maybe (fail "Time parse error") return 
                    =<< (parseTime defaultTimeLocale "%Y%m%d%H%M%S") <$> 
                    v .: "timestamp"
       debugMode <- v .: "debugMode"
       return $ Identifier { .. }
     -- A non-Object value is of the wrong type, so fail.
     parseJSON _          = mzero

instance ToJSON Identifier where
  toJSON idf@Identifier{..} = 
    object [ "experimentName" .= experimentName
           , "tag" .= tag
           , "uuid" .= uuid
           , "timestamp" .= (formatTime defaultTimeLocale "%Y%m%d%H%M%S" 
                             timestamp)
           , "debugMode" .= debugMode
           ]
  
data XPState = XPState { identifier :: Identifier
                       , loggingState :: LoggingState
                       , dataState :: DataState
                       , experimentConfig :: Value
                       } deriving (Show,Eq)

idDesc :: XPState -> String
idDesc state@XPState{..} = experimentName identifier ++ (show $ timestamp identifier)

type XXP = StateT XPState IO

log :: Priority -> String -> XXP ()
log l m = do 
  st <- get 
  liftIO $ logM ("xxp." ++ (experimentName $ identifier st)) l m

loggerLevel = (fromMaybe NOTICE) . getLevel

throwOnLeft _ (Right v) = return v
throwOnLeft f (Left e)  = liftIO $ throwIO (f e)

decodeOrError f j = 
  throwOnLeft (\s -> ErrorCall $ "JSON parsing error in file: " ++ f) $ 
   eitherDecode j

uniqLoc t u = 
  "log" </>
    (formatTime defaultTimeLocale "%Y%m%d%H%M%S" t) ++ 
    fromMaybe "" (fmap UUID.toString u)

mergeValues (Object a) (Object b) = Object (HM.unionWith mergeValues a b)
mergeValues a b = a

initialState :: IO XPState
initialState = do
  -- Get the experiment parameters
  time <- getCurrentTime
  name <- getProgName
  args <- getArgs
  gduuid <- nextUUID
  -- Get the logging state
  
  -- Console level via arguments
  let consoleLogLevel = fromMaybe NOTICE (readMaybe $ args !! 0) 
  updateGlobalLogger rootLoggerName (setLevel consoleLogLevel)    
  
  -- Read the logging configuration json
  logJSON <- BS.readFile "log.json"
  loggingState' <- decodeOrError "log.json" logJSON
     
  let loggingState = loggingState' { consoleLogLevel = consoleLogLevel
                                   , logLocation = uniqLoc time gduuid
                                   }
  

  
  let dataState = DataState { dataLogFiles = []
                            , dataLogLocation = 
                                 fromMaybe 
                                   (logLocation loggingState </> "data") 
                                   (externalDataLogLocation loggingState)
                            }
          
  let idf = Identifier { experimentName = intercalate "_" $ 
                                            tail $ splitOn "_" name
                       , tag = args !! 1
                       , uuid = fromMaybe "" (fmap UUID.toString gduuid)
                       , timestamp = time
                       , debugMode = fromMaybe False (readMaybe $ args !! 4)
                       }
            
  -- Create the experiment state
  return $ XPState { identifier = idf
                   , experimentConfig = Null
                   , ..
                   }

ifJust :: Monad m => (a -> m ()) -> Maybe a -> m ()
ifJust f m = maybe (return ()) f m

setupDirectories :: XXP ()
setupDirectories = do
  st <- get
  liftIO $ do
    createDirectoryIfMissing True "log"
    let ls = loggingState st
    let ds = dataState st
    let logDir = logLocation ls
    createDirectory logDir
    createDirectoryIfMissing True (dataLogLocation ds)
    ifJust (writeFile (logDir </> "data.link")) (externalDataLogLocation ls)
      
saveIdentifierAndConfig :: XXP ()
saveIdentifierAndConfig = do
  st <- get
  liftIO $ do
    let ls = loggingState st
    let logDir = logLocation ls
    BS.writeFile (logDir </> "id.json") (encode $ identifier st)
    BS.writeFile (logDir </> "config.json") (encode $ experimentConfig st)

instance Plated Value where
  plate f (Object v) = Object <$> traverse f v
  plate f (Array v) = Array <$> traverse f v
  plate _ v = pure v

loadLinkedConfigs = loadLinkedConfigs' []

loadLinkedConfigs' :: [String] -> Value -> IO Value
loadLinkedConfigs' incs = transformM tryLoad
  where tryLoad (Object (HM.toList -> [("load", String file)])) = do
          let fileString = T.unpack file
          config <- BS.readFile $ "config" </> fileString
          value <- decodeOrError fileString config
          if (length incs < 128 && fileString `notElem` incs) then
            loadLinkedConfigs' (fileString:incs) value
            else throwIO $ ErrorCall 
                   "Inclusion cycle detected or inclusion depth exceeded."
        tryLoad o = return o
  
loadConfiguration :: XXP ()
loadConfiguration = do
  -- Quite an imperative command
  st <- get
  args <- liftIO getArgs
  let customConfig = args !! 2
      forceConfigFile = args !! 3
  -- A forced configuration overrides everything
  if forceConfigFile /= "" then
    (do forceConfig <- liftIO $ BS.readFile forceConfigFile
        forceConfigValue <- decodeOrError forceConfigFile forceConfig
        put $ st { experimentConfig = forceConfigValue }
    )
    else 
    (do config <- liftIO $ BS.readFile "config.json"
        configValue <- decodeOrError "config.json" config
        put $ st { experimentConfig = configValue }
        -- Is there an experiment local config
        let localConfigFile = "config_" 
                                ++ (experimentName . identifier) st 
                                ++ ".json"
        localExists <- liftIO $ doesFileExist localConfigFile
        when localExists $ do
          localConfig <- liftIO $ BS.readFile localConfigFile
          localConfigValue <- decodeOrError localConfigFile 
                                localConfig :: XXP Value
          put $ st { experimentConfig = 
                        mergeValues localConfigValue configValue }
        st <- get  
        linkedValue <- liftIO $ loadLinkedConfigs (experimentConfig st)
        put st { experimentConfig = linkedValue }
    )
    
setupLogging :: XXP ()
setupLogging = do
  st <- get  
  let ls = loggingState st
  liftIO $ updateGlobalLogger rootLoggerName (setLevel DEBUG)    
  consoleHandler <- liftIO $ streamHandler stderr (consoleLogLevel ls)
  fileHandler <- liftIO $ fileHandler ((logLocation ls) </> "log.txt") 
                   (fileLogLevel ls)
  let consoleFormatter = if (consoleLogLevel ls) == DEBUG then
                           (simpleLogFormatter "[$loggername/$prio] $msg")
                           else (simpleLogFormatter $ 
                                 (experimentName $ identifier st) ++ ": $msg")
      fileFormatter = if (fileLogLevel ls) == DEBUG then
                           (simpleLogFormatter 
                            "[$loggername/$prio@$utcTime] $msg")
                           else (simpleLogFormatter "$utcTime: $msg")
      consoleHandler' = LH.setFormatter consoleHandler consoleFormatter 
      fileHandler' = LH.setFormatter fileHandler fileFormatter
  liftIO $ updateGlobalLogger rootLoggerName 
      (setLevel DEBUG . setHandlers [consoleHandler', fileHandler'])
  return ()

fatalCatch :: String -> XXP a -> XXP a
fatalCatch s f = catch f (\e -> do log ERROR $ s ++ 
                                     (show (e :: SomeException))
                                   liftIO $ exitWith (ExitFailure 1))

wrapExperiment :: XXP () -> XXP ()
wrapExperiment xp = do
  st <- get
  
  fatalCatch "Error during setup: " 
    (do -- Create the required directories
        setupDirectories
        -- Setup logging
        setupLogging)
 
  log NOTICE "Preparing"
  log DEBUG "Loading configuration"
  -- Load the configuration
  fatalCatch "Error while loading configuration: " loadConfiguration
  log DEBUG "Saving configuration"
  -- Everything setup? Then save the configuration 
  fatalCatch "Error while saving configuration: " saveIdentifierAndConfig
  log NOTICE "Starting"
  fatalCatch "Error while running experiment: " xp
  log NOTICE "Cleanup"
  -- Clean up
  -- Copy contents of run directory
  log NOTICE "Done"
  
-- Public Functions

  
newtype Wait a = Wait (TVar (Maybe a))

fork :: IO a -> IO (Wait a)
fork m = do
  w <- atomically (newTVar Nothing)
  forkIO (m >>= atomically . writeTVar w . Just)
  return (Wait w)
 
wait :: Wait a -> IO a
wait (Wait w) = atomically $ do
  r <- readTVar w
  case r of     
    Just a -> return a
    Nothing -> retry
    
logD :: (Proxy p) => XPState -> () -> Consumer p String IO r
logD = logD' "binary"
    
logD' :: (Proxy p) => String -> XPState -> () -> Consumer p String IO r
logD' ln st () = runIdentityP $ forever $ do
  a <- request ()
  lift $ logM ("xxp." ++ (experimentName $ identifier st) ++ "." ++ ln) 
    NOTICE (ln ++ ": " ++ a)

customProc :: String -> String -> [String] -> XXP ExitCode
customProc dir p args = do
  st <- get
  liftIO $ do
    (_, Just hOut, Just hErr, hProc) <- createProcess (proc p args)
      { std_out = CreatePipe
      , std_err = CreatePipe
      , cwd = Just dir
      }
    oid <- fork $ runProxy $ (hGetLineS hOut) >-> (logD' p st)
    eid <- fork $ runProxy $ (hGetLineS hErr) >-> (logD' (p ++ ": error") st)
    wait oid
    wait eid
    waitForProcess hProc


cmake :: String -> XXP ()
cmake target = do
  -- Run cmake in build directory "i.e cmake ../src"
  exitCode <- customProc "build" "cmake" ["../src"]
  when (exitCode /= ExitSuccess) (liftIO $ 
                                    throwIO $ 
                                    ErrorCall $ "cmake " 
                                      ++ (show exitCode))
  -- Run make target in build directory
  exitCode <- customProc "build" "make" [target]
  when (exitCode /= ExitSuccess) (liftIO $ 
                                    throwIO $ 
                                    ErrorCall $ "make " 
                                      ++ (show exitCode))
  return ()

shellExec = liftIO . runIO
  
gitCommitWithBranch :: String -> XXP ()
gitCommitWithBranch branch = do
  currentBranch <- liftIO $ (runSL $ ("git rev-parse --abbrev-ref HEAD" :: String))
  liftIO $ runIO $ ("git checkout " ++ branch :: String)
  liftIO $ runIO $ ("git rebase master" :: String)

spawn :: String -> XXP ()
spawn binary = do
  st <- get
  liftIO $ do 
    -- TODO Run working directory
    (Just hIn, Just hOut, _, hProc) <- createProcess (proc binary []) 
      { std_out = CreatePipe
      , std_in = CreatePipe 
      }
    oid <- fork $ runProxy $ (hGetLineS hOut) >-> (logD st)
    BS.hPut hIn (encode $ experimentConfig st)
    wait oid

runXXP :: XXP () -> IO ()
runXXP xp = do 
  -- Initialize the state
  state <- catch initialState 
           (\e -> do errorM "xxp.core" $ 
                       "Initialization error: " 
                       ++ (show $ (e :: SomeException))
                     exitWith (ExitFailure 1))
  -- Run the experiment
  catch (liftM fst $ runStateT (wrapExperiment xp) state)
    (\e -> do errorM "xxp.core" $ "Exiting with error: "
                ++ (show $ (e :: SomeException))
              exitWith (ExitFailure 1))
 