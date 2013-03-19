{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}

module XXP.Setup ( initialState
                 , setupDirectories
                 , setupLogging
                 , saveIdentifierAndConfig
                 , loadConfiguration
                 ) where



import Control.Applicative
import Control.Monad
import Control.Lens.Plated

import Control.Exception.Lifted

import Data.Maybe
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Char8 as BSSC
import Data.ByteString.Lazy (ByteString)
import Data.Traversable (traverse)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Text (Text)
import Data.UUID.V1
import qualified Data.UUID as UUID
import Data.List.Split
import Data.List
import qualified Data.ByteString.Base64.URL as B64

import Text.Read (readEither, readMaybe)

import System.Log
import System.Log.Logger
import System.Log.Formatter
import qualified System.Log.Handler as LH
import System.Log.Handler.Simple

import System.IO(stderr)
import System.Environment
import System.FilePath
import System.Directory

import XXP.State
import XXP.Logging
import XXP.Experiment
import XXP.Util

b64uuid = map (\x -> case x of
                  '=' -> '_'
                  _ -> x) . BSSC.unpack . B64.encode . BSS.pack  . BS.unpack .
            maybe "<NO-UUID>" UUID.toByteString 

initialState :: IO XPState
initialState = do
  -- Get the experiment parameters
  time <- getCurrentTime
  name <- getProgName
  args <- getArgs
  gduuid <- nextUUID
  -- Get the logging state

  -- Console level via arguments
  let consoleLogLevel = fromMaybe NOTICE (readMaybe $ head args)
  updateGlobalLogger rootLoggerName (setLevel consoleLogLevel)

  -- Read the logging configuration json
  logJSON <- BS.readFile "log.json"
  loggingState' <- decodeOrError "log.json" logJSON

  let uuidString = b64uuid gduuid

  let loggingState = loggingState' { consoleLogLevel = consoleLogLevel
                                   , logLocation = uniqueLoc time uuidString
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
                       , uuid = uuidString
                       , timestamp = time
                       , debugMode = fromMaybe False (readMaybe $ args !! 4)
                       , gdb = fromMaybe False (readMaybe $ args !! 5)
                       }

  -- Create the experiment state
  return XPState { identifier = idf
                 , experimentConfig = Null
                 , ..
                 }

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

setupLogging :: XXP ()
setupLogging = do
  st <- get
  let ls = loggingState st
  liftIO $ updateGlobalLogger rootLoggerName (setLevel DEBUG)
  consoleHandler <- liftIO $ streamHandler stderr (consoleLogLevel ls)
  fileHandler <- liftIO $ fileHandler (logLocation ls </> "log.txt")
                   (fileLogLevel ls)
  let consoleFormatter = simpleLogFormatter
                         (if consoleLogLevel ls == DEBUG then
                            "[$loggername/$prio] $msg"
                          else experimentName (identifier st) ++ ": $msg")
      fileFormatter = simpleLogFormatter
                      (if fileLogLevel ls == DEBUG then
                         "[$loggername/$prio@$utcTime] $msg"
                       else "$utcTime: $msg")
      consoleHandler' = LH.setFormatter consoleHandler consoleFormatter
      fileHandler' = LH.setFormatter fileHandler fileFormatter
  liftIO $ updateGlobalLogger rootLoggerName
      (setLevel DEBUG . setHandlers [consoleHandler', fileHandler'])  
  return ()

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
          if length incs < 128 && fileString `notElem` incs then
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

