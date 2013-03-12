{-# LANGUAGE OverloadedStrings, RecordWildCards, ViewPatterns #-}

module XXP.Experiment ( LoggingState(..)
                      , DataState(..)
                      , Identifier(..)
                      , XPState(..)
                      , mergeValues
                      , idDesc
                      , uniqueID
                      , uniqueRunID
                      , uniqueLoc
                      ) where

import Control.Applicative
import Control.Monad

import qualified Data.HashMap.Strict as HM
import Data.Either
import Data.Aeson
import Data.Time.Clock
import Data.Time.Format
import Text.Read

import System.Locale
import System.FilePath
import System.Log

data LoggingState = LoggingState { externalDataLogLocation :: Maybe FilePath
                                 , fileLogLevel :: Priority
                                   -- Passed from xxp binary
                                   -- concerns the runtime log level
                                   -- for stdout
                                 , consoleLogLevel :: Priority
                                 , logLocation :: FilePath
                                 } deriving (Show, Eq)

instance FromJSON LoggingState where
     parseJSON (Object v) = do
       fileLogLevel <- either fail return =<< readEither <$> v .: "fileLogLevel"
       externalDataLogLocation <- v .:? "externalDataLogLocation"
       return  LoggingState { consoleLogLevel = NOTICE
                            , logLocation = ""
                            , ..
                            }
     -- A non-Object value is of the wrong type, so fail.
     parseJSON _          = mzero


data DataState = DataState { dataLogLocation :: FilePath
                           , dataLogFiles :: [FilePath]
                           } deriving (Show, Eq)

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
                    =<< parseTime defaultTimeLocale "%Y%m%d%H%M%S" <$>
                    v .: "timestamp"
       debugMode <- v .: "debugMode"
       return Identifier { .. }
     -- A non-Object value is of the wrong type, so fail.
     parseJSON _          = mzero

instance ToJSON Identifier where
  toJSON idf@Identifier{..} =
    object [ "experimentName" .= experimentName
           , "tag" .= tag
           , "uuid" .= uuid
           , "timestamp" .= formatTime defaultTimeLocale "%Y%m%d%H%M%S"
                              timestamp
           , "debugMode" .= debugMode
           ]

data XPState = XPState { identifier :: Identifier
                       , loggingState :: LoggingState
                       , dataState :: DataState
                       , experimentConfig :: Value
                       } deriving (Show,Eq)

mergeValues (Object a) (Object b) = Object (HM.unionWith mergeValues a b)
mergeValues a b = a

idDesc :: XPState -> String
idDesc state@XPState{..} = experimentName identifier
                             ++ " @ "
                             ++ show (timestamp identifier)

uniqueID :: XPState -> String
uniqueID state@XPState{..} = experimentName identifier
                             ++ uniqueRunID
                              (timestamp identifier)
                              (uuid identifier)

uniqueRunID t u = formatTime defaultTimeLocale "%Y%m%d%H%M%S" t ++ u

uniqueLoc t u = "log" </> (uniqueRunID t u)
