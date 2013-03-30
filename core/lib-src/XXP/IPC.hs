{-# LANGUAGE OverloadedStrings #-}
module XXP.IPC ( withIPC
               , serverHandler
               , Command(..)
               , Response(..)
               , IPC(..)
               , CommandHandler
               ) where

import Prelude hiding (log)

import Control.Proxy
import Control.Monad
import Control.Monad.IO.Class

import Data.List
import Data.Aeson
import qualified Data.ByteString.Char8 as BSS
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSC
import System.IO

import XXP.State
import XXP.Experiment
import XXP.Logging

import System.FilePath
import System.ZMQ3

data Foo = Bar
         | BLUB deriving (Eq, Show)


data IPC = IPC { ipcSocket :: Socket Rep
               , socketName :: String
               }

liftPIO :: (MonadIO m, MonadTrans t) => IO a -> t m a
liftPIO = lift . liftIO

withIPC :: (IPC -> XXP a) -> XXP a
withIPC f = do
  st <- get
  let socketName = uniqueID st ++ ".soc"
  ctx <- liftIO $ context
  responder <- liftIO $ socket ctx Rep
  liftIO $ bind responder ("ipc://run/" ++ socketName)
  r <- f $ IPC responder socketName
  liftIO $ do close responder
              destroy ctx
  return r

data Command = DAT String
             | RQF String
             | PIP String String String
             | EOF String
             | BLK String
             | UBL String
             | RQD String String
             | DNE
             | RQJ
             | NOP deriving (Read, Show, Eq)

data Response = ACK | STR String deriving (Read, Show, Eq)

type CommandHandler = Command -> XXP Response

commandHandlerS :: (Proxy p)
                   => CommandHandler
                   -> Command
                   -> Server p Command Response XXP r
commandHandlerS cmdH = runIdentityK $ foreverK $ \cmd -> do
  result <- lift $ cmdH cmd
  respond result

zmqResponderC :: (Proxy p)
               => Socket Rep
               -> ()
               -> Client p Command Response XXP ()
zmqResponderC socket () = runIdentityP go where
  go = do
    msg <- zmqReceive
    case head msg of
      "DNE" -> zmqReply ACK
      "DAT" -> forward1 DAT msg
      "RQF" -> forward1 RQF msg
      "RQJ" -> forward0 RQJ
      "PIP" -> forward3 PIP msg
      "EOF" -> forward1 EOF msg
      "BLK" -> forward1 BLK msg
      "UBL" -> forward1 UBL msg
      "RQD" -> forward2 RQD msg
      "NOP" -> forward0 NOP
  forward0 f = (do response <- request f
                   zmqReply response
                   go)
  forward1 f m = (do response <- request $ f (m !! 1)
                     zmqReply response
                     go)
  forward2 f m = (do response <- request $ f (m !! 1) (m !! 2)
                     zmqReply response
                     go)
  forward3 f m = (do response <- request $ f (m !! 1) (m !! 2) (m !! 3)
                     zmqReply response
                     go)
  zmqReceive = liftPIO $ liftM (map BSS.unpack) $ receiveMulti socket
  zmqReply ACK = liftPIO $ send socket [] ""
  zmqReply (STR s) = liftPIO $ send socket [] (BSS.pack s)

serverHandler (IPC socket sn) cmdHandler = do
  log NOTICE $ "Server up and running"
  runProxy $ commandHandlerS cmdHandler >-> zmqResponderC socket
