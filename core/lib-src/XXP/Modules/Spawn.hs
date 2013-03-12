module XXP.Modules.Spawn (spawn) where

import Control.Proxy

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BSC

import System.FilePath
import System.Process
import System.IO

import Network

import XXP.Experiment
import XXP.State
import XXP.Logging
import XXP.Process

-- sending
send h = hPutStrLn h "Test from experiment runner"
 
-- receiving
receive h = do
	putStr "Receiving: "
	input <- hGetLine h
	putStrLn input
	return $ null input

serverHandler socket sn st hIn _ _ = do
    BS.hPut hIn $ BSC.pack $ sn ++ "\n"
    BS.hPut hIn $ (encode $ experimentConfig st)
    hClose hIn

    (h,host,port) <- accept socket
    putStrLn $ "Received connection from " ++ host ++ ":" ++ show port
    hSetBuffering h LineBuffering
    putStrLn $ "Starting server loop"
    receive h
    send h
    hClose h
    sClose socket

spawn :: String -> XXP ()
spawn binary = do
  st <- get
  writeLogFile "debug" (show $ debugMode . identifier $ st)
  -- Start the server where data logs are coming in
  let socketName = uniqueID st ++ ".soc"
  socket <- liftIO $ listenOn $ UnixSocket ("run" </> socketName)
  exitCode <- customProc' "run"
                         (".." </> "build" </> binary)
                         "binary"
                         []
                         (serverHandler socket socketName st)
  writeLogFile "exit" (show exitCode)

