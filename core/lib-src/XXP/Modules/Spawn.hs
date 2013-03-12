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

spawn :: String -> XXP ()
spawn binary = do
  st <- get
  liftIO $ do
    writeFile (logLocation (loggingState st) </> "debug")
      (show $ debugMode . identifier $ st)
    -- Start the server where data logs are coming in
    let socketName = uniqueID st ++ ".soc"
    socket <- listenOn $ UnixSocket
              ("run" </> socketName)
    
    -- TODO Run in working directory
    (Just hIn, Just hOut, Just hErr, hProc) <- createProcess
                                       (proc (".." </> "build" </> binary) []) 
      { std_out = CreatePipe
      , std_in = CreatePipe
      , std_err = CreatePipe
      , cwd = Just "run"
      }
    oid <- fork $ runProxy $ hGetLineS hOut >-> logD NOTICE st
    eid <- fork $ runProxy $ hGetLineS hErr >-> logD ERROR st
    BS.hPut hIn $ BSC.pack $ socketName ++ "\n"
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

    wait oid
    wait eid
    exitCode <- waitForProcess hProc
    writeFile (logLocation (loggingState st) </> "exit")
      (show exitCode)

