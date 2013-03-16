module XXP.Modules.Shell ( shellExec
                         , shellResult
                         , shellLines
                         ) where

import XXP.State

import HSH

shellExec :: String -> XXP ()
shellExec = liftIO . runIO

shellResult :: String -> XXP String
shellResult = liftIO . runSL

shellLines :: String -> XXP [String]
shellLines = liftIO . run
