module XXP.Modules.Shell ( shellExec
                         , shellResult
                         ) where

import XXP.State

import HSH

shellExec :: String -> XXP ()
shellExec = liftIO . runIO

shellResult :: String -> XXP String
shellResult = liftIO . runSL
