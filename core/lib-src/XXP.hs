module XXP ( module XXP.Core
           , module XXP.Experiment
           , module XXP.State
           , module XXP.Util
           , module XXP.Logging
           , module XXP.Modules.Shell
           , module XXP.Modules.VCS
           , module XXP.Modules.Build
           , module XXP.Modules.Spawn
           ) where

{-
TODO LIST

* IPC Should catch parse error and directly return ERR
* The IPC should use ByteStrings
* Create a XXP.Prelude to hide log and offer liftedIO versions
* Transform everything to uses lenses for the records
* CMake file to simply include
* Change string& to string&& and make code shorter

-}

import XXP.Core
import XXP.Experiment
import XXP.State
import XXP.Util
import XXP.Logging

import XXP.Modules.Shell
import XXP.Modules.VCS
import XXP.Modules.Build
import XXP.Modules.Spawn
