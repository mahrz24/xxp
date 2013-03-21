module XXP ( module XXP.Core
           , module XXP.Experiment
           , module XXP.State
           , module XXP.Util
           , module XXP.Logging
           , module XXP.Modules.Shell
           , module XXP.Modules.VCS
           , module XXP.Modules.Build
           , module XXP.Modules.Spawn
           , module XXP.Modules.HPC
           ) where

{-
TODO LIST

* IPC Should catch parse error and directly return ERR
* The IPC and everything else actually should use ByteStrings
* Create a XXP.Prelude to hide log and offer liftedIO versions
* Transform everything to use lenses for the records
* Comments everywhere
* Examples
* Check for pipes resource handling, possibly not a problem because of the short
  runtime and the few files open, but better safe than sorry.
* Proper error handling in the whole postprocessing part (IO () change to IO Either ..)
* Proper error handling for pipes
* Better abstraction for data pipes
* Verbose commands
* Fetch exit status in hpc 
* Directory module that does all the conversions needed
* Better data api
* MPI Bridge less string copying

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
import XXP.Modules.HPC
