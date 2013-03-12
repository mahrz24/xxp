module XXP.Modules.VCS (gitCommit) where

import XXP.Modules.Shell
import XXP.State
import XXP.Experiment

import System.FilePath

gitCommit :: XXP ()
gitCommit = do
  st <- get
  -- Any changes are stored in a commit
  shellExec ("git commit -am \"[xxp:auto commit] "
             ++ idDesc st
             ++ "\" --allow-empty")
  currentCommit <- shellResult "git rev-parse HEAD"
  -- And the current state is logged
  liftIO $ writeFile (logLocation (loggingState st) </> "rev") currentCommit

