{-# LANGUAGE RecordWildCards #-}
module XXP.Modules.HPC ( hpcSpawn
                         HPCConfig(..)
                       ) where

data HPCConfig = HPCConfig { submissionCommand :: String
                           }

hpcSpawn :: XXP ()
hpcSpawn = undefined
