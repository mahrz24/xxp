module XXP.State ( XXP
                   -- Some convenience exports for stuff you might want to do
                   -- in the XXP monad
                 , get
                 , put
                 , liftIO
                 , throwIO
                 ) where

import XXP.Experiment
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Exception.Lifted

type XXP = StateT XPState IO
