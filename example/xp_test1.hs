import Xxp.Core

{-

Short TODO list of what I want to implement for a initial version

* Minimal data logging facilities via domain sockets
* List & less for data logs

-}

main = runXXP $ do cmake "test1"
                   -- gitCommit
                   spawn "test1"