import Xxp.Core

{-

Short TODO list of what I want to implement for a initial version

* C++ header only adaptor with json parser (picojson)
* Minimal data logging facilities via domain sockets
* Log success/no success
* List & less for data logs

-}

main = runXXP $ do cmake "test1"
                   -- gitCommit
                   spawn "test1"