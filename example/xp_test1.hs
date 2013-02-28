import Xxp.Core

{-

Short TODO list of what I want to implement for a initial version

* C++ header only adaptor with json parser
* Git auto commit and write hash out
* Debug flag for cmake
* Minimal data logging facilities via domain sockets

-}

main = runXXP $ do cmake "test1"
                   gitCommit
                   spawn "test1"