import Xxp.Core

{-

Short TODO list of what I want to implement for a initial version

* C++ header only adaptor with json parser
* Git auto commit and write hash out
* Minimal data logging facilities via domain sockets

-}

main = runXXP $ do cmake "test1"
                   gitCommitWithBranch "experiments"
                   spawn "test1"