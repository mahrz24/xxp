import XXP

main = runXXP $ do cmake "test1"
                   -- gitCommit
                   spawn "test1"
