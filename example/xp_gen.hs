import XXP

main = runXXP $ do cmake "gen"
                   -- gitCommit
                   spawn "gen"
