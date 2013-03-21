import XXP

main = runXXP $ do cmake "test1"
                   -- gitCommit
                   spawnWithMPI "test1" defaultMPIConfig { bridgeCommand = "../../build_mpi/mpibridge"
                                                         , instances = Just 10
                                                         }
