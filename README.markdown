# Xxp

Haskell based framework for running simulations and numerical experiments in a modular fashion and collects logged data. It can be used to facilitate several tasks like re-compiling if a file change has been detected, automatic vc commit before running the simulation and storing log data in a database.

## Experiment Setup

An experiment setup has the following structure

```
/experiment_root     # The root directory 
  /src               # The sources for the binaries
  /run               # Runtime directory for the simulations
  /log               # Logs of simulation runs
  /db                # Optional (needed in case a db like sqlite is used)
  /config            # Optional (if different parameter sets are needed)
  exp1.hs            # An experiment description file
  exp2.hs            # Another experiment description file
  config.json        # Configuration for all experiments
  config_exp1.json   # Configuration specific to experiment 1
```

An experiment is started using the xxp binary which takes the experiment name as command line parameter:

```
xxp run exp1
```

An experiment run will be identified by the triple (time, revision/githash, uuid) and it is possible to supply a tag for convenience:

```
xxp run exp1 -t test_run
```

It is also possible to supply a custom config via command line:

```
xxp run exp1 -c '{"samples":20}'
```

A sample experiment description file looks as follows:

```haskell
import XXP.Core

simulation1 = git commit $ withCMake "simulation1" $ binary "simulation1"

main = runXXP $ spawn simulation1 <:> loadConfiguration

```

This example compiles the target `simulation1` using cmake in a build directory within `<experiment_root>/run`, if the compilation is successfull the source is commited and the hash or revision number
