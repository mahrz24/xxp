# Xxp

Haskell based framework for running simulations and numerical experiments in a modular fashion and collects logged data. It can be used to facilitate several tasks like re-compiling if a file change has been detected, automatic vc commit before running the simulation and storing log data in a database.

## Experiment Setup

An experiment setup has the following structure

```
/experiment_root     # The root directory 
  /src               # The sources for the binaries
  /run               # Runtime directory for the simulations
  /build	     # Build directory for the experiment description as well as the run
  /log               # Logs of simulation runs
  /db                # Optional (needed in case a db like sqlite is used)
  /config            # Optional (if different parameter sets are needed)
  xp_test1.hs        # An experiment description file
  xp_2.hs            # Another experiment description file
  config.json        # Configuration for all experiments
  config_test1.json  # Configuration specific to experiment 1
```

An experiment is started using the xxp binary which takes the experiment name as command line parameter:

```
xxp run test1
```

An experiment run will be identified by the triple (time, revision/githash, uuid) and it is possible to supply a tag for convenience:

```
xxp run test1 -t test_run
```

It is also possible to supply a custom config via command line:

```
xxp run test1 -c '{"samples":20}'
```

A sample experiment description file looks as follows:

```haskell
import XXP.Core

simulation1 = git commit ||| cmake "simulation1" ||| binary "simulation1"

main = runXXP $ loadConfiguration ||| spawn simulation1

```

This example compiles the target `simulation1` using cmake in a build directory within `<experiment_root>/run`, if the compilation is successfull the source is commited and the hash or revision number can be used for logging. The experiment itself is constructed as a `Xperiment` monad which is then executed using `runXXP`. The `spawn` function takes a binary description and creates a `Xperiment` which simply spawns the binary using the present configuration. Initially the configuration is empty, the loadConfiguration loads the `config.json` file and merges it (recursively) with the `config_<experiment_filename>.json` configuration (the latter overwriting the former). It is possible to merge the config with an additional command line argument by using the `-c` option as shown above. After this all objects in the configuration with a single `load` key, as for example `{ "load" : "sample_params.json" }` are replaced by loading the corresponding JSON file from the `config` directory.

Another option is the `-f` force option which will replace the whole config by a specific JSON file (without loading external configurations). 

Without specific logging options all output of the binary will be routed to stdout and there will be a minimal log entry containing the parameters, time, hash/revision and runtime of the experiment. 

## Data Parameters

In some case the parameters of a simulation cannot be expressed well in JSON format or a simply to large to be passed as configuration parameters. For these cases there is the possibility to use file objects, like  `{"file" : "input.dat"}`. These are copied into the runtime directory and the name is automatically replaced with the absolute filename. If the object also contains the key `extern` set to true, the file is not copied. This should only be used if the file is to large to be copied for every experiment run and it does not change after the experiment is run to allow reproducibility. 

## Data Logging

Configuration and data parameters are automatically logged as explained above, but data that is generated during the run of a simulation can be logged using a predefined logging protocol. A log consists of samples, where each sample has an index that consists of arbitrary many values (integers, floating points, bools, labels), a type descriptor (integer, floating point) and the value it holds.

## Other xxp Commands

Besides running simulations xxp can also be used to manage your simulation runs, archiving data and formatting data for further analysis.

```
xxp clean
```
Deletes all unneeded files in the runtime directory.

```
xxp rml [exp_name]
```
Deletes all logs & data of all experiments (or a specific experiment if the name is given) in the current experiment root directory (also if stored in a database).

```
xxp list [exp_name]
```
List all runs of all or a specific experiments.

```
xxp plist [exp_name]
```
List all configuration parameters of all or a specific experiment.

```
xxp rerun <timestamp, tag, uuid>
```
Reruns an experiment that is uniquely defined, allows for parameter overriding, source is checked out to older revision if the experiment uses version control.

```
xxp reuse <timestamp, tag, uuid>
```
Reruns an experiment, but with current source code, reusing old parameters

### Archiving Data

```
xxp archive <target>
```

```
xxp unarchive <source>
```

### Data Analysis

```
xxp query <query>
```
