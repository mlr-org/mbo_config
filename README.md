# mlr3mbo - Configuration

Code to configure and benchmark [mlr3mbo](https://github.com/mlr-org/mlr3mbo) using problems from [YAHPO Gym](https://github.com/slds-lmu/yahpo_gym).

## Setup

* Run `install.sh` to setup the environment on the ncar derecho cluster
* Run `install.R` to install the R packages and python packages

## Coordinate Descent

Files for running the coordinate descent are in the `coordinate_descent` folder.

* `coordinate_descent_numeric.R` run coordinate descent on the pure numeric subset of YAHPO Gym SO v1
* `coordinate_descent_mixed.R` run coordinate descent on the YAHPO Gym SO v1
* `OptimizerCoordinateDescent.R` bbotk optimizer for coordinate descent

The results of the coordinate descent are stored in `coordinate_descent/results`.

## Benchmarking

Files for benchmarking mlr3mbo against SMAC3, Ax, HEBO and Optuna are in the `competitors` folder.

* `run_numeric_mlr3mbo.R` run mlr3mbo on the numeric benchmark instances
* `run_numeric_competitors.R` run SMAC3, Ax, HEBO and Optuna on the numeric benchmark instances
* `run_mixed_mlr3mbo.R` run mlr3mbo on the mixed benchmark instances
* `run_mixed_competitors.R` run SMAC3, Ax, HEBO and Optuna on the mixed benchmark instances
* `wrapper_*.py` python wrappers for SMAC3, Ax, HEBO and Optuna

The results of the benchmarking are stored in `competitors/results`.

Common files for the coordinate descent and benchmarking are in the `common` folder.
