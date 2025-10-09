# mlr3mbo - Configuration

Code to perform AC of [mlr3mbo](https://github.com/mlr-org/mlr3mbo) using problems from [YAHPO Gym](https://github.com/slds-lmu/yahpo_gym).

## Setup

* Run `install.sh` to setup the environment on the ncar derecho cluster
* Run `install.R` to install the R packages and python packages

## Structure

### Files for AC:

* `coordinate_descent.R` main workhorse file for coordinate descent. `YAHPO_BENCHMARK` can be set to `"pure_numeric"`, `"mixed"` or `""` to use the new pure numeric subset of YAHPO GYM SO v1, the new mixed subset (TBD) of it or the original YAHPO-SO v1
* `OptimizerCoordinateDescent.R` implementation of the coordinate descent optimizer used in `coordinate_descent.R`
* `run_coordinate_descent.pbs` submit file to run coordinate descent as configured in `coordinate_descent.R`. Currently uses one node with 128 CPUs and 235 GB of RAM on the main queue of ncar derecho for the maximum walltime of 12 hours
* `batchtools.conf.coordinate_descent.R` batchtools config for `run_coordinate_descent.pbs`
* `helper.R` code to construct optimization instances and search spaces
* `report.qmd` create a quarto HTML report (`quarto render report.qmd --to html`) of the AC results
* `yahpo_pure_numeric_rs_reference.rds`, `yahpo_mixed_rs_reference.rds`, `yahpo_rs_reference.rds` results from the random search runs

### Files for benchmarking:

* `run_yahpo_competitors.R` run competitors on YAHPO Gym benchmarks
* `run_yahpo_mlr3mbo.R` run mlr3mbo on YAHPO Gym benchmarks
* `run_yahpo_rs.R` run a large scale random search on YAHPO Gym benchmarks
* `batchtools.conf.R` batchtools config for `run_yahpo_*`
* `pbs_derecho.tmpl` batchtools template file. Currently submits to develop queue of ncar derecho which allows for getting single cores and is cheaper
* `helper.R` code to construct optimization instances and search spaces
* `*_venv/` python venv for competitor, `*_wrapper.py` python wrapper code for competitor
* `subprocess_yahpo.py` python code to evaluate YAHPO Gym via a subprocess
* `config.py`, `configspace_utils.py` python config and helper files
* `analyze.R` code to analyze benchmarking results and create figures etc.

