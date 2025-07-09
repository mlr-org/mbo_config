options("install.opts" = "--without-keep.source")
options("renv.config.pak.enabled" = TRUE)

renv::init(".", bare = TRUE)
renv::load(".")
renv::settings$snapshot.type("all")
renv::settings$r.version("4.4.0")

renv::install(c("pak", "reticulate", "slds-lmu/yahpo_gym/yahpo_gym_r"))

library(reticulate)

conda_create("yahpo_gym", python = "3.8", packages = c(
  "onnxruntime",
  "pip",
  "pyyaml",
  "pandas",
  "configspace"
), channel = "conda-forge")

conda_install(envname = "yahpo_gym", pip= TRUE, packages="'git+https://github.com/slds-lmu/yahpo_gym#egg=yahpo_gym&subdirectory=yahpo_gym'")

use_condaenv("yahpo_gym", required = TRUE)

import("yahpo_gym")
library("yahpogym")

# clone yahpo_data
system("git clone https://github.com/slds-lmu/yahpo_data ~/yahpo_data")
system("git -C ~/yahpo_data checkout pure_numeric")

init_local_config(data_path = "~/yahpo_data")

# check yahpo_gym
init_local_config(data_path = "~/yahpo_data")
b = BenchmarkSet$new("iaml_glmnet")
obj = b$get_objective("40981", multifidelity = FALSE)

# R packages for mlr3mbo
renv::install(c(
  "batchtools",
  "here",
  "mlr-org/mlr3@predict_newdata_fast",
  "mlr-org/mlr3learners@ranger_se",
  "mlr-org/mlr3mbo@so_config_5",
  "mlr3pipelines",
  "mlr-org/bbotk@so_config",
  "ranger",
  "DiceKriging",
  "rgenoud",
  "ranger",
  "nloptr",
  "cmaes",
  "fastGHQuad",
  "lhs"
))

# SMAC
conda_create("smac", python = "3.8", packages = c(
  "smac"
), channel = "conda-forge")

# HEBO
conda_create("hebo", python = "3.8")
conda_install(envname = "hebo", pip = TRUE, packages = c("HEBO", "ConfigSpace"))

# AX
conda_create("ax", python = "3.8", packages = c(
  "ax-platform",
  "mkl",
  "mkl-service",
  "intel-openmp",
  "ConfigSpace"
), channel = "conda-forge")