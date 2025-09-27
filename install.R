options("install.opts" = "--without-keep.source")
options("renv.config.pak.enabled" = TRUE)
options(repos=c(CRAN="https://cran.r-project.org"))
install.packages("renv")

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

# check yahpo_gym
init_local_config(data_path = "~/yahpo_data")
b = BenchmarkSet$new("iaml_glmnet")
obj = b$get_objective("40981", multifidelity = FALSE)

# R packages for mlr3mbo
renv::install(c(
  "batchtools",
  "here",
  "mlr3",
  "mlr-org/mlr3learners",
  "mlr-org/mlr3mbo@so_config_6",
  "mlr3pipelines",
  "mlr-org/bbotk@benchmark",
  "ranger",
  "DiceKriging",
  "rgenoud",
  "ranger",
  "nloptr",
  "cmaes",
  "fastGHQuad",
  "lhs"
))

system("git clone --recursive https://github.com/mlr-org/libcmaesr.git /tmp/libcmaesr")
renv::load(".")
renv::install("languageserver") # Why?
renv::install("/tmp/libcmaesr")

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
  "ConfigSpace"
), channel = "conda-forge")


  # "mkl",
  # "mkl-service",
  # "intel-openmp",
