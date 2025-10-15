options("install.opts" = "--without-keep.source")
options("renv.config.pak.enabled" = TRUE)
options(repos=c(CRAN="https://cran.r-project.org"))
install.packages("renv")
conda_dir = "/glade/work/marcbecker/conda-envs"

renv::init(".", bare = TRUE)
renv::load(".")
renv::settings$snapshot.type("all")
renv::settings$r.version("4.4.2")

renv::install(c("pak", "reticulate", "slds-lmu/yahpo_gym/yahpo_gym_r"))

library(reticulate)

conda_create(sprintf("%s/yahpo_gym", conda_dir), python = "3.8", packages = c(
  "onnxruntime",
  "pip",
  "pyyaml",
  "pandas",
  "configspace"
), channel = "conda-forge")

conda_install(envname = sprintf("%s/yahpo_gym", conda_dir), pip= TRUE, packages="'git+https://github.com/slds-lmu/yahpo_gym#egg=yahpo_gym&subdirectory=yahpo_gym'")

use_condaenv(sprintf("%s/yahpo_gym", conda_dir), required = TRUE)

import("yahpo_gym")
library("yahpogym")

# clone yahpo_data
system("git clone https://github.com/slds-lmu/yahpo_data ~/yahpo_data")
system("git -C ~/yahpo_data checkout pure_numeric")

# check yahpo_gym
init_local_config(data_path = "~/yahpo_data")
b = BenchmarkSet$new("iaml_glmnet")
obj = b$get_objective("40981", multifidelity = FALSE)

#conda_install(envname = sprintf("%s/yahpo_gym", conda_dir), packages = c("botorch", "gpytorch"))

# R packages for mlr3mbo
renv::install(c(
  "batchtools",
  "here",
  "mlr3",
  "mlr3learners",
  "mlr-org/mlr3mbo@so_config_6",
  "mlr3pipelines",
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
renv::install("languageserver")
renv::install("/tmp/libcmaesr")

# SMAC
conda_create(sprintf("%s/smac", conda_dir), python = "3.10")
conda_install(envname = sprintf("%s/smac", conda_dir), pip = TRUE, packages = c("smac"))

# HEBO
conda_create(sprintf("%s/hebo", conda_dir), python = "3.10")
conda_install(envname = sprintf("%s/hebo", conda_dir), pip = TRUE, packages = c("HEBO", "ConfigSpace"))

# AX
conda_create(sprintf("%s/ax", conda_dir), python = "3.8")
conda_install(envname = sprintf("%s/ax", conda_dir), pip = TRUE, packages = c("ax-platform", "ConfigSpace"))
