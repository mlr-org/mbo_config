library(reticulate)

# YAHPO Gym

remotes::install_github("slds-lmu/yahpo_gym/yahpo_gym_r")

py_require(packages = c(
  "yahpo_gym", 
  "onnxruntime", 
  "pip", 
  "pyyaml", 
  "pandas",
  "configspace"), python_version = "3.8")

import("yahpo_gym")

library("yahpogym")

# clone yahpo_data
system("git clone https://github.com/slds-lmu/yahpo_data ~/yahpo_data")
system("git checkout pure_numeric")

init_local_config(data_path = "~/yahpo_data")

b = BenchmarkSet$new("iaml_glmnet")
obj = b$get_objective("40981", multifidelity = FALSE)

# SMAC
py_require(packages = c(
  "smac",
  "yahpo_gym", 
  "onnxruntime", 
  "pip", 
  "pyyaml", 
  "pandas",
  "configspace"), python_version = "3.8")
import("smac")

# HEBO
py_require(packages = c(
  "hebo",
  "yahpo_gym", 
  "onnxruntime", 
  "pip", 
  "pyyaml", 
  "pandas",
  "configspace"), python_version = "3.8")
import("hebo")

# AX
py_require(packages = c(
  "ax-platform",
  "yahpo_gym", 
  "onnxruntime", 
  "pip", 
  "pyyaml", 
  "pandas",
  "configspace"), python_version = "3.8")

import("ax-platform")





