renv::init(bare = TRUE)
renv::settings$snapshot.type("all")
renv::settings$r.version("4.4.0")
renv::install("pak")

pak::pak(c(
  "mlr-org/mlr3mbo@so_config_2", 
  "batchtools", 
  "be-marc/yahpo_gym/yahpo_gym_r", 
  "lhs", 
  "ranger", 
  "R.utils", 
  "mlr3learners",
  "mlr3pipelines",
  "mlr-org/bbotk"))

renv::snapshot()

# yahpo setup

reticulate::conda_create(
  envname = "yahpo_gym",
  packages = c("onnxruntime", "pip", "pyyaml", "pandas"),
  channel = "conda-forge",
  python_version = "3.8"
)
reticulate::conda_install(envname = "yahpo_gym", packages = "configspace", channel = "conda-forge")
reticulate::conda_install(envname = "yahpo_gym", pip = TRUE, packages = "'git+https://github.com/slds-lmu/yahpo_gym#egg=yahpo_gym&subdirectory=yahpo_gym'")

reticulate::use_condaenv("yahpo_gym", required=TRUE)
library("yahpogym")
init_local_config(data_path = "yahpo_data")

# reticulate::use_condaenv("yahpo_gym", required=TRUE)
# library("yahpogym")
# b = BenchmarkSet$new("iaml_glmnet")
# obj = b$get_objective("40981", multifidelity = FALSE)