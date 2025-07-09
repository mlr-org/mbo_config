library(reticulate)
library(bbotk)

py_require(packages = c(
  "yahpo_gym", 
  "onnxruntime", 
  "pip", 
  "pyyaml", 
  "pandas",
  "configspace"), python_version = "3.8")

import("yahpo_gym")

library("yahpogym")

b = BenchmarkSet$new("iaml_glmnet")
obj = b$get_objective("40981", multifidelity = FALSE)

p = opt("random_search")
ois = OptimInstanceBatchMultiCrit$new(obj, search_space = b$get_search_space(drop_fidelity_params = TRUE), terminator = trm("evals", n_evals = 10))
p$optimize(ois)

return(ois)