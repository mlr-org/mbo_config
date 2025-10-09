reticulate::use_condaenv("yahpo_gym", required = TRUE)

library("yahpogym")
library("bbotk")

b = BenchmarkSet$new("iaml_glmnet")
b$subset_codomain("auc")
obj = b$get_objective("40981", multifidelity = FALSE)

instance = oi(
  objective = obj,
  terminator = trm("evals", n_evals = 30000L),
)

optimizer = opt("local_search", n_searches = 10L, n_steps = 30L, n_neighbors = 100L)

optimizer$optimize(instance)
