con = file("competitors_mlr3mbo_numeric.log")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

library(batchtools)
library(data.table)
source("common/submit.R")
source("common/numeric_objective.R")

registry_name = "/glade/derecho/scratch/marcbecker/mbo_config/registries/competitors_mlr3mbo_numeric"
unlink(registry_name, recursive = TRUE)
packages = c(
  "data.table",
  "mlr3",
  "mlr3learners",
  "mlr3misc",
  "mlr3mbo",
  "mlr3pipelines",
  "bbotk",
  "paradox",
  "ranger",
  "R6",
  "checkmate",
  "yahpogym",
  "renv")

reg = makeExperimentRegistry(
  file.dir = registry_name,
  packages = packages,
  source = c("common/numeric_objective.R"),
  seed = 1832
)
reg$cluster.functions = makeClusterFunctionsHyperQueue()
saveRegistry(reg)

# problems
instances = fread("common/numeric_instances.csv", colClasses = c("instance" = "character"))
instances[, budget := as.integer(20 + 40 * sqrt(dim))]

pwalk(instances, function(scenario, instance, target_variable, budget, ...) {
  id = sprintf("%s_%s", scenario, instance)
  addProblem(id, data = list(scenario = scenario, instance = instance, target_variable = target_variable, budget = budget))
})


# algorithms
addAlgorithm("mlr3mbo", fun = function(job, data, instance, ...) {
  renv::load(".")

  xs = list(
    input_trafo = "none",
    output_trafo = "log",
    init = "random",
    init_size_fraction = 0.05,
    random_interleave_iter = 0,
    surrogate = "gam",
    extratrees = NA_character_,
    trees = NA_integer_,
    variance_estimator = NA_character_,
    kernel = NA_character_,
    nugget = NA_real_,
    scaling = NA,
    acqf = "CB",
    lambda = 3,
    acqopt = "CMAES",
    epsilon_decay = NA,
    lambda_decay = FALSE
  )
  optim_instance = invoke(numeric_objective, .args = c(instance, xs))
  optim_instance$archive$data
})

# experiments
addExperiments(repls = 30L)

submitJobs(findJobs(), reg = reg)



