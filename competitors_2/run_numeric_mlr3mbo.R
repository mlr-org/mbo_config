library(batchtools)
library(data.table)
source("common/submit.R")
source("common/numeric_objective.R")

registry_name = "/glade/derecho/scratch/marcbecker/mbo_config/registries/competitors_mlr3mbo_numeric_2"
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
  seed = 7832
)

#reg = loadRegistry(registry_name, writeable = TRUE)
reg$cluster.functions = makeClusterFunctionsHyperQueue()
saveRegistry(reg)

# problems
setup = fread("common/pure_numeric_instances.csv", colClasses = c("instance" = "character"))
setup[, budget := as.integer(20 + 40 * sqrt(dim))]

pwalk(setup, function(scenario, instance, target_variable, budget, ...) {
  id = sprintf("%s_%s", scenario, instance)
  addProblem(id, data = list(scenario = scenario, instance = instance, target_variable = target_variable, budget = budget))
})


# algorithms
addAlgorithm("mlr3mbo", fun = function(job, data, instance, ...) {
  renv::load(".")
  logger = lgr::get_logger("mlr3/bbotk")
  logger$set_threshold("warn")

  xs = list(
    input_trafo = "none",
    output_trafo = "log",
    init = "lhs",
    init_size_fraction = 0.25,
    random_interleave_iter = 0,
    surrogate = "gp",
    extratrees = NA_character_,
    trees = NA_integer_,
    variance_estimator = NA_character_,
    kernel = "matern3_2",
    nugget = 1e-8,
    scaling = FALSE,
    acqf = "EI",
    lambda = NA_integer_,
    acqopt = "CMAES",
    epsilon_decay = FALSE,
    lambda_decay = NA
  )
  optim_instance = invoke(numeric_objective, .args = c(instance, xs))
  optim_instance$archive$data
})

# experiments
addExperiments(repls = 30L)

submitJobs(findJobs(), reg = reg)

