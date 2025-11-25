con = file("competitors_mlr3mbo_mixed.log")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

library(batchtools)
library(data.table)
source("common/submit.R")
source("common/mixed_objective.R")

registry_name = "/glade/derecho/scratch/marcbecker/mbo_config/registries/competitors_mlr3mbo_mixed"
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
  source = c("common/mixed_objective.R"),
  seed = 7832
)

#reg = loadRegistry(registry_name, writeable = TRUE)
reg$cluster.functions = makeClusterFunctionsHyperQueue()
saveRegistry(reg)

# problems
instances = fread("common/mixed_instances.csv", colClasses = c("instance" = "character"))
instances[, budget := as.integer(20 + 40 * sqrt(dim))]

pwalk(instances, function(scenario, instance, target_variable, budget, dim, ...) {
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
    trees = 500,
    variance_estimator = "law_of_total_variance",
    acqf = "CB",
    lambda = 1,
    acqopt = "LS",
    epsilon_decay = NA,
    lambda_decay = FALSE
  )
  optim_instance = invoke(mixed_objective, .args = c(instance, xs))
  optim_instance$archive$data
})

# experiments
addExperiments(repls = 30L)

submitJobs(findJobs(), reg = reg)

waitForJobs(findJobs(), reg = reg)