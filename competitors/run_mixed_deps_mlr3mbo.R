library(batchtools)
library(data.table)
source("common/submit.R")
source("common/mixed_deps_objective.R")

registry_name = "/glade/derecho/scratch/marcbecker/competitors_mlr3mbo_mixed_deps"
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
  source = c("common/mixed_deps_objective.R"),
  seed = 7832
)
saveRegistry(reg)

# problems
setup = readRDS("common/mixed_deps_instances.rds")
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
    output_trafo = "none",
    init = "random",
    init_size_fraction = 0.25,
    random_interleave_iter = 4,
    trees = 500,
    variance_estimator = "jack",
    acqf = "CB",
    lambda = 1,
    acqopt = "LS",
    epsilon_decay = NA,
    lambda_decay = FALSE
  )
  optim_instance = invoke(mixed_deps_objective, .args = c(instance, xs))
  optim_instance$archive$data
})

# experiments
addExperiments(repls = 30L)

submit(
  findJobs()$job.id, 
  reg, 
  template = "common/pbs_derecho_main.tmpl", 
  jobs_per_node = 128L, 
  chunk_size = 1L, 
  max_concurrent_nodes = 2L,  
  log_dir = "/glade/work/marcbecker/logs",
  log_prefix = "competitors_mlr3mbo_mixed_deps"
)

# restart failed jobs
reg = loadRegistry(registry_name, writeable = TRUE)

submit(
  findNotDone(reg = reg)$job.id, 
  reg, 
  template = "common/pbs_derecho_main.tmpl", 
  jobs_per_node = 128L, 
  chunk_size = 1L, 
  max_concurrent_nodes = 2L,  
  log_dir = ".",
  log_prefix = "competitors_mlr3mbo_mixed_deps"
)
