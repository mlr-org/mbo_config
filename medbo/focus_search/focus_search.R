library(batchtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(bbotk)
library(paradox)
library(R6)
library(checkmate)
library(reticulate)
library(yahpogym)

use_condaenv("yahpo_gym", required=TRUE)
yahpo_gym = import("yahpo_gym")

reg = makeExperimentRegistry(
  file.dir = "/gscratch/mbecke16/registry_focus_search",
  conf.file = "beartooth/batchtools.conf.R",
  packages = "renv")

# add problems

## yahpo
loader_yahpo = function(scenario, instance, target, budget) {
  library(reticulate)
  library(yahpogym)

  use_condaenv("yahpo_gym", required = TRUE)
  yahpo_gym = import("yahpo_gym")

  benchmark = BenchmarkSet$new(scenario)
  benchmark$subset_codomain(target)
  objective = benchmark$get_objective(instance, multifidelity = FALSE)

  oi(
    objective,
    search_space = benchmark$get_search_space(drop_fidelity_params = TRUE),
    terminator = trm("evals", n_evals = budget),
    check_values = FALSE)
}

benchmarks = yahpogym::list_benchmarks()
scenarios_rbv2 = grep("^rbv2", names(benchmarks$configs), value = TRUE)

walk(scenarios_rbv2, function(scenario) {
  b = BenchmarkSet$new(scenario)
  walk(b$instances, function(instance) {
    addProblem(
      name = sprintf("%s_%s", scenario, instance),
      data = list(
        loader = loader_yahpo,
        args = list(scenario = scenario, instance = instance, target = "acc", budget = 200 * 3000)
      )
    )
  })
})

scenarios_lcbench = grep("^lcbench", names(benchmarks$configs), value = TRUE)

walk(scenarios_lcbench, function(scenario) {
  b = BenchmarkSet$new(scenario)
  walk(b$instances, function(instance) {
    addProblem(
      name = sprintf("%s_%s", scenario, instance),
      data = list(
        loader = loader_yahpo,
        args = list(scenario = scenario, instance = instance, target = "val_accuracy", budget = 200 * 3000)
      )
    )
  })
})

# add problem
focus_search = function(job, data, instance, ...) {
  renv::load(".")

  library(bbotk)
  library(mlr3misc)

  optim_instance = invoke(data$loader, .args = data$args)

  n_evals = data$args$budget
  batch_size = 10000L
  maxit = ceiling((n_evals / (batch_size)))
  optimizer = opt("focus_search", n_points = batch_size, maxit = maxit)

  optimizer$optimize(optim_instance)
  optim_instance

  list(
    data = optim_instance$archive$data,
    target = optim_instance$archive$cols_y)
}

addAlgorithm(
  name = "focus_search",
  fun = focus_search
)

addExperiments(repls = 30)

ids = getJobTable()[, list(job.id)]
ids[, chunk := batchtools::chunk(job.id, chunk.size = 30, shuffle = FALSE)]

resources = list(
  walltime = 3600 * 6,
  memory = 4000,
  ncpus = 2)

submitJobs(ids, resources = resources)
