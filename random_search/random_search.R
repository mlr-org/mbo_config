library(batchtools)
library(bbotk)
library(mlr3misc)
library(reticulate)
library(yahpogym)
library(data.table)

use_condaenv("yahpo_gym", required=TRUE)
yahpo_gym = import("yahpo_gym")

unlink("/gscratch/mbecke16/mbo_config/registry_random_search", recursive = TRUE)

reg = makeExperimentRegistry(
  file.dir = "/gscratch/mbecke16/mbo_config/registry_random_search",
  conf.file = "random_search/batchtools.conf.R",
)

reg = loadRegistry(
  file.dir = "/gscratch/mbecke16/mbo_config/registry_random_search",
  conf.file = "random_search/batchtools.conf.R",
  writeable = TRUE
)


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
    walk(c("acc", "bac", "auc", "logloss"), function(target) {
      addProblem(
        name = sprintf("%s_%s_%s", scenario, instance, target),
        data = list(
          loader = loader_yahpo,
          args = list(scenario = scenario, instance = instance, target = target, budget = 1e6)
        )
      )
    })
  })
})

addAlgorithm(
  name = "mbo",
  fun = function(
    data,
    job,
    instance,
    ....
    ) {

    library(bbotk)
    library(mlr3misc)

    optim_instance = invoke(data$loader, .args = data$args)

    optimizer = opt("random_search", batch_size = 1e3)
    optimizer$optimize(optim_instance)
    optim_instance$archive$data
  }
)

ids = addExperiments(repls = 1, reg = reg)

# submitJobs(ids = 1)

# # testJob(1)

ids[, chunk := batchtools::chunk(job.id, chunk.size = 100, shuffle = FALSE)]
job_ids = submitJobs(ids = ids, reg = reg)$job.id
waitForJobs(ids = job_ids, reg = reg)


job_ids = submitJobs(ids = findExpired(), reg = reg)$job.id


submitJobs(ids = findErrors(), reg = reg)$job.id
