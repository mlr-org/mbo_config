library(batchtools)
library(mlr3misc)
library(data.table)
library(paradox)

unlink("v2/registry", recursive = TRUE)

reg = makeExperimentRegistry(
  file.dir = "v2/registry",
  conf.file = NA # slurm_wyoming_worker.tmpl
)

instances = runif(10)

iwalk(instances, function(instance, i) {
  addProblem(
    name = sprintf("test_%i", i),
    data = instance,
    seed = 1)
})

worker = function(data, job, instance, z) {
  data^2 + z
}

addAlgorithm(
  name = "mbo",
  fun = worker
)

ades = list(
  mbo = data.table(z = sample(seq(100), 10))
)

addExperiments(algo.designs = ades, repls = 5)

submitJobs()

waitForJobs()

reduceResultsDataTable()
