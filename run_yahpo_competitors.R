library(batchtools)
library(mlr3misc)
library(data.table)

packages = c("data.table")

root = here::here()
experiments_dir = file.path(root)

source_files = map_chr(c("helper.R"), function(x) file.path(experiments_dir, x))
for (source_file in source_files) {
  source(source_file)
}

reg = makeExperimentRegistry("/glade/derecho/scratch/lschneider/yahpo_competitors", packages = packages, source = source_files)
saveRegistry(reg)
# reg = loadRegistry("/glade/derecho/scratch/lschneider/yahpo_competitors")

smac4hpo_wrapper = function(job, data, instance, ...) {
  reticulate::use_virtualenv("/glade/u/home/lschneider/mbo_config/smac_venv", required = TRUE)
  library(reticulate)
  py_run_file("smac_wrapper.py")
  result = py$run_smac4hpo(scenario = instance$scenario, instance = instance$instance, target_variable = instance$target_variable, direction = instance$direction, budget = instance$budget, seed = job$seed)
  result = as.data.table(result)
  result
}

# add algorithms
addAlgorithm("smac4hpo", fun = smac4hpo_wrapper)

setup = data.table(
  scenario = rep(c("lcbench", "nb301", paste0("rbv2_", c("glmnet", "rpart", "ranger", "xgboost", "super"))), c(3L, 1L, 2L, 2L, 2L, 4L, 6L)),
  instance = c(
      "167168", "189873", "189906",
      "CIFAR10",
      "375", "458",
      "14", "40499",
      "16", "42",
      "12", "1501", "16", "40499",
      "1053", "1457", "1063", "1479", "15", "1468"
  ),
  target_variable = rep(c("val_accuracy", "acc"), c(4L, 16L)),
  direction = rep("maximize", 20L),
  budget = rep(c(126L, 250L, 90L, 134L, 110L, 170L, 267L), c(3L, 1L, 2L, 2L, 2L, 4L, 6L))
)

setup[, id := seq_len(.N)]

# add problems
prob_designs = map(seq_len(nrow(setup)), function(i) {
  prob_id = paste0(setup[i, ]$scenario, "_", setup[i, ]$instance, "_", setup[i, ]$target_variable)
  addProblem(prob_id, data = list(scenario = setup[i, ]$scenario, instance = setup[i, ]$instance, target_variable = setup[i, ]$target_variable, budget = setup[i, ]$budget, on_integer_scale = setup[i, ]$on_integer_scale))
  setNames(list(setup[i, ]), nm = prob_id)
})
prob_names = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = prob_names

# add jobs for optimizers
optimizers = data.table(algorithm = c("smac4hpo"))

for (i in seq_len(nrow(optimizers))) {
  algo_designs = setNames(list(optimizers[i, ]), nm = optimizers[i, ]$algorithm)

  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = algo_designs,
    repls = 30L
  )
  addJobTags(ids, as.character(optimizers[i, ]$algorithm))
}

jobs = findJobs()
resources.default = list(walltime = 3600L * 1L, memory = 4000L, ntasks = 1L, ncpus = 1L, nodes = 1L)
submitJobs(jobs, resources = resources.default)

done = findDone()
results = reduceResultsList(done, function(result, job) {
  data = result
  pars = job$pars
  target_variable = pars$prob.pars$target_variable
  tmp = data[, eval(target_variable), with = FALSE]
  colnames(tmp) = "target"
  tmp[, orig_direction := pars$prob.pars$direction]
  tmp[, best := cummin(target)]
  tmp[, method := pars$algo.pars$algorithm]
  tmp[, scenario := pars$prob.pars$scenario]
  tmp[, instance := pars$prob.pars$instance]
  tmp[, target_variable := pars$prob.pars$target_variable]
  tmp[, problem := paste0(scenario, "_", instance, "_", target_variable)]
  tmp[, repl := job$repl]
  tmp[, iter := seq_len(.N)]
  tmp
})
results = rbindlist(results, fill = TRUE)
saveRDS(results, "yahpo_competitors_raw.rds")

