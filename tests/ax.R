library(batchtools)
library(paradox)
library(mlr3misc)
library(data.table)

packages = c("data.table", "paradox")

root = here::here()
experiments_dir = file.path(root)

source_files = map_chr(c("runtime/helper.R"), function(x) file.path(experiments_dir, x))
for (source_file in source_files) {
  source(source_file)
}

unlink(file.path(root, "ax_competitors"), recursive = TRUE)
reg = makeExperimentRegistry("ax_competitors",
  conf.file = NA,
  packages = packages,
  source = source_files)
saveRegistry(reg)


# smac4hpo_wrapper = function(job, data, instance, ...) {
#   reticulate::use_condaenv("smac", required = TRUE)
#   library(reticulate)
#   py_run_file("competitor_wrapper/smac_wrapper.py")
#   result = py$run_smac(benchmark = instance$benchmark, scenario = instance$scenario, instance = instance$instance, target_variable = instance$target_variable, direction = instance$direction, budget = instance$budget, seed = job$seed, facade = "hpo")
#   result = as.data.table(result)
#   result
# }

# smac4bb_wrapper = function(job, data, instance, ...) {
#   reticulate::use_virtualenv("/glade/u/home/lschneider/mbo_config/smac_venv", required = TRUE)
#   library(reticulate)
#   py_run_file("smac_wrapper.py")
#   result = py$run_smac(benchmark = instance$benchmark, scenario = instance$scenario, instance = instance$instance, target_variable = instance$target_variable, direction = instance$direction, budget = instance$budget, seed = job$seed, facade = "bb")
#   result = as.data.table(result)
#   result
# }

# hebo_wrapper = function(job, data, instance, ...) {
#   reticulate::use_condaenv("hebo", required = TRUE)
#   library(reticulate)
#   py_run_file("competitor_wrapper/hebo_wrapper.py")
#   result = py$run_hebo(benchmark = instance$benchmark, scenario = instance$scenario, instance = instance$instance, target_variable = instance$target_variable, direction = instance$direction, budget = instance$budget, seed = job$seed)
#   result = as.data.table(result)
#   result
# }

ax_wrapper = function(job, data, instance, ...) {
  reticulate::use_condaenv("ax", required = TRUE)
  library(reticulate)
  py_run_file("competitor_wrapper/ax_wrapper.py")
  result = py$run_ax(benchmark = instance$benchmark, scenario = instance$scenario, instance = instance$instance, target_variable = instance$target_variable, direction = instance$direction, budget = instance$budget, seed = job$seed)
  result = as.data.table(result)
  result
}

# add algorithms
# addAlgorithm("smac4hpo", fun = smac4hpo_wrapper)
# addAlgorithm("smac4bb", fun = smac4bb_wrapper)
# addAlgorithm("hebo", fun = hebo_wrapper)
addAlgorithm("ax", fun = ax_wrapper)

setup = mlr3misc::rowwise_table(
  ~benchmark, ~scenario, ~instance, ~target_variable, ~direction, ~budget,
  "pure_numeric", "rbv2_rpart", "14", "acc", "maximize", 100L
)
setup[, id := seq_len(.N)]

# add problems
prob_designs = map(seq_len(nrow(setup)), function(i) {
  prob_id = paste0(setup[i, ]$scenario, "_", setup[i, ]$instance, "_", setup[i, ]$target_variable)
  addProblem(prob_id, data = list(benchmark = setup[i, ]$benchmark, scenario = setup[i, ]$scenario, instance = setup[i, ]$instance, target_variable = setup[i, ]$target_variable, direction = setup[i, ]$direction, budget = setup[i, ]$budget))
  setNames(list(setup[i, ]), nm = prob_id)
})
prob_names = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = prob_names

# add jobs for optimizers
optimizers = data.table(algorithm = c("ax")) # "smac4bb", "hebo", "ax"

for (i in seq_len(nrow(optimizers))) {
  algo_designs = setNames(list(optimizers[i, ]), nm = optimizers[i, ]$algorithm)

  ids = addExperiments(
    prob.designs = prob_designs,
    algo.designs = algo_designs,
    repls = 1L
  )
  addJobTags(ids, as.character(optimizers[i, ]$algorithm))
}

jobs = findJobs()
resources.default = list(walltime = 3600L * 1L, memory = 4000L, ntasks = 1L, ncpus = 1L, nodes = 1L)

testJob(1)

#submitJobs(jobs, resources = resources.default)

# done = findDone()
# results = reduceResultsList(done, function(result, job) {
#   # result should already be corrected for minimization
#   data = result
#   pars = job$pars
#   target_variable = pars$prob.pars$target_variable
#   tmp = data[, eval(target_variable), with = FALSE]
#   colnames(tmp) = "target"
#   tmp[, orig_direction := pars$prob.pars$direction]
#   tmp[, best := cummin(target)]
#   tmp[, method := pars$algo.pars$algorithm]
#   tmp[, benchmark := pars$prob.pars$benchmark]
#   tmp[, scenario := pars$prob.pars$scenario]
#   tmp[, instance := pars$prob.pars$instance]
#   tmp[, target_variable := pars$prob.pars$target_variable]
#   tmp[, budget := pars$prob.pars$budget]
#   tmp[, problem := paste0(scenario, "_", instance, "_", target_variable)]
#   tmp[, repl := job$repl]
#   tmp[, iter := seq_len(.N)]
#   tmp
# })
# results = rbindlist(results, fill = TRUE)
# if (YAHPO_BENCHMARK == "pure_numeric") {
#   saveRDS(results, "yahpo_pure_numeric_competitors_raw.rds")
# } else if (YAHPO_BENCHMARK == "mixed") {
#   stop("TBD")
# } else if (YAHPO_BENCHMARK == "mixed_deps") {
#   saveRDS(results, "yahpo_mixed_deps_competitors_raw.rds")
# }

