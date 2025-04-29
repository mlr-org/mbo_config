library(batchtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(bbotk)
library(paradox)
library(R6)
library(checkmate)

YAHPO_BENCHMARK = "pure_numeric"  # "pure_numeric", "mixed", ""

reticulate::use_virtualenv("/glade/u/home/lschneider/mbo_config/yahpo_venv", required = TRUE)
library(reticulate)
yahpo_gym = import("yahpo_gym")

packages = c("data.table", "mlr3", "mlr3misc", "bbotk", "paradox", "R6", "checkmate")

root = here::here()
experiments_dir = file.path(root)

source_files = map_chr(c("helper.R"), function(x) file.path(experiments_dir, x))
for (source_file in source_files) {
  source(source_file)
}


registry_name = gsub("YAHPO_BENCHMARK", replacement = YAHPO_BENCHMARK, x = "/glade/derecho/scratch/lschneider/yahpo_YAHPO_BENCHMARK_rs")
reg = makeExperimentRegistry(registry_name, packages = packages, source = source_files)
saveRegistry(reg)
# reg = loadRegistry(registry_name)

rs_wrapper = function(job, data, instance, ...) {
  reticulate::use_virtualenv("/glade/u/home/lschneider/mbo_config/yahpo_venv", required = TRUE)
  library(yahpogym)
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  optim_instance = make_optim_instance_rs(instance)
  optimizer = opt("random_search", batch_size = 10^4L)
  optimizer$optimize(optim_instance)
  optim_instance
}

# add algorithms
addAlgorithm("rs", fun = rs_wrapper)

if (YAHPO_BENCHMARK == "pure_numeric") {
  setup = data.table(
    scenario = rep(c("lcbench", paste0("rbv2_", c("glmnet", "rpart", "ranger", "xgboost"))), c(3L, 2L, 2L, 2L, 4L)),
    instance = c(
        "167168", "189873", "189906",
        "375", "458",
        "14", "40499",
        "16", "42",
        "12", "1501", "16", "40499"
    ),
    target_variable = rep(c("val_accuracy", "acc"), c(3L, 10L)),
    direction = rep("maximize", 13L),
    budget = rep(c(126L, 90L, 134L, 110L, 170L), c(3L, 2L, 2L, 2L, 4L)),
    benchmark = YAHPO_BENCHMARK
  )
} else if (YAHPO_BENCHMARK == "mixed") {
    stop("TBD")
} else if (YAHPO_BENCHMARK == "") {
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
    budget = rep(c(126L, 250L, 90L, 134L, 110L, 170L, 267L), c(3L, 1L, 2L, 2L, 2L, 4L, 6L)),
    benchmark = YAHPO_BENCHMARK
  )
}

setup[, id := seq_len(.N)]

# add problems
prob_designs = map(seq_len(nrow(setup)), function(i) {
  prob_id = paste0(setup[i, ]$scenario, "_", setup[i, ]$instance, "_", setup[i, ]$target_variable)
  addProblem(prob_id, data = list(scenario = setup[i, ]$scenario, instance = setup[i, ]$instance, target_variable = setup[i, ]$target_variable, direction = setup[i, ]$direction, budget = setup[i, ]$budget))
  setNames(list(setup[i, ]), nm = prob_id)
})
prob_names = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = prob_names

# add jobs for optimizers
optimizers = data.table(algorithm = c("rs"))

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
resources.default = list(walltime = 3600L * 6L, memory = 16000L, ntasks = 1L, ncpus = 1L, nodes = 1L)
submitJobs(jobs, resources = resources.default)

done = findDone()
results = reduceResultsList(done, function(result, job) {
  data = result$archive$data
  pars = job$pars
  target_variable = pars$prob.pars$target_variable
  tmp = data[, eval(target_variable), with = FALSE]
  colnames(tmp) = "target"
  tmp[, orig_direction := pars$prob.pars$direction]
  if (pars$prob.pars$direction == "maximize") {
    tmp[, target := - target]
  }
  tmp[, best := cummin(target)]
  tmp[, method := pars$algo.pars$algorithm]
  tmp[, benchmark := pars$prob.pars$benchmark]
  tmp[, scenario := pars$prob.pars$scenario]
  tmp[, instance := pars$prob.pars$instance]
  tmp[, target_variable := pars$prob.pars$target_variable]
  tmp[, problem := paste0(scenario, "_", instance, "_", target_variable)]
  tmp[, repl := job$repl]
  tmp[, iter := seq_len(.N)]
  tmp
})
results = rbindlist(results, fill = TRUE)
if (YAHPO_BENCHMARK == "pure_numeric") {
  saveRDS(results, "yahpo_pure_numeric_rs_raw.rds")
} else if (YAHPO_BENCHMARK == "mixed") {
  stop("TBD")
} else if (YAHPO_BENCHMARK == "") {
  saveRDS(results, "yahpo_rs_raw.rds")
}

results_simulated = reduceResultsList(done, function(result, job) {
  n_repl = 30L
  data = result$archive$data
  pars = job$pars
  target_variable = pars$prob.pars$target_variable
  tmp = data[, eval(target_variable), with = FALSE]
  colnames(tmp) = "target"
  tmp[, orig_direction := pars$prob.pars$direction]
  if (pars$prob.pars$direction == "maximize") {
    tmp[, target := - target]
  }
  map_dtr(seq_len(n_repl), function(repl) {
    subset = tmp[sample(.N, size = pars$prob.pars$budget, replace = FALSE), ]
    subset[, best := cummin(target)]
    subset[, method := pars$algo.pars$algorithm]
    subset[, benchmark := pars$prob.pars$benchmark]
    subset[, scenario := pars$prob.pars$scenario]
    subset[, instance := pars$prob.pars$instance]
    subset[, target_variable := pars$prob.pars$target_variable]
    subset[, budget := pars$prob.pars$budget]
    subset[, problem := paste0(scenario, "_", instance, "_", target_variable)]
    subset[, repl := repl]
    subset[, iter := seq_len(.N)]
    subset
  })
})
results_simulated = rbindlist(results_simulated, fill = TRUE)
if (YAHPO_BENCHMARK == "pure_numeric") {
  saveRDS(results_simulated, "yahpo_pure_numeric_rs_simulated.rds")
} else if (YAHPO_BENCHMARK == "mixed") {
  stop("TBD")
} else if (YAHPO_BENCHMARK == "") {
  saveRDS(results_simulated, "yahpo_rs_simulated.rds")
}

results_reference = results_simulated[iter == budget, .(mean_best = mean(best), se_best = sd(best) / sqrt(.N)), by = .(scenario, instance, target_variable, orig_direction, problem)]
results_reference = merge(results_reference, results[iter == 10^6L, c("problem", "best")], by = "problem")
if (YAHPO_BENCHMARK == "pure_numeric") {
  saveRDS(results_reference, "yahpo_pure_numeric_rs_reference.rds")
} else if (YAHPO_BENCHMARK == "mixed") {
  stop("TBD")
} else if (YAHPO_BENCHMARK == "") {
  saveRDS(results_reference, "yahpo_rs_reference.rds")
}

