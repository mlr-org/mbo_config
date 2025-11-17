library(batchtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(bbotk)
library(paradox)
library(R6)
library(checkmate)

YAHPO_BENCHMARK = "mixed_deps"  # "pure_numeric", "mixed", "mixed_deps"

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
reg = makeExperimentRegistry(registry_name, packages = packages, source = c(source_files, "common/pure_numeric_helper.R"))
saveRegistry(reg)
# reg = loadRegistry(registry_name)

rs_wrapper = function(job, data, instance, ...) {
  reticulate::use_virtualenv("/glade/u/home/lschneider/mbo_config/yahpo_venv", required = TRUE)
  library(yahpogym)
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  rs_budget = 10^6L
  benchmark = BenchmarkSet$new(instance$scenario, instance = instance$instance)
  benchmark$subset_codomain(instance$target)
  objective = benchmark$get_objective(instance$instance, multifidelity = FALSE)
  search_space = benchmark$get_search_space(drop_fidelity_params = TRUE)
  if (instance$benchmark == "pure_numeric") {
    objective = fix_objective_domain_constants_pure_numeric(instance$scenario, objective=objective)
    search_space = get_search_space_pure_numeric(instance$scenario)
  }
  optim_instance = oi(objective, search_space = search_space, terminator = trm("evals", n_evals = rs_budget))
  optim_instance

  optim_instance = make_optim_instance_rs(instance)
  optimizer = opt("random_search", batch_size = 10^4L)
  optimizer$optimize(optim_instance)
  optim_instance
}

# add algorithms
addAlgorithm("rs", fun = rs_wrapper)

if (YAHPO_BENCHMARK == "pure_numeric") {
  # setup = data.table(
  #   benchmark = YAHPO_BENCHMARK,
  #   scenario = rep(c("lcbench", paste0("rbv2_", c("glmnet", "rpart", "ranger", "xgboost"))), c(3L, 2L, 2L, 2L, 4L)),
  #   instance = c(
  #       "167168", "189873", "189906",
  #       "375", "458",
  #       "14", "40499",
  #       "16", "42",
  #       "12", "1501", "16", "40499"
  #   ),
  #   target_variable = rep(c("val_accuracy", "acc"), c(3L, 10L)),
  #   direction = rep("maximize", 13L),
  #   budget = rep(c(126L, 77L, 100L, 100L, 147L), c(3L, 2L, 2L, 2L, 4L))
  # )
  setup = mlr3misc::rowwise_table(
    ~benchmark, ~scenario, ~instance, ~target_variable, ~direction, ~budget,
    "pure_numeric", "lcbench", "167168", "val_accuracy", "maximize", 400L,
    "pure_numeric", "lcbench", "189873", "val_accuracy", "maximize", 400L,
    "pure_numeric", "lcbench", "189906", "val_accuracy", "maximize", 400L,
    "pure_numeric", "rbv2_rpart", "14", "acc", "maximize", 400L,
    "pure_numeric", "rbv2_rpart", "40499", "acc", "maximize", 400L,
    "pure_numeric", "rbv2_xgboost", "12", "acc", "maximize", 400L,
    "pure_numeric", "rbv2_xgboost", "1501", "acc", "maximize", 400L,
    "pure_numeric", "rbv2_xgboost", "40499", "acc", "maximize", 400L
  )
} else if (YAHPO_BENCHMARK == "mixed") {
    stop("TBD")
} else if (YAHPO_BENCHMARK == "mixed_deps") {
    setup = data.table(
    benchmark = YAHPO_BENCHMARK,
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
    budget = rep(c(126L, 254L, 90L, 110L, 134L, 170L, 267L), c(3L, 1L, 2L, 2L, 2L, 4L, 6L))
  )
}

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
