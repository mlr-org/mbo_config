library(batchtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(mlr3mbo)
library(mlr3pipelines)
library(bbotk)
library(paradox)
library(R6)
library(checkmate)

YAHPO_BENCHMARK = "pure_numeric"  # "pure_numeric", "mixed", ""

reticulate::use_virtualenv("/glade/u/home/lschneider/mbo_config/yahpo_venv", required = TRUE)
library(reticulate)
yahpo_gym = import("yahpo_gym")

packages = c("data.table", "mlr3", "mlr3learners", "mlr3misc", "mlr3mbo", "mlr3pipelines", "bbotk", "paradox", "ranger", "R6", "checkmate")

root = here::here()
experiments_dir = file.path(root)

source_files = map_chr(c("helper.R"), function(x) file.path(experiments_dir, x))
for (source_file in source_files) {
  source(source_file)
}


registry_name = gsub("YAHPO_BENCHMARK", replacement = YAHPO_BENCHMARK, x = "/glade/derecho/scratch/lschneider/yahpo_YAHPO_BENCHMARK_mlr3mbo")
reg = makeExperimentRegistry(registry_name, packages = packages, source = source_files)
saveRegistry(reg)
# reg = loadRegistry(registry_name)

mlr3mbo_wrapper = function(job, data, instance, ...) {
  reticulate::use_virtualenv("/glade/u/home/lschneider/mbo_config/yahpo_venv", required = TRUE)
  library(yahpogym)
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  optim_instance = make_optim_instance(instance)

  log_scale = TRUE
  init = "sobol"
  init_size_fraction = "0.10"
  random_interleave_iter = "0"
  rf_type = "extratrees"
  acqf = "CB"
  lambda = "3"
  acqopt = "FS"
  epsilon_decay = NA
  lambda_decay = TRUE

  random_interleave_iter = as.numeric(random_interleave_iter)
  init_size_fraction = as.numeric(init_size_fraction)
  lambda = as.numeric(lambda)
  init_design_size = ceiling(as.numeric(init_size_fraction) * instance$budget)
  init_design = if (init == "random") {
    generate_design_random(optim_instance$search_space, n = init_design_size)$data
  } else if (init == "lhs") {
    generate_design_lhs(optim_instance$search_space, n = init_design_size)$data
  } else if (init == "sobol") {
    generate_design_sobol(optim_instance$search_space, n = init_design_size)$data
  }

  optim_instance$eval_batch(init_design)

  surrogate = get_surrogate_mixed_deps(surrogate)

  if (log_scale) {
    surrogate$output_trafo = OutputTrafoLog$new(invert_posterior = FALSE)
  }

  acq_optimizer = get_acq_optimizer_mixed_deps(acqopt)

  acq_function = if (acqf == "EI" && log_scale) {
    AcqFunctionEILog$new()
  } else if (acqf == "EI" && !log_scale) {
    AcqFunctionEI$new()
  } else if (acqf == "CB") {
    AcqFunctionCB$new(lambda = as.numeric(lambda))
  } else if (acqf == "PI") {
    AcqFunctionPI$new()
  } else if (acqf == "Mean") {
    AcqFunctionMean$new()
  }

  if (isTRUE(epsilon_decay)) {
    acq_function$constants$values$epsilon = 0.1
    callback_decay_epsilon = callback_batch("mlr3mbo.decay_epsilon",
      on_optimization_end = function(callback, context) {
        epsilon = context$instance$objective$constants$get_values()[["epsilon"]]
        context$instance$objective$constants$set_values("epsilon" = epsilon * 0.99)
      }
    )
    acq_function$callbacks = list(callback_decay_epsilon)
  }

  if (isTRUE(lambda_decay)) {
    callback_decay_lambda = callback_batch("mlr3mbo.decay_lambda",
      on_optimization_end = function(callback, context) {
        lambda = context$instance$objective$constants$get_values()[["lambda"]]
        context$instance$objective$constants$set_values("lambda" = lambda * 0.99)
      }
    )
    acq_function$callbacks = list(callback_decay_lambda)
  }

  bayesopt_ego(
      optim_instance,
      surrogate = surrogate,
      acq_function = acq_function,
      acq_optimizer = acq_optimizer,
      random_interleave_iter = random_interleave_iter,
      init_design_size = init_design_size)

  optim_instance
}

mlr3mbo_wrapper_pure_numeric = function(job, data, instance, ...) {
  reticulate::use_virtualenv("/glade/u/home/lschneider/mbo_config/yahpo_venv", required = TRUE)
  library(yahpogym)
  logger = lgr::get_logger("bbotk")
  logger$set_threshold("warn")
  future::plan("sequential")

  optim_instance = make_optim_instance(instance)

  input_trafo = "none"
  output_trafo = "none"
  init = "random"
  init_size_fraction = "0.25"
  random_interleave_iter = "0"
  surrogate = "gp_5_2"
  acqf = "EI"
  lambda = NA_character_
  acqopt = "RS_1000"
  epsilon_decay = FALSE
  lambda_decay = NA

  random_interleave_iter = as.numeric(random_interleave_iter)
  init_size_fraction = as.numeric(init_size_fraction)
  lambda = as.numeric(lambda)
  init_design_size = ceiling(as.numeric(init_size_fraction) * instance$budget)
  init_design = if (init == "random") {
    generate_design_random(optim_instance$search_space, n = init_design_size)$data
  } else if (init == "lhs") {
    generate_design_lhs(optim_instance$search_space, n = init_design_size)$data
  } else if (init == "sobol") {
    generate_design_sobol(optim_instance$search_space, n = init_design_size)$data
  }

  optim_instance$eval_batch(init_design)

  surrogate = get_surrogate_pure_numeric(surrogate)

  if (input_trafo == "unitcube") {
    surrogate$input_trafo = InputTrafoUnitcube$new()
  }

  if (output_trafo == "standardize") {
    surrogate$output_trafo = OutputTrafoStandardize$new()
  } else if (output_trafo == "log") {
    surrogate$output_trafo = OutputTrafoLog$new(invert_posterior = FALSE)
  }

  acq_optimizer = get_acq_optimizer_pure_numeric(acqopt)

  acq_function = if (acqf == "EI" && output_trafo == "log") {
    AcqFunctionEILog$new()
  } else if (acqf == "EI" && output_trafo != "log") {
    AcqFunctionEI$new()
  } else if (acqf == "CB") {
    AcqFunctionCB$new(lambda = as.numeric(lambda))
  } else if (acqf == "PI") {
    AcqFunctionPI$new()
  } else if (acqf == "Mean") {
    AcqFunctionMean$new()
  }

  if (isTRUE(epsilon_decay)) {
    acq_function$constants$values$epsilon = 0.1
    callback_decay_epsilon = callback_batch("mlr3mbo.decay_epsilon",
      on_optimization_end = function(callback, context) {
        epsilon = context$instance$objective$constants$get_values()[["epsilon"]]
        context$instance$objective$constants$set_values("epsilon" = epsilon * 0.99)
      }
    )
    acq_function$callbacks = list(callback_decay_epsilon)
  }

  if (isTRUE(lambda_decay)) {
    callback_decay_lambda = callback_batch("mlr3mbo.decay_lambda",
      on_optimization_end = function(callback, context) {
        lambda = context$instance$objective$constants$get_values()[["lambda"]]
        context$instance$objective$constants$set_values("lambda" = lambda * 0.99)
      }
    )
    acq_function$callbacks = list(callback_decay_lambda)
  }

  bayesopt_ego(
      optim_instance,
      surrogate = surrogate,
      acq_function = acq_function,
      acq_optimizer = acq_optimizer,
      random_interleave_iter = random_interleave_iter,
      init_design_size = init_design_size)

  optim_instance
}


# add algorithms
addAlgorithm("mlr3mbo_configured", fun = mlr3mbo_wrapper)
addAlgorithm("mlr3mbo_pure_numeric_configured", fun = mlr3mbo_wrapper_pure_numeric)

if (YAHPO_BENCHMARK == "pure_numeric") {
  setup = data.table(
    benchmark = YAHPO_BENCHMARK,
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
    budget = rep(c(126L, 77L, 100L, 100L, 147L), c(3L, 2L, 2L, 2L, 4L))
  )
} else if (YAHPO_BENCHMARK == "mixed") {
    stop("TBD")
} else if (YAHPO_BENCHMARK == "") {
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
#optimizers = data.table(algorithm = c("mlr3mbo_configured"))
optimizers = data.table(algorithm = c("mlr3mbo_pure_numeric_configured"))

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
resources.default = list(walltime = 3600L * 3L, memory = 4000L, ntasks = 1L, ncpus = 1L, nodes = 1L)
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
  tmp[, budget := pars$prob.pars$budget]
  tmp[, problem := paste0(scenario, "_", instance, "_", target_variable)]
  tmp[, repl := job$repl]
  tmp[, iter := seq_len(.N)]
  tmp
})
results = rbindlist(results, fill = TRUE)
if (YAHPO_BENCHMARK == "pure_numeric") {
  saveRDS(results, "yahpo_pure_numeric_mlr3mbo_raw.rds")
} else if (YAHPO_BENCHMARK == "mixed") {
  stop("TBD")
} else if (YAHPO_BENCHMARK == "") {
  saveRDS(results, "yahpo_mlr3mbo_raw.rds")
}

