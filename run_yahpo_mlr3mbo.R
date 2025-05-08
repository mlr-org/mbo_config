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

  #log_scale = TRUE
  #init = "sobol"
  #init_size_fraction = "0.25"
  #random_interleave_iter = "0"
  #rf_type = "standard"
  #acqf = "EI"
  #lambda = NA_character_
  #acqopt = "RS"
  #epsilon_decay = FALSE
  #lambda_decay = NA

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

  learner = LearnerRegrRangerMbo$new()
  learner$predict_type = "se"
  learner$param_set$values$keep.inbag = TRUE

  if (rf_type == "standard") {
    learner$param_set$values$se.method = "jack"
    learner$param_set$values$splitrule = "variance"
    learner$param_set$values$num.trees = 1000L
  } else if (rf_type == "extratrees") {
    learner$param_set$values$se.method = "jack"
    learner$param_set$values$splitrule = "extratrees"
    learner$param_set$values$num.random.splits = 1L
    learner$param_set$values$num.trees = 1000L
  } else if (rf_type == "smaclike_simple") {
    learner$param_set$values$se.method = "simple"
    learner$param_set$values$splitrule = "variance"
    learner$param_set$values$num.trees = 10L
    learner$param_set$values$replace = TRUE
    learner$param_set$values$sample.fraction = 1
    learner$param_set$values$min.node.size = 3
    learner$param_set$values$min.bucket = 3
    learner$param_set$values$mtry.ratio = 5/6
  } else if (rf_type == "smaclike_law_of_total_variance") {
    learner$param_set$values$se.method = "law_of_total_variance"
    learner$param_set$values$splitrule = "variance"
    learner$param_set$values$num.trees = 10L
    learner$param_set$values$replace = TRUE
    learner$param_set$values$sample.fraction = 1
    learner$param_set$values$min.node.size = 3
    learner$param_set$values$min.bucket = 3
    learner$param_set$values$mtry.ratio = 5/6
  }

  surrogate = SurrogateLearner$new(
    #GraphLearner$new(
    #  po("colapply", applicator = as.factor, affect_columns = selector_type("character")) %>>%
    #  po("imputesample", affect_columns = selector_type("logical")) %>>%
    #  po("imputeoor", multiplier = 3, affect_columns = selector_type(c("integer", "numeric", "character", "factor", "ordered"))) %>>%
    #  po("fixfactors", affect_columns = selector_type(c("character", "factor", "ordered")), droplevels = TRUE) %>>%
    #  po("imputesample", id = "final_imputesample", affect_columns = selector_type(c("character", "factor", "ordered"))) %>>%
    #  learner
    #)
    GraphLearner$new(
      ppl("robustify", learner = learner, impute_missings = TRUE, factors_to_numeric = FALSE, ordered_action = "ignore", character_action = "factor", POSIXct_action = "ignore") %>>%
      learner
    )
  )
  surrogate$param_set$values$catch_errors = FALSE

  acq_optimizer = if (acqopt == "RS_1000") {
    AcqOptimizer$new(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 1000L))
  } else if (acqopt == "RS") {
    AcqOptimizer$new(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 30000L))
  } else if (acqopt == "FS") {
    n_repeats = 3L
    maxit = 9L
    batch_size = ceiling((30000L / n_repeats) / (1 + maxit)) # 1000L
    AcqOptimizer$new(opt("focus_search", n_points = batch_size, maxit = maxit), terminator = trm("evals", n_evals = 30000L))
  } else if (acqopt == "LS") {
    acq_optimizer = AcqOptimizer$new(opt("local_search", n_initial_points = 10L, initial_random_sample_size = 10000L), terminator = trm("evals", n_evals = 30000L))
    acq_optimizer
  }
  acq_optimizer$param_set$values$catch_errors = FALSE

  acq_function = if (acqf == "EI" && log_scale) {
    AcqFunctionLogEI$new()
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

  if (!log_scale) {
    bayesopt_ego(
      optim_instance,
      surrogate = surrogate,
      acq_function = acq_function,
      acq_optimizer = acq_optimizer,
      random_interleave_iter = random_interleave_iter,
      init_design_size = init_design_size)
  } else {
    bayesopt_ego_log(
      optim_instance,
      surrogate = surrogate,
      acq_function = acq_function,
      acq_optimizer = acq_optimizer,
      random_interleave_iter = random_interleave_iter,
      init_design_size = init_design_size)
  }

  optim_instance
}

# add algorithms
addAlgorithm("mlr3mbo_configured", fun = mlr3mbo_wrapper)

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
optimizers = data.table(algorithm = c("mlr3mbo_configured"))

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

