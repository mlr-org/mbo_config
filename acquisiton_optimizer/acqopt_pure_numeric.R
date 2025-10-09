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
library(reticulate)
library(mlr3learners)
library(yahpogym)

data.table::setDTthreads(1L)

source("coordinate_descent/helper.R")
source("submit_ncar.R")

use_condaenv("yahpo_gym", required = TRUE)
yahpo_gym = import("yahpo_gym")

unlink("/glade/derecho/scratch/marcbecker/mbo_config/acquisition_optimizer_pure_numeric", recursive = TRUE)
reg = makeExperimentRegistry(
  file.dir = "/glade/derecho/scratch/marcbecker/mbo_config/acquisition_optimizer_pure_numeric",
  conf.file = "batchtools.conf.main.R",
  packages = c("mlr3", "mlr3misc", "mlr3mbo", "mlr3pipelines", "bbotk", "paradox", "R6", "checkmate", "reticulate", "mlr3learners", "yahpogym", "data.table", "renv"),
  source = c("coordinate_descent/helper.R"),
  seed = 7832
)

# add problems
instances_desc = mlr3misc::rowwise_table(
  ~scenario, ~instance, ~target_variable, ~direction,
  "lcbench", "167168", "val_accuracy", "maximize",
  # "lcbench", "189873", "val_accuracy", "maximize",
  # "lcbench", "189906", "val_accuracy", "maximize",
  # "rbv2_rpart", "14", "acc", "maximize",
  "rbv2_rpart", "40499", "acc", "maximize",
  # "rbv2_xgboost", "12", "acc", "maximize",
  "rbv2_xgboost", "1501", "acc", "maximize" #,
  # "rbv2_xgboost", "40499", "acc", "maximize"
)

mbo_desc = data.table(
  init_design_size = 100,
  input_trafo = "none",
  output_trafo = "none",
  random_interleave_iter = "0",
  surrogate = "gp",
  extratrees = NA,
  trees = NA_character_,
  variance_estimator = NA_character_,
  kernel = "gauss",
  nugget = "0",
  scaling = FALSE,
  acqf = "EI",
  lambda = NA_character_,
  epsilon_decay = FALSE,
  lambda_decay = NA
)

problems = cbind(instances_desc, mbo_desc)
#merge(instances_desc, mbo_desc, by = NULL, allow.cartesian = TRUE)

pwalk(problems, function(
  scenario, 
  instance, 
  target_variable, 
  direction,
  init_design_size,
  input_trafo,
  output_trafo,
  random_interleave_iter,
  surrogate,
  extratrees,
  trees,
  variance_estimator,
  kernel,
  nugget,
  scaling,
  acqf,
  lambda,
  epsilon_decay,
  lambda_decay) {
  mbo_config_hash = mlr3misc::calculate_hash(
    init_design_size,
    input_trafo,
    output_trafo,
    random_interleave_iter,
    surrogate,
    extratrees,
    trees,
    variance_estimator,
    kernel,
    nugget,
    scaling,
    acqf,
    lambda,
    epsilon_decay,
    lambda_decay
  )
  prob_id = sprintf("%s_%s_%s_%s", scenario, instance, target_variable, mbo_config_hash)
  addProblem(prob_id, data = list(
    scenario = scenario, 
    instance = instance, 
    target_variable = target_variable, 
    direction = direction,
    init_design_size = init_design_size,
    input_trafo = input_trafo,
    output_trafo = output_trafo,
    random_interleave_iter = random_interleave_iter,
    surrogate = surrogate,
    extratrees = extratrees,
    trees = trees,
    variance_estimator = variance_estimator,
    kernel = kernel,
    nugget = nugget,
    scaling = scaling,
    acqf = acqf,
    lambda = lambda,
    epsilon_decay = epsilon_decay,
    lambda_decay = lambda_decay
    ))
})

data = list(
  scenario = "lcbench",
  instance = "167168",
  target_variable = "val_accuracy",
  direction = "maximize",
  init_design_size = 100,
  input_trafo = "none",
  output_trafo = "none",
  random_interleave_iter = "0",
  surrogate = "gp",
  extratrees = NA,
  trees = NA_character_,
  variance_estimator = NA_character_,
  kernel = "gauss",
  nugget = "0",
  scaling = FALSE,
  acqf = "EI",
  lambda = NA_character_,
  epsilon_decay = FALSE,
  lambda_decay = NA
)

addAlgorithm(
  name = "acquisition_optimizer",
  fun = function(
    job,
    data,
    instance,
    acqopt_id,
    acqopt,
    acqopt_budget
    ) {
    renv::load("/glade/u/home/marcbecker/mbo_config/")
    reticulate::use_condaenv("yahpo_gym", required = TRUE)
    library(yahpogym)
    logger = lgr::get_logger("mlr3/bbotk")
    logger$set_threshold("warn")
    future::plan("sequential")

    archive_data = fread(sprintf("/glade/work/marcbecker/mbo_config/random_search/archive/pure_numeric/%s_%s_%s.csv", data$scenario, data$instance, data$target_variable))
    archive_data[, batch_nr := 1L]

    data$budget = 400L
    data$benchmark = "pure_numeric"

    optim_instance = make_optim_instance(data)
    optim_instance$archive$data = archive_data[sample(seq_len(.N), data$init_design_size)]

    random_interleave_iter = as.numeric(data$random_interleave_iter)
    init_size_fraction = as.numeric(data$init_size_fraction)
    lambda = as.numeric(data$lambda)
    surrogate = get_surrogate_pure_numeric(data$surrogate, data$extratrees, data$trees, data$variance_estimator, data$kernel, data$nugget, data$scaling)

    if (data$input_trafo == "unitcube") {
      surrogate$input_trafo = InputTrafoUnitcube$new()
    }

    if (data$output_trafo == "standardize") {
      surrogate$output_trafo = OutputTrafoStandardize$new()
    } else if (data$output_trafo == "log") {
      surrogate$output_trafo = OutputTrafoLog$new(invert_posterior = FALSE)
    }

    acq_function = if (data$acqf == "EI" && data$output_trafo == "log") {
      AcqFunctionEILog$new()
    } else if (data$acqf == "EI" && data$output_trafo != "log") {
      AcqFunctionEI$new()
    } else if (data$acqf == "CB") {
      AcqFunctionCB$new(lambda = as.numeric(data$lambda))
    } else if (data$acqf == "PI") {
      AcqFunctionPI$new()
    } else if (data$acqf == "Mean") {
      AcqFunctionMean$new()
    } else {
      stopf("Unknown acquisition function: %s", data$acqf)
    }

    if (isTRUE(data$epsilon_decay)) {
      acq_function$constants$values$epsilon = 0.1
      callback_decay_epsilon = callback_batch("mlr3mbo.decay_epsilon",
        on_optimization_end = function(callback, context) {
          epsilon = context$instance$objective$constants$get_values()[["epsilon"]]
          context$instance$objective$constants$set_values("epsilon" = epsilon * 0.99)
        }
      )
      acq_function$callbacks = list(callback_decay_epsilon)
    }

    if (isTRUE(data$lambda_decay)) {
      callback_decay_lambda = callback_batch("mlr3mbo.decay_lambda",
        on_optimization_end = function(callback, context) {
          lambda = context$instance$objective$constants$get_values()[["lambda"]]
          context$instance$objective$constants$set_values("lambda" = lambda * 0.99)
        }
      )
      acq_function$callbacks = list(callback_decay_lambda)
    }

    surrogate$archive = optim_instance$archive
    acq_function$surrogate = surrogate
    acqopt = acqopt(acq_function, acqopt_budget * acq_function$domain$length)
    acqopt$acq_function = acq_function
    
    acq_function$surrogate$update()
    acq_function$update()
      
    runtime = system.time({res = acqopt$optimize()})

    data.table(
      budget = acqopt_budget * acq_function$domain$length,
      d = acq_function$domain$length,
      id = acqopt_id,
      runtime = runtime[["elapsed"]],
      y = res[[acq_function$id]],
      acqopt = list(list(acqopt))
    )
  }
)

acqopt_random = function(acq_function, budget) {
  acqopt = AcqOptimizer$new(
    opt("random_search", batch_size = budget), 
    terminator = trm("evals", n_evals = budget))
  acqopt$acq_function = acq_function
  acqopt
}

acqopt_direct = function(acq_function, budget) {
  acqopt = AcqOptimizerDirect$new()
  acqopt$param_set$set_values(
    maxeval = budget,
    restart_strategy = "random",
    n_restarts = 5L,
    ftol_rel = 1e-4
  )
  acqopt$acq_function = acq_function
  acqopt
}

acqopt_local_search = function(acq_function, budget) {
  acqopt = AcqOptimizerLocalSearch$new()
  acqopt$param_set$set_values(
    n_searches = 10L,
    n_steps = ceiling(budget / 1000L),
    n_neighs = 100L
  )
  acqopt$acq_function = acq_function
  acqopt
}

acqopt_cmaes = function(acq_function, budget) {
  acqopt = AcqOptimizerCmaes$new()
  acqopt$param_set$set_values(
    maxEvals = budget,
    xtol = 1e-4
  )
  acqopt$acq_function = acq_function
  acqopt
}

ades = list(
  acquisition_optimizer = data.table(
    acqopt_id = c(
      "random_search",
      "direct",
      "local_search",
      "cmaes"
    ),
    acqopt = list(
      acqopt_random,
      acqopt_direct,
      acqopt_local_search,
      acqopt_cmaes
    ),
    acqopt_budget = rep(100 * 2^(0:7), each = 4L)
  )
)

job_ids = addExperiments(algo.design = ades, repls = 100L)

job_ids = submit_ncar(job_ids$job.id, reg, template = "pbs_derecho_main.tmpl", n_jobs = 128L)
waitForJobs(ids = job_ids, reg = reg)

