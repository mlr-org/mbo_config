library(paradox)

source("common/pure_numeric_helper.R")

pure_numeric_objective = function(
  scenario,
  instance,
  target_variable,
  budget,
  input_trafo,
  output_trafo,
  init,
  init_size_fraction,
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
  acqopt,
  epsilon_decay,
  lambda_decay
  ) {
  assert_string(scenario)
  assert_string(instance)
  assert_string(target_variable)
  assert_int(budget)

  assert_string(input_trafo)
  assert_string(output_trafo)
  assert_string(init)
  assert_number(init_size_fraction)
  assert_int(random_interleave_iter)
  assert_string(surrogate)
  assert_flag(extratrees, na.ok = TRUE)
  assert_int(trees, na.ok = TRUE)
  assert_string(variance_estimator, na.ok = TRUE)
  assert_string(kernel, na.ok = TRUE)
  assert_number(nugget, na.ok = TRUE)
  assert_flag(scaling, na.ok = TRUE)
  assert_string(acqf)
  assert_number(lambda, na.ok = TRUE)
  assert_string(acqopt)
  assert_flag(epsilon_decay, na.ok = TRUE)
  assert_flag(lambda_decay, na.ok = TRUE)

  reticulate::use_condaenv("/glade/work/marcbecker/conda-envs/yahpo_gym", required = TRUE)
  data.table::setDTthreads(1L)
  future::plan("sequential")

  # create optim instance from yahpo gym problem
  benchmark = BenchmarkSet$new(scenario, instance = instance)
  benchmark$subset_codomain(target_variable)
  objective = benchmark$get_objective(instance, multifidelity = FALSE)
  search_space = get_search_space_pure_numeric(scenario)
  objective = fix_objective_domain_constants_pure_numeric(scenario, objective = objective)
  optim_instance = oi(objective, search_space = search_space, terminator = trm("evals", n_evals = budget))

  # intial design
  init_design_size = ceiling(init_size_fraction * budget)
  init_design = if (init == "random") {
    generate_design_random(optim_instance$search_space, n = init_design_size)$data
  } else if (init == "lhs") {
    generate_design_lhs(optim_instance$search_space, n = init_design_size)$data
  } else if (init == "sobol") {
    generate_design_sobol(optim_instance$search_space, n = init_design_size)$data
  }
  optim_instance$eval_batch(init_design)

  # surrogate model
  learner = if (surrogate == "rf") {
    lrn("regr.ranger",
      num.trees = trees,
      se.method = variance_estimator,
      splitrule = if (extratrees) "extratrees" else "variance",
      predict_type = "se",
      keep.inbag = TRUE,
      sample.fraction = 1,
      min.node.size = 3,
      min.bucket = 3,
      mtry.ratio = 5/6
    )
  } else if (surrogate == "gp") {
    lrn("regr.km",
      predict_type = "se",
      control = list(trace = FALSE),
      optim.method = "gen",
      covtype = kernel,
      nugget.stability = nugget,
      scaling = scaling
    )
  }
  surrogate = srlrn(learner)

  # input trafo
  if (input_trafo == "unitcube") {
    surrogate$input_trafo = InputTrafoUnitcube$new()
  }

  # output trafo
  if (output_trafo == "standardize") {
    surrogate$output_trafo = OutputTrafoStandardize$new()
  } else if (output_trafo == "log") {
    surrogate$output_trafo = OutputTrafoLog$new(invert_posterior = FALSE)
  }

  # acq optimizer
  dim = optim_instance$search_space$length
  budget = 100L * dim^2

  if (acqopt == "RS_1000") {
    acq_optimizer = AcqOptimizerRandomSearch$new()
    acq_optimizer$param_set$set_values(max_fevals = 1000L)
  } else if (acqopt == "RS") {
    acq_optimizer = AcqOptimizerRandomSearch$new()
    acq_optimizer$param_set$set_values(max_fevals = budget)
  } else if (acqopt == "LS") {
    acq_optimizer = AcqOptimizerLocalSearch$new()
    acq_optimizer$param_set$set_values(n_searches = 10L, n_steps = ceiling(budget / 300L), n_neighs = 30L)
  } else if (acqopt == "DIRECT") {
    acq_optimizer = AcqOptimizerDirect$new()
    acq_optimizer$param_set$set_values(
      maxeval = budget,
      max_restarts = dim * 5L,
      ftol_rel = 1e-4
    )
  } else if (acqopt == "CMAES") {
    acq_optimizer = AcqOptimizerCmaes$new()
    acq_optimizer$param_set$set_values(
      max_fevals = budget,
      max_restarts = 1000L

    )
  } else if (acqopt == "LBFGSB") {
    acq_optimizer = AcqOptimizerLbfgsb$new()
    acq_optimizer$param_set$set_values(
      maxeval = budget,
      max_restarts = dim * 5L,
      ftol_rel = 1e-4
    )
  }

  # acq function
  acq_function = if (acqf == "EI" && output_trafo == "log") {
    AcqFunctionEILog$new()
  } else if (acqf == "EI" && output_trafo != "log") {
    AcqFunctionEI$new()
  } else if (acqf == "CB") {
    AcqFunctionCB$new(lambda = lambda)
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

if (FALSE) {
  renv::load(".")

  library(data.table)
  library(mlr3)
  library(mlr3learners)
  library(mlr3misc)
  library(mlr3mbo)
  library(mlr3pipelines)
  library(bbotk)
  library(paradox)
  library(R6)
  library(checkmate)
  library(yahpogym)

  xs = list(
    scenario = "lcbench",
    instance = "167168",
    target_variable = "val_accuracy",
    budget = 4L,
    input_trafo = "none",
    output_trafo = "none",
    init = "random",
    init_size_fraction = 0.05,
    random_interleave_iter = 0L,
    surrogate = "gp",
    extratrees = FALSE,
    trees = 500L,
    variance_estimator = "local_common",
    kernel = "gauss",
    nugget = 0L,
    scaling = FALSE,
    acqf = "EI",
    lambda = NA_character_,
    acqopt = "CMAES",
    epsilon_decay = FALSE,
    lambda_decay = FALSE
  )

  mlr3misc::invoke(pure_numeric_objective, .args = xs)
}
