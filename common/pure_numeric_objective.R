library(paradox)

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

get_search_space_pure_numeric = function(scenario) {
  if (scenario == "lcbench") {
    search_space = lcbench_search_space_pure_numeric
    params = setdiff(search_space$ids(), "epoch")
    search_space = search_space$subset(params)
  } else if (scenario == "rbv2_glmnet") {
    search_space = rbv2_glmnet_search_space_pure_numeric
    params = setdiff(search_space$ids(), c("trainsize", "repl"))
    search_space = search_space$subset(params)
  } else if (scenario == "rbv2_rpart") {
    search_space = rbv2_rpart_search_space_pure_numeric
    params = setdiff(search_space$ids(), c("trainsize", "repl"))
    search_space = search_space$subset(params)
  } else if (scenario == "rbv2_ranger") {
    search_space = rbv2_ranger_search_space_pure_numeric
    params = setdiff(search_space$ids(), c("trainsize", "repl"))
    search_space = search_space$subset(params)
  } else if (scenario == "rbv2_xgboost") {
    search_space = rbv2_xgboost_search_space_pure_numeric
    params = setdiff(search_space$ids(), c("trainsize", "repl"))
    search_space = search_space$subset(params)
  }
  search_space
}

fix_objective_domain_constants_pure_numeric = function(scenario, objective) {
  if (scenario == "lcbench") {
    objective = objective
  } else if (scenario == "rbv2_glmnet") {
    constants = objective$constants
    constants = ps_union(
      list(
        constants,
        ps(num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")))
      )
    )
    constants$set_values("num.impute.selected.cpo" = "impute.mean")
    objective$constants = constants

    domain = objective$domain
    domain$deps = data.table()
    params = setdiff(domain$ids(), "num.impute.selected.cpo")
    domain = domain$subset(params)
    objective$domain = domain
  } else if (scenario == "rbv2_rpart") {
    constants = objective$constants
    constants = ps_union(
      list(
        constants,
        ps(num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")))
      )
    )
    constants$set_values("num.impute.selected.cpo" = "impute.mean")
    objective$constants = constants

    domain = objective$domain
    domain$deps = data.table()
    params = setdiff(domain$ids(), "num.impute.selected.cpo")
    domain = domain$subset(params)
    objective$domain = domain
  } else if (scenario == "rbv2_ranger") {
    constants = objective$constants
    constants = ps_union(
      list(
        constants,
        ps(num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
           respect.unordered.factors = p_fct(levels = c("ignore", "order", "partition")),
           splitrule = p_fct(levels = c("gini", "extratrees")))
      )
    )
    constants$set_values("num.impute.selected.cpo" = "impute.mean")
    constants$set_values("respect.unordered.factors" = "ignore")
    constants$set_values("splitrule" = "gini")
    objective$constants = constants

    domain = objective$domain
    domain$deps = data.table()
    params = setdiff(domain$ids(), c("num.impute.selected.cpo", "respect.unordered.factors", "splitrule", "num.random.splits"))
    domain = domain$subset(params)
    objective$domain = domain
  } else if (scenario == "rbv2_xgboost") {
    constants = objective$constants
    constants = ps_union(
      list(
        constants,
        ps(num.impute.selected.cpo = p_fct(levels = c("impute.mean", "impute.median", "impute.hist")),
           booster = p_fct(levels = c("gblinear", "gbtree", "dart")))
      )
    )
    constants$set_values("num.impute.selected.cpo" = "impute.mean")
    constants$set_values("booster" = "gbtree")
    objective$constants = constants

    domain = objective$domain
    domain$deps = data.table()
    params = setdiff(domain$ids(), c("num.impute.selected.cpo", "booster", "rate_drop", "skip_drop"))
    domain = domain$subset(params)
    objective$domain = domain
  }
  objective
}

lcbench_search_space_pure_numeric = ps(
  # OpenML_task_id
  epoch = p_int(lower = 1L, upper = 52L, tags = "budget"),
  batch_size = p_dbl(lower = log(16L), upper = log(512L), tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
  learning_rate = p_dbl(lower = log(1e-4), upper = log(1e-1), tags = "log", trafo = function(x) exp(x)),
  momentum = p_dbl(lower = 0.1, upper = 0.99),
  weight_decay = p_dbl(lower = 1e-5, upper = 1e-1),
  num_layers = p_dbl(lower = 1, upper = 5, tags = "int", trafo = function(x) as.integer(round(x))),
  max_units = p_dbl(lower = log(64L), upper = log(1024L), tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
  max_dropout = p_dbl(lower = 0, upper = 1)
)

rbv2_glmnet_search_space_pure_numeric = ps(
  alpha = p_dbl(lower = 0, upper = 1),
  s = p_dbl(lower = -7, upper = 7, tags = "log", trafo = function(x) exp(x)),
  trainsize = p_dbl(lower = 0.03, upper = 1, tags = "budget"),
  repl = p_int(lower = 1L, upper = 10L, tags = "budget")
  # num.impute.selected.cpo = "impute.mean"
  # task_id
)

rbv2_rpart_search_space_pure_numeric = ps(
  cp = p_dbl(lower = -7, upper = 0, tags = "log", trafo = function(x) exp(x)),
  maxdepth = p_dbl(lower = 1, upper = 30, tags = "int", trafo = function(x) as.integer(round(x))),
  minbucket = p_dbl(lower = 1, upper = 100, tags = "int", trafo = function(x) as.integer(round(x))),
  minsplit = p_dbl(lower = 1, upper = 100, tags = "int", trafo = function(x) as.integer(round(x))),
  trainsize = p_dbl(lower = 0.03, upper = 1, tags = "budget"),
  repl = p_int(lower = 1L, upper = 10L, tags = "budget")
  # num.impute.selected.cpo = "impute.mean"
  # task_id
)

rbv2_ranger_search_space_pure_numeric = ps(
  num.trees = p_dbl(lower = 1, upper = 2000, tags = "int", trafo = function(x) as.integer(round(x))),
  # replace = p_lgl(),
  sample.fraction = p_dbl(lower = 0.1, upper = 1),
  mtry.power = p_dbl(lower = 0, upper = 1),
  #respect.unordered.factors = "ignore"
  min.node.size = p_dbl(lower = 1, upper = 100, tags = "int", trafo = function(x) as.integer(round(x))),
  # splitrule = "gini"
  # num.random.splits = p_int(lower = 1L, upper = 100L, depends = splitrule == "extratrees"),
  trainsize = p_dbl(lower = 0.03, upper = 1, tags = "budget"),
  repl = p_int(lower = 1L, upper = 10L, tags = "budget")
  # num.impute.selected.cpo = "impute.mean"
  # task_id
)

rbv2_xgboost_search_space_pure_numeric = ps(
  #booster = "gbtree"
  nrounds = p_dbl(lower = 2, upper = 8, tags = c("int", "log"), trafo = function(x) as.integer(round(exp(x)))),
  eta = p_dbl(lower = -7, upper = 0, tags = "log", trafo = function(x) exp(x)),
  gamma = p_dbl(lower = -10, upper = 2, tags = "log", trafo = function(x) exp(x)),
  lambda = p_dbl(lower = -7, upper = 7, tags = "log", trafo = function(x) exp(x)),
  alpha = p_dbl(lower = -7, upper = 7, tags = "log", trafo = function(x) exp(x)),
  subsample = p_dbl(lower = 0.1, upper = 1),
  max_depth = p_dbl(lower = 1, upper = 15, tags = "int", trafo = function(x) as.integer(round(x))),
  min_child_weight = p_dbl(lower = 1, upper = 5, tags = "log", trafo = function(x) exp(x)),
  colsample_bytree = p_dbl(lower = 0.01, upper = 1),
  colsample_bylevel = p_dbl(lower = 0.01, upper = 1),
  # rate_drop = p_dbl(lower = 0, upper = 1, depends = booster == "dart"),
  # skip_drop = p_dbl(lower = 0, upper = 1, depends = booster == "dart"),
  trainsize = p_dbl(lower = 0.03, upper = 1, tags = "budget"),
  repl = p_int(lower = 1L, upper = 10L, tags = "budget")
  # num.impute.selected.cpo = "impute.mean"
  # task_id
)

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