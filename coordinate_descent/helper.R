determine_budget = function(d) {
  ceiling(20 + 40 * sqrt(d))
}

make_optim_instance = function(instance) {
  benchmark = BenchmarkSet$new(instance$scenario, instance = instance$instance)
  benchmark$subset_codomain(instance$target)
  objective = benchmark$get_objective(instance$instance, multifidelity = FALSE)
  search_space = benchmark$get_search_space(drop_fidelity_params = TRUE)
  if (instance$benchmark == "pure_numeric") {
    objective = fix_objective_domain_constants_pure_numeric(instance$scenario, objective=objective)
    search_space = get_search_space_pure_numeric(instance$scenario)
  }
  if (instance$budget != determine_budget(search_space$length)) {
    stop("Incorrect budget.")
  }
  optim_instance = OptimInstanceBatchSingleCrit$new(objective, search_space = search_space, terminator = trm("evals", n_evals = instance$budget))
  optim_instance
}

make_optim_instance_rs = function(instance) {
  rs_budget = 10^6L
  benchmark = BenchmarkSet$new(instance$scenario, instance = instance$instance)
  benchmark$subset_codomain(instance$target)
  objective = benchmark$get_objective(instance$instance, multifidelity = FALSE)
  search_space = benchmark$get_search_space(drop_fidelity_params = TRUE)
  if (instance$benchmark == "pure_numeric") {
    objective = fix_objective_domain_constants_pure_numeric(instance$scenario, objective=objective)
    search_space = get_search_space_pure_numeric(instance$scenario)
  }
  if (instance$budget != determine_budget(search_space$length)) {
    stop("Incorrect budget.")
  }
  optim_instance = OptimInstanceBatchSingleCrit$new(objective, search_space = search_space, terminator = trm("evals", n_evals = rs_budget))
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

get_surrogate_mixed_deps = function(surrogate, extratrees, trees, variance_estimator, kernel, nugget, scaling) {
  learner = if (surrogate == "rf") {
    lrn("regr.ranger",
      num.trees = as.integer(trees),
      se.method = variance_estimator,
      splitrule = if (extratrees) "extratrees" else "variance",
      predict_type = "se",
      keep.inbag = TRUE,
      sample.fraction = 1,
      min.node.size = 3,
      min.bucket = 3,
      mtry.ratio = 5/6
    )
    # impute missings? Ranger can handle missing values now
  } else if (surrogate == "gp") {
    learner = lrn("regr.km",
      predict_type = "se",
      control = list(trace = FALSE),
      optim.method = "gen",
      covtype = kernel,
      nugget.stability = as.numeric(nugget),
      scaling = scaling
    )
     ppl("robustify", learner = learner, impute_missings = TRUE, factors_to_numeric = TRUE, ordered_action = "factor", character_action = "factor", POSIXct_action = "ignore") %>>%
     learner
  }
  srlrn(learner)
  #surrogate$param_set$values$catch_errors = FALSE
}

get_surrogate_pure_numeric = function(surrogate, extratrees, trees, variance_estimator, kernel, nugget, scaling) {
  learner = if (surrogate == "rf") {
    lrn("regr.ranger",
      num.trees = as.integer(trees),
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
      nugget.stability = as.numeric(nugget),
      scaling = scaling
    )
  }

  srlrn(learner)
  #surrogate$param_set$values$catch_errors = FALSE
}

get_acq_optimizer_mixed_deps = function(acqopt) {
  acq_optimizer = if (acqopt == "RS_1000") {
    acqo(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 1000L))
  } else if (acqopt == "RS") {
    acqo(opt("random_search", batch_size = 30000L), terminator = trm("evals", n_evals = 30000L))
  # } else if (acqopt == "FS") {
  #   n_repeats = 3L
  #   maxit = 9L
  #   batch_size = ceiling((30000L / n_repeats) / (1 + maxit)) # 1000L
  #   AcqOptimizer$new(opt("focus_search", n_points = batch_size, maxit = maxit), terminator = trm("evals", n_evals = 30000L))
  } else if (acqopt == "LS") {
    acqo(opt("local_search", n_searches = 10L, n_steps = 30L, n_neighbors = 100L), terminator = trm("evals", n_evals = 30000L))
  }
  #acq_optimizer$param_set$values$catch_errors = FALSE
  acq_optimizer
}

get_acq_optimizer_pure_numeric = function(acqopt) {
 acq_optimizer = if (acqopt == "RS_1000") {
    acqo(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 1000L))
  } else if (acqopt == "RS") {
    acqo(opt("random_search", batch_size = 30000L), terminator = trm("evals", n_evals = 30000L))
  # } else if (acqopt == "FS") {
  #   n_repeats = 3L
  #   maxit = 9L
  #   batch_size = ceiling((30000L / n_repeats) / (1 + maxit)) # 1000L
  #   AcqOptimizer$new(opt("focus_search", n_points = batch_size, maxit = maxit), terminator = trm("evals", n_evals = 30000L))
  } else if (acqopt == "LS") {
    acqo(opt("local_search", n_searches = 10L, n_steps = 30L, n_neighbors = 100L), terminator = trm("evals", n_evals = 30000L))
  } else if (acqopt == "DIRECT") {
    # optimizer = opt("chain",
    #   optimizers = rep(list(opt("random_search", batch_size = 5000L), opt("nloptr", algorithm = "NLOPT_GN_DIRECT_L")), times = 5L),
    #   terminators = rep(list(trm("evals", n_evals = 5000L), trm("combo", terminators = list(trm("evals", n_evals = 1000L), trm("stagnation", iters = 100L, threshold = 1e-12)))), times = 5L))
    # cb = callback_batch("start_values",
    #   on_optimization_begin = function(callback, context) {
    #   if (class(context$optimizer)[1L] == "OptimizerBatchNLoptr") {
    #     start = unlist(context$result[, context$instance$archive$cols_x, with = FALSE])  # previous random search
    #     context$optimizer$param_set$values$start_values = "custom"
    #     context$optimizer$param_set$values$start = start
    #   }
    # })
    acq_optimizer = AcqOptimizerDirect$new()
    acq_optimizer$param_set$set_values(
       random_restart_size = 5000L,
       n_random_restarts = 5L,
       maxeval = 1000,
       ftol_rel = 1e-4 # delta_f / f < 1e-4
    )
    acq_optimizer
  } else if (acqopt == "CMAES") {
    acq_optimizer = AcqOptimizerCmaes$new()
    acq_optimizer$param_set$set_values(
      maxit = 6000L,
      restart_strategy = "ipop",
      n_restarts = 5L,
      population_multiplier = 2L
    )
    acq_optimizer
  } else if (acqopt == "LBFGSB") {
    # optimizer = opt("chain",
    #   optimizers = rep(list(opt("random_search", batch_size = 5000L), opt("nloptr", algorithm = "NLOPT_LD_LBFGS", approximate_eval_grad_f = TRUE)), times = 5L),
    #   terminators = rep(list(trm("evals", n_evals = 5000L), trm("combo", terminators = list(trm("evals", n_evals = 1000L), trm("stagnation", iters = 100L, threshold = 1e-12)))), times = 5L))
    # cb = callback_batch("start_values",
    #   on_optimization_begin = function(callback, context) {
    #   if (class(context$optimizer)[1L] == "OptimizerBatchNLoptr") {
    #     start = unlist(context$result[, context$instance$archive$cols_x, with = FALSE])  # previous random search
    #     context$optimizer$param_set$values$start_values = "custom"
    #     context$optimizer$param_set$values$start = start
    #   }
    # })
    # acq_optimizer = AcqOptimizer$new(optimizer, terminator = trm("evals", n_evals = 30000L), callbacks = list(cb))
    # acq_optimizer
    acq_optimizer = AcqOptimizerLbfgsb$new()
    acq_optimizer$param_set$set_values(
       random_restart_size = 5000L,
       n_random_restarts = 5L,
       maxeval = 1000,
       ftol_rel = 1e-4 # delta_f / f < 1e-4
    )
    acq_optimizer
  }
  #acq_optimizer$param_set$values$catch_errors = FALSE
  acq_optimizer
}

