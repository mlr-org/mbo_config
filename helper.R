make_optim_instance = function(instance) {
  benchmark = BenchmarkSet$new(instance$scenario, instance = instance$instance)
  benchmark$subset_codomain(instance$target)
  objective = benchmark$get_objective(instance$instance, multifidelity = FALSE)
  search_space = benchmark$get_search_space(drop_fidelity_params = TRUE)
  if (instance$benchmark == "pure_numeric") {
    objective = fix_objective_domain_constants_pure_numeric(instance$scenario, objective=objective)
    search_space = get_search_space_pure_numeric(instance$scenario)
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
    params = setdiff(domain$ids(), "num.impute.selected.cpo")
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
    params = setdiff(domain$ids(), "num.impute.selected.cpo")
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
