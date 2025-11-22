renv::load(".")

packages = c(
  "data.table",
  "mlr3",
  "mlr3learners",
  "mlr3misc",
  "mlr3mbo",
  "mlr3pipelines",
  "bbotk",
  "paradox",
  "ranger",
  "R6",
  "checkmate",
  "yahpogym",
  "renv")

lapply(packages, library, character.only = TRUE)

mixed_objective = function(
  scenario,
  instance,
  target_variable,
  budget,
  input_trafo,
  output_trafo,
  init,
  init_size_fraction,
  random_interleave_iter,
  trees,
  variance_estimator,
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
  assert_int(trees, na.ok = TRUE)
  assert_string(variance_estimator, na.ok = TRUE)
  assert_string(acqf)
  assert_number(lambda, na.ok = TRUE)
  assert_string(acqopt)
  assert_flag(epsilon_decay, na.ok = TRUE)
  assert_flag(lambda_decay, na.ok = TRUE)

  reticulate::use_condaenv("/home/marc/miniconda/envs/yahpo_gym", required = TRUE)
  data.table::setDTthreads(1L)
  future::plan("sequential")

  # create optim instance from yahpo gym problem
  benchmark = BenchmarkSet$new(scenario, instance = instance)
  benchmark$subset_codomain(target_variable)
  objective = benchmark$get_objective(instance, multifidelity = FALSE)
  search_space = benchmark$get_search_space(drop_fidelity_params = TRUE)
  optim_instance = oi(objective, search_space = search_space, terminator = trm("evals", n_evals = budget), check_values = TRUE)

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
  learner = lrn("regr.ranger",
    num.trees = trees,
    se.method = variance_estimator,
    splitrule = "variance",
    predict_type = "se",
    keep.inbag = TRUE,
    sample.fraction = 1,
    min.node.size = 3,
    min.bucket = 3,
    mtry.ratio = 5/6
  )
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

xs = list(
  input_trafo = "none",
  output_trafo = "none",
  init = "random",
  init_size_fraction = 0.25,
  random_interleave_iter = 0,
  trees = 500,
  variance_estimator = "jack",
  acqf = "CB",
  lambda = 1,
  acqopt = "LS",
  epsilon_decay = NA,
  lambda_decay = FALSE
)

optim_instance = invoke(mixed_objective,
  scenario = "rbv2_xgboost",
  instance = "12",
  target_variable = "acc",
  budget = 50,
  .args = xs)

optim_instance$archive$data

# optim_instance$archive$data[170,]

# data = optim_instance$archive$data

# data[booster == "dart"]


# fwrite(data[, 1:15], "mlr3mbo/results/rbv2_xgboost_mfeat-factors_12_accuracy_archive.csv")


# ##############

# library(bbotk)

# benchmark = BenchmarkSet$new("rbv2_xgboost", instance = "12")
# objective = benchmark$get_objective(instance = "12", multifidelity = FALSE)
# search_space = benchmark$get_search_space(drop_fidelity_params = TRUE)

# objective$eval(list(
#   booster = "dart"
# ))

# get_private(objective)$.fun

# instance = oi(
#   objective = objective,
#   search_space = search_space,
#   terminator = trm("evals", n_evals = 1),
#   check_values = TRUE,
# )

# domain = objective$domain


# xs2 = xs
# xs2$nrounds = NA
# domain$check(xs2)

# domain$deps[,]$cond

# search_space$check(xs)

# xs = data[170, x_domain][[1]]

# objective$eval(data[170, x_domain][[1]])

#   search_space$deps[,]$cond


# xs3  = xs

# xs3$colsample_bytree = 1
# xs3$colsample_bylevel = 1

# objective$eval(xs3)


# tail(data[, 1:15])


# generate_design_random(search_space, n = 10)$data

# xdt = data[170, 1:14]
# xdt$booster = "gbtree"

# search_space$check_dt(xdt)


# ss_2 = ps(
#   x0 = p_fct(c("a", "b")),
#   x1 = p_dbl(lower = 0, upper = 1, depends = x0 == "a"),
#   x2 = p_dbl(lower = 0, upper = 1)
# )

# ss_2$check_dt(data.table(x0 = "a"))

# ss_2$check(list(x0 = "a"))

# ss_2$assert(list(x0 = "a"))


# domain$check(xs)
