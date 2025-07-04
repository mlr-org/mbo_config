set.seed(7832)

library(bbotk)
library(mlr3mbo)
library(mlr3misc)

# lgr::get_logger("bbotk")$set_threshold("warn")

acquisition_optimizer = "random_search"
surrogate = "rf_var_jk_500"
acq_function = "ei"

search_space = ps(
  x_1 = p_dbl(lower = -1, upper = 1),
  x_2 = p_dbl(lower = -1, upper = 1),
  x_3 = p_dbl(lower = -1, upper = 1),
  x_4 = p_dbl(lower = -1, upper = 1),
  x_5 = p_dbl(lower = -1, upper = 1)
)

fun = function(xss) {
  res = map_dbl(xss, function(xs) xs[[1]]^2 + xs[[2]]^2 + xs[[3]]^2 + xs[[4]]^2 + xs[[5]]^2)
  data.table(y = res)
}

# callback to measure surrogate train time
callback_surrogate = callback_batch("mlr3mbo.surrogate_time",
  on_optimizer_after_eval = function(callback, context) {
    callback$state$surrogate_train = c(callback$state$surrogate_train, context$optimizer$surrogate$learner$timings["train"])
  }
)

codomain = ps(y = p_dbl(tags = "minimize"))
objective = bbotk::ObjectiveRFunMany$new(fun = fun, domain = search_space, codomain = codomain, properties = "single-crit")
instance = oi(objective, terminator = trm("evals", n_evals = 20L), callbacks = list(callback_surrogate))
initial_design = generate_design_random(instance$search_space, n = 10L)$data
instance$eval_batch(initial_design)


surrogate_learner = switch(surrogate,
  rf_var_jk_500 = {
    learner = LearnerRegrRangerMbo$new()
    learner$predict_type = "se"
    learner$param_set$values$keep.inbag = TRUE
    learner$param_set$values$splitrule = "variance"
    learner$param_set$values$se.method = "jack"
    learner$param_set$values$num.trees = 500L
    learner$param_set$values$sample.fraction = 1
    learner$param_set$values$min.node.size = 3
    learner$param_set$values$min.bucket = 3
    learner$param_set$values$mtry.ratio = 5/6
    learner
  },
  rf_var_s_500 = {
    learner = LearnerRegrRangerMbo$new()
    learner$predict_type = "se"
    learner$param_set$values$keep.inbag = TRUE
    learner$param_set$values$splitrule = "variance"
    learner$param_set$values$se.method = "simple"
    learner$param_set$values$num.trees = 500L
    learner$param_set$values$sample.fraction = 1
    learner$param_set$values$min.node.size = 3
    learner$param_set$values$min.bucket = 3
    learner$param_set$values$mtry.ratio = 5/6
    learner
  })
surrogate = srlrn(surrogate_learner, archive = instance$archive, catch_errors = FALSE)
surrogate$update()

acq_function = switch(acq_function,
  ei = acqf("ei", surrogate = surrogate),
  cb = acqf("cb", lambda = 1, surrogate = surrogate)
)
acq_function$update()

optimizer = switch(acquisition_optimizer,
  random_search = opt("random_search", batch_size = 10000L),
  local_search = opt("local_search", n_initial_points = 10L, initial_random_sample_size = 100L, neighbors_per_point = 100L),
  direct = opt("nloptr", algorithm = "NLOPT_GN_DIRECT_L", xtol_rel = -1, xtol_abs = -1, ftol_rel = -1, ftol_abs = -1)
)
callback_acq_optimizer = callback_batch("mlr3mbo.acq_optimizer_time",
  on_optimization_begin = function(callback, context) {
    callback$state$begin = c(callback$state$begin, Sys.time())
  },
  on_optimization_end = function(callback, context) {
    callback$state$end = c(callback$state$end, Sys.time())
  }
)
callback_acq_optimizer_surrogate = callback_batch("mlr3mbo.acq_optimizer_surrogate_time",
  on_optimizer_after_eval = function(callback, context) {
  callback$state$time = c(callback$state$time, context$instance$objective$surrogate$learner$timings["predict"])
  }
)
acq_optimizer = acqo(optimizer, terminator = trm("evals", n_evals = 10000L), acq_function = acq_function, callbacks = list(callback_acq_optimizer_surrogate, callback_acq_optimizer))

optimizer = opt("mbo", loop_function = bayesopt_ego, surrogate = surrogate, acq_function = acq_function, acq_optimizer = acq_optimizer)

runtime = system.time({optimizer$optimize(instance)})

surrogate_train_runtime = sum(unlist(callback_surrogate$state$surrogate_train))
surrogate_predict_runtime = sum(unlist(callback_acq_optimizer_surrogate$state$time))
acq_optimizer_runtime = sum(unlist(callback_acq_optimizer$state$end) - unlist(callback_acq_optimizer$state$begin))

list(
  elapsed = runtime["elapsed"],
  surrogate_train = surrogate_train_runtime,
  acq_optimizer = acq_optimizer_runtime,
  surrogate_predict = surrogate_predict_runtime)