# Script to measure the runtime of the acquisition optimizer
# The

library(bbotk)
library(mlr3mbo)
library(mlr3misc)

lgr::get_logger("mlr3/bbotk")$set_threshold("warn")

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

codomain = ps(y = p_dbl(tags = "minimize"))
objective = bbotk::ObjectiveRFunMany$new(fun = fun, domain = search_space, codomain = codomain, properties = "single-crit")
instance = oi(objective, terminator = trm("evals", n_evals = 100L))
initial_design = generate_design_random(instance$search_space, n = 10L)$data
instance$eval_batch(initial_design)
surrogate = srlrn(lrn("regr.featureless"), archive = instance$archive)
surrogate$update()
acq_function = acqf("mean", surrogate = surrogate)
acq_function$update()

callback_acq_optimizer_surrogate = callback_batch("mlr3mbo.acq_optimizer_surrogate_time",
  on_optimizer_after_eval = function(callback, context) {
    callback$state$time = c(callback$state$time, context$instance$objective$surrogate$learner$timings["predict"])
  }
)

local_search = opt("local_search", n_initial_points = 10L, initial_random_sample_size = 100L, neighbors_per_point = 90L)
acq_optimizer = acqo(local_search, terminator = trm("evals", n_evals = 10000L), acq_function = acq_function, callbacks = callback_acq_optimizer_surrogate)

random_search = opt("random_search", batch_size = 10000L)
acq_optimizer = acqo(random_search, terminator = trm("evals", n_evals = 10000L), acq_function = acq_function, callbacks = callback_acq_optimizer_surrogate)

# runtime of the acquisition optimizer
system.time(acq_optimizer$optimize())

# runtime of the surrogate predict
sum(acq_optimizer$callbacks$mlr3mbo.acq_optimizer_surrogate_time$state$time)
