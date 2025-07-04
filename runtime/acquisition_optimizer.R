set.seed(7832)

library(bbotk)
library(mlr3mbo)
library(mlr3misc)

#lgr::get_logger("mlr3/bbotk")$set_threshold("warn")

acquisition_optimizer = "local_search" # "random_search", "local_search", "focus_search", "direct", "cmaes"

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

optimizer = switch(acquisition_optimizer,
  random_search = opt("random_search", batch_size = 10000L),
  local_search = opt("local_search", n_initial_points = 10L, initial_random_sample_size = 100L, neighbors_per_point = 100L),
  focus_search = opt("focus_search", maxit = 9, n_points = 1000L),
  direct = opt("nloptr", algorithm = "NLOPT_GN_DIRECT_L", xtol_rel = -1, xtol_abs = -1, ftol_rel = -1, ftol_abs = -1),
  cmaes = opt("cmaes")
)

acq_optimizer = acqo(optimizer, terminator = trm("evals", n_evals = 10000L), acq_function = acq_function, callbacks = callback_acq_optimizer_surrogate)

# runtime of the acquisition optimizer
runtime = system.time(acq_optimizer$optimize())

# runtime of the surrogate predict
surrogate_predict = sum(acq_optimizer$callbacks$mlr3mbo.acq_optimizer_surrogate_time$state$time)

list(
  elapsed = runtime["elapsed"],
  surrogate_predict = surrogate_predict
)

#     acquisition_optimizer                                  renv_project mean_runtime  sd_runtime median_runtime mad_runtime mean_runtime_surrogate_predict sd_runtime_surrogate_predict median_runtime_surrogate_predict mad_runtime_surrogate_predict
#                    <char>                                        <char>        <num>       <num>          <num>       <num>                          <num>                        <num>                            <num>                         <num>
#  1:                 cmaes mlr3mbo/default/snapshots/snapshot_2025_06_03     194192.7 17118.39281       189235.5   9572.4069                        12384.6                  962.3539658                          12052.0                      527.0643
#  2:                 cmaes mlr3mbo/default/snapshots/snapshot_2025_07_04     259131.3 23522.83913       247133.0   7851.8496                        12849.2                 1059.8019521                          12418.0                      638.2593
#  3:                direct mlr3mbo/default/snapshots/snapshot_2025_06_03     200308.9 23456.43051       188433.5   3647.9373                        12821.2                 1490.3714526                          12048.5                      204.5988
#  4:                direct mlr3mbo/default/snapshots/snapshot_2025_07_04     262511.3 11567.71635       266929.0   7168.3710                        13032.5                  600.9953318                          13287.0                      377.3217
#  5:          focus_search mlr3mbo/default/snapshots/snapshot_2025_06_03        980.2   100.51291          928.5     54.1149                           12.1                    1.1972190                             12.0                        1.4826
#  6:          focus_search mlr3mbo/default/snapshots/snapshot_2025_07_04       1510.8   214.42834         1557.0    258.7137                           14.5                    1.7159384                             15.0                        1.4826
#  7:          local_search mlr3mbo/default/snapshots/snapshot_2025_06_03      10718.1   478.98630        10632.5    580.4379                           14.0                    0.9428090                             14.0                        0.7413
#  8:          local_search mlr3mbo/default/snapshots/snapshot_2025_07_04      11205.1   692.19497        11152.5    392.8890                           14.9                    1.9692074                             15.0                        0.7413
#  9:         random_search mlr3mbo/default/snapshots/snapshot_2025_06_03        599.9    99.15134          582.5    119.3493                            1.9                    0.3162278                              2.0                        0.0000
# 10:         random_search mlr3mbo/default/snapshots/snapshot_2025_07_04        631.7   140.60745          604.5    156.4143                            2.4                    0.6992059                              2.0                        0.0000