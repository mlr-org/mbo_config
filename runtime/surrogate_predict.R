set.seed(7832)

library(bbotk)
library(mlr3mbo)

#lgr::get_logger("bbotk")$set_threshold("warn")

surrogate = "rf_var_jk_500" # "rf_var_jk_10", "rf_var_s_10", "rf_var_jk_500", "rf_var_s_500"

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

surrogate_learner = switch(surrogate,
  rf_var_jk_10 = {
  learner = LearnerRegrRangerMbo$new()
    learner$predict_type = "se"
    learner$param_set$values$keep.inbag = TRUE
    learner$param_set$values$splitrule = "variance"
    learner$param_set$values$se.method = "jack"
    learner$param_set$values$num.trees = 10L
    learner$param_set$values$sample.fraction = 1
    learner$param_set$values$min.node.size = 3
    learner$param_set$values$min.bucket = 3
    learner$param_set$values$mtry.ratio = 5/6
    learner
  },
  rf_var_s_10 = {
    learner = LearnerRegrRangerMbo$new()
    learner$predict_type = "se"
    learner$param_set$values$keep.inbag = TRUE
    learner$param_set$values$splitrule = "variance"
    learner$param_set$values$se.method = "simple"
    learner$param_set$values$num.trees = 10L
    learner$param_set$values$sample.fraction = 1
    learner$param_set$values$min.node.size = 3
    learner$param_set$values$min.bucket = 3
    learner$param_set$values$mtry.ratio = 5/6
    learner
  },
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

newdata = generate_design_random(instance$search_space, n = size)$data

system.time(surrogate$predict(newdata))

# milliseconds
#         surrogate  size                                  renv_project median_runtime mad_runtime mean_runtime  sd_runtime  timestamp
#            <char> <num>                                        <char>          <num>       <num>        <num>       <num>      <num>
#  1:  rf_var_jk_10     1 mlr3mbo/default/snapshots/snapshot_2025_07_04           10.0      1.4826         10.8    2.658320 1751654446
#  2:  rf_var_jk_10    10 mlr3mbo/default/snapshots/snapshot_2025_07_04            9.0      1.4826         10.0    2.708013 1751654446
#  3:  rf_var_jk_10   100 mlr3mbo/default/snapshots/snapshot_2025_07_04           13.5      4.4478         13.0    3.527668 1751654446
#  4:  rf_var_jk_10  1000 mlr3mbo/default/snapshots/snapshot_2025_07_04           11.5      1.4826         11.5    1.354006 1751654446
#  5: rf_var_jk_500     1 mlr3mbo/default/snapshots/snapshot_2025_07_04           12.0      2.2239         12.9    2.766867 1751654446
#  6: rf_var_jk_500    10 mlr3mbo/default/snapshots/snapshot_2025_07_04           12.0      2.2239         12.7    2.540779 1751654446
#  7: rf_var_jk_500   100 mlr3mbo/default/snapshots/snapshot_2025_07_04           16.5      2.2239         16.3    1.494434 1751654446
#  8: rf_var_jk_500  1000 mlr3mbo/default/snapshots/snapshot_2025_07_04           51.0     18.5325         52.9   12.169634 1751654446
#  9:   rf_var_s_10     1 mlr3mbo/default/snapshots/snapshot_2025_07_04           10.0      1.4826         11.0    3.091206 1751654446
# 10:   rf_var_s_10    10 mlr3mbo/default/snapshots/snapshot_2025_07_04           11.0      1.4826         11.7    3.267687 1751654446
# 11:   rf_var_s_10   100 mlr3mbo/default/snapshots/snapshot_2025_07_04           19.0      1.4826         21.1    6.674162 1751654446
# 12:   rf_var_s_10  1000 mlr3mbo/default/snapshots/snapshot_2025_07_04          112.0     25.9455        138.5   53.175287 1751654446
# 13:  rf_var_s_500     1 mlr3mbo/default/snapshots/snapshot_2025_07_04           17.0      3.7065         18.3    3.743142 1751654446
# 14:  rf_var_s_500    10 mlr3mbo/default/snapshots/snapshot_2025_07_04           70.0     30.3933         72.0   22.385511 1751654446
# 15:  rf_var_s_500   100 mlr3mbo/default/snapshots/snapshot_2025_07_04          449.5     48.1845        496.1   96.887621 1751654446
# 16:  rf_var_s_500  1000 mlr3mbo/default/snapshots/snapshot_2025_07_04         5249.5    652.3440       5612.8 1311.662118 1751654446