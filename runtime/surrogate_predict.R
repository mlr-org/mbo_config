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

