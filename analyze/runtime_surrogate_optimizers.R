pak::pak("mlr-org/mlr3mbo@so_config_3")
library(mlr3mbo)
library(mlr3misc)
library(mlr3pipelines)
library(microbenchmark)
library(bbotk)

source("helper.R")

task = tsk("california_housing")
task$filter(sample(task$nrow, 200))

surrogate_ids = c("rf_var_jk_10", "rf_var_s_10", "rf_var_ltv_10", "rf_var_jk_500", "rf_var_s_500", "rf_var_ltv_500")

surrogates = set_names(map(surrogate_ids, get_surrogate_mixed_deps), surrogate_ids)

microbenchmark(
  rf_var_jk_10 = surrogates[["rf_var_jk_10"]]$learner$train(task),
  rf_var_s_10 = surrogates[["rf_var_s_10"]]$learner$train(task),
  rf_var_ltv_10 = surrogates[["rf_var_ltv_10"]]$learner$train(task),
  rf_var_jk_500 = surrogates[["rf_var_jk_500"]]$learner$train(task),
  rf_var_s_500 = surrogates[["rf_var_s_500"]]$learner$train(task),
  rf_var_ltv_500 = surrogates[["rf_var_ltv_500"]]$learner$train(task),
  times = 1,
  unit = "s",

)

# Unit: seconds
#            expr       min        lq      mean    median        uq       max neval
#    rf_var_jk_10 0.2121403 0.2121403 0.2121403 0.2121403 0.2121403 0.2121403     1
#     rf_var_s_10 0.2354600 0.2354600 0.2354600 0.2354600 0.2354600 0.2354600     1
#   rf_var_ltv_10 0.2303939 0.2303939 0.2303939 0.2303939 0.2303939 0.2303939     1
#   rf_var_jk_500 0.2943027 0.2943027 0.2943027 0.2943027 0.2943027 0.2943027     1
#    rf_var_s_500 0.5857421 0.5857421 0.5857421 0.5857421 0.5857421 0.5857421     1
#  rf_var_ltv_500 0.5669015 0.5669015 0.5669015 0.5669015 0.5669015 0.5669015     1

# 20k rows
task = tsk("california_housing")

microbenchmark(
  rf_var_jk_10 = surrogates[["rf_var_jk_10"]]$learner$predict(task),
  rf_var_s_10 = surrogates[["rf_var_s_10"]]$learner$predict(task),
  rf_var_ltv_10 = surrogates[["rf_var_ltv_10"]]$learner$predict(task),
  rf_var_jk_500 = surrogates[["rf_var_jk_500"]]$learner$predict(task),
  rf_var_s_500 = surrogates[["rf_var_s_500"]]$learner$predict(task),
  rf_var_ltv_500 = surrogates[["rf_var_ltv_500"]]$learner$predict(task),
  times = 1,
  unit = "s"
)

# Unit: seconds
#            expr       min        lq      mean    median        uq       max neval
#    rf_var_jk_10  14.70026  14.70026  14.70026  14.70026  14.70026  14.70026     1
#     rf_var_s_10  16.77638  16.77638  16.77638  16.77638  16.77638  16.77638     1
#   rf_var_ltv_10  16.55996  16.55996  16.55996  16.55996  16.55996  16.55996     1
#   rf_var_jk_500 166.84375 166.84375 166.84375 166.84375 166.84375 166.84375     1
#    rf_var_s_500 705.55228 705.55228 705.55228 705.55228 705.55228 705.55228     1
#  rf_var_ltv_500 814.47199 814.47199 814.47199 814.47199 814.47199 814.47199     1


optimizers_ids = c("RS_1000", "RS", "FS", "LS")

optimizers = set_names(map(optimizers_ids, get_acq_optimizer_mixed_deps), optimizers_ids)


PS_1D = ps(
  x = p_dbl(lower = -1, upper = 1)
  )

FUN_1D = function(xs) {
  list(y = as.numeric(xs)^2)
}
FUN_1D_CODOMAIN = ps(y = p_dbl(tags = "minimize"))
OBJ_1D = bbotk::ObjectiveRFun$new(fun = FUN_1D, domain = PS_1D, codomain = FUN_1D_CODOMAIN, properties = "single-crit")

optimizers = set_names(map(optimizers_ids, function(id) {
  instance = oi(OBJ_1D, terminator = trm("evals", n_evals = 5L))
  design = generate_design_grid(instance$search_space, resolution = 4L)$data
  instance$eval_batch(design)
  acqfun = AcqFunctionEI$new(SurrogateLearner$new(surrogates[["rf_var_jk_10"]]$learner, archive = instance$archive))
  acqopt = get_acq_optimizer_mixed_deps(id)
  acqopt$acq_function = acqfun
  acqfun$surrogate$update()
  acqfun$update()
  acqopt
}), optimizers_ids)

microbenchmark(
  RS_1000 = optimizers[["RS_1000"]]$optimize(),
  RS = optimizers[["RS"]]$optimize(),
  FS = optimizers[["FS"]]$optimize(),
  LS = optimizers[["LS"]]$optimize(),
  times = 1,
  unit = "s"
)

# Unit: seconds
#     expr        min         lq       mean     median         uq        max neval
#  RS_1000  0.1852757  0.1852757  0.1852757  0.1852757  0.1852757  0.1852757     1
#       RS  0.9236741  0.9236741  0.9236741  0.9236741  0.9236741  0.9236741     1
#       FS  6.0085529  6.0085529  6.0085529  6.0085529  6.0085529  6.0085529     1
#       LS 26.3838500 26.3838500 26.3838500 26.3838500 26.3838500 26.3838500     1