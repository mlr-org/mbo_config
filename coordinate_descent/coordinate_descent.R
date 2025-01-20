library(batchtools)
library(mlr3misc)
library(data.table)
library(paradox)
library(bbotk)
library(reticulate)
library(yahpogym)

use_condaenv("yahpo_gym", required=TRUE)
yahpo_gym = import("yahpo_gym")

source("OptimizerCoordinateDescent.R")

unlink("/gscratch/mbecke16/mbo_config/registry_coordinate_descent", recursive = TRUE)

reg = makeExperimentRegistry(
  file.dir = "/gscratch/mbecke16/mbo_config/registry_coordinate_descent",
  conf.file = "/home/mbecke16/mbo_config/coordinate_descent/batchtools.conf.R",
)

# unlink("registry_coordinate_descent", recursive = TRUE)

# reg = makeExperimentRegistry(
#   file.dir = "registry_coordinate_descent",
#   conf.file = NA,
# )

reg = loadRegistry(
  file.dir = "/gscratch/mbecke16/mbo_config/registry_coordinate_descent",
  conf.file = "/home/mbecke16/mbo_config/coordinate_descent/batchtools.conf.R",
  writeable = TRUE
)

set.seed(7832)

# add problems
## yahpo
loader_yahpo = function(scenario, instance, target, budget) {
  library(reticulate)
  library(yahpogym)
  library(bbotk)

  use_condaenv("yahpo_gym", required = TRUE)
  yahpo_gym = import("yahpo_gym")

  benchmark = BenchmarkSet$new(scenario)
  benchmark$subset_codomain(target)
  objective = benchmark$get_objective(instance, multifidelity = FALSE)

  oi(
    objective,
    search_space = benchmark$get_search_space(drop_fidelity_params = TRUE),
    terminator = trm("evals", n_evals = budget),
    check_values = FALSE)
}

instances = fread("random_search/instances.csv")
instances[, instance := as.character(instance)]
rbv2 = instances[grep("rbv2", scenario)]
lcbench = instances[grep("lcbench", scenario)]

#rbv2 = rbv2[, .SD[sample(.N, min(.N, 16))], by = scenario]

pwalk(rbv2, function(scenario, instance) {
  walk(c("acc", "bac", "auc", "logloss"), function(target) {
    addProblem(
      name = sprintf("%s_%s_%s", scenario, instance, target),
      data = list(
        loader = loader_yahpo,
        args = list(scenario = scenario, instance = instance, target = target, budget = 200)
      )
    )
  })
})

#lcbench = lcbench[, .SD[sample(.N, min(.N, 16))], by = scenario]

pwalk(lcbench, function(scenario, instance) {
  walk(c("val_accuracy", "val_balanced_accuracy", "val_cross_entropy"), function(target) {
    addProblem(
      name = sprintf("%s_%s_%s", scenario, instance, target),
      data = list(
        loader = loader_yahpo,
        args = list(scenario = scenario, instance = instance, target = target, budget = 200)
      )
    )
  })
})

# add algorithms
search_space = ps(
  log_scale = p_lgl(),
  init = p_fct(c("random", "lhs", "sobol")),
  init_size_fraction = p_fct(c("0.05", "0.10", "0.25")),
  random_interleave_iter = p_fct(c("0", "2", "5", "10")),
  rf_type = p_fct(c("standard", "extratrees", "smaclike_simple", "smaclike_law_of_total_variance")),
  acqf = p_fct(c("EI", "CB", "PI", "Mean")),
  lambda = p_fct(c("1", "3", "10"), depends = acqf == "CB"),
  acqopt = p_fct(c("RS_1000", "RS", "FS", "LS")),
  epsilon_decay = p_lgl(depends = acqf == "EI"),
  lambda_decay = p_lgl(depends = acqf == "CB")
)

# NOTE: other surrogates

addAlgorithm(
  name = "mbo",
  fun = function(
    data,
    job,
    instance,
    log_scale,
    init,
    init_size_fraction,
    random_interleave_iter,
    rf_type,
    acqf,
    lambda,
    acqopt,
    epsilon_decay,
    lambda_decay,
    id,
    config_hash
    ) {

    renv::load(".")

    library(batchtools)
    library(mlr3misc)
    library(data.table)
    library(paradox)
    library(bbotk)
    library(mlr3learners)
    library(mlr3mbo)
    library(mlr3pipelines)

    random_interleave_iter = as.numeric(random_interleave_iter)
    init_size_fraction = as.numeric(init_size_fraction)
    lambda = as.numeric(lambda)

    optim_instance = invoke(data$loader, .args = data$args)

    init_design_size = ceiling(as.numeric(init_size_fraction) * data$args$budget)
    init_design = if (init == "random") {
      generate_design_random(optim_instance$search_space, n = init_design_size)$data
    } else if (init == "lhs") {
      generate_design_lhs(optim_instance$search_space, n = init_design_size)$data
    } else if (init == "sobol") {
      generate_design_sobol(optim_instance$search_space, n = init_design_size)$data
    }

    optim_instance$eval_batch(init_design)

    learner = LearnerRegrRangerMbo$new()
    learner$predict_type = "se"
    learner$param_set$values$keep.inbag = TRUE

    if (rf_type == "standard") {
      learner$param_set$values$se.method = "jack"
      learner$param_set$values$splitrule = "variance"
      learner$param_set$values$num.trees = 1000L
    } else if (rf_type == "extratrees") {
      learner$param_set$values$se.method = "jack"
      learner$param_set$values$splitrule = "extratrees"
      learner$param_set$values$num.random.splits = 1L
      learner$param_set$values$num.trees = 1000L
    } else if (rf_type == "smaclike_simple") {
      learner$param_set$values$se.method = "simple"
      learner$param_set$values$splitrule = "variance"
      learner$param_set$values$num.trees = 10L
      learner$param_set$values$replace = TRUE
      learner$param_set$values$sample.fraction = 1
      learner$param_set$values$min.node.size = 3
      learner$param_set$values$min.bucket = 3
      learner$param_set$values$mtry.ratio = 5/6
    } else if (rf_type == "smaclike_law_of_total_variance") {
      learner$param_set$values$se.method = "law_of_total_variance"
      learner$param_set$values$splitrule = "variance"
      learner$param_set$values$num.trees = 10L
      learner$param_set$values$replace = TRUE
      learner$param_set$values$sample.fraction = 1
      learner$param_set$values$min.node.size = 3
      learner$param_set$values$min.bucket = 3
      learner$param_set$values$mtry.ratio = 5/6
    }

    surrogate = SurrogateLearner$new(GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>%
      po("imputeoor", multiplier = 3, affect_columns = selector_type(c("integer", "numeric", "character", "factor", "ordered"))) %>>%
      po("colapply", applicator = as.factor, affect_columns = selector_type("character")) %>>%
      learner))
    surrogate$param_set$values$catch_errors = TRUE

    acq_optimizer = if (acqopt == "RS_1000") {
      AcqOptimizer$new(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 1000L))
    } else if (acqopt == "RS") {
      AcqOptimizer$new(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 20000L))
    } else if (acqopt == "FS") {
      n_repeats = 2L
      maxit = 9L
      batch_size = ceiling((20000L / n_repeats) / (1 + maxit)) # 1000L
      AcqOptimizer$new(opt("focus_search", n_points = batch_size, maxit = maxit), terminator = trm("evals", n_evals = 20000L))
    } else if (acqopt == "LS") {
      acq_optimizer = AcqOptimizer$new(opt("local_search", n_initial_points = 10L, initial_random_sample_size = 20000L), terminator = trm("evals", n_evals = 30000L))
      acq_optimizer
    }
    acq_optimizer$param_set$values$catch_errors = FALSE

    acq_function = if (acqf == "EI" && log_scale) {
      AcqFunctionLogEI$new()
    } else if (acqf == "EI" && !log_scale) {
      AcqFunctionEI$new()
    } else if (acqf == "CB") {
      AcqFunctionCB$new(lambda = as.numeric(lambda))
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

    if (!log_scale) {
      bayesopt_ego(
        optim_instance,
        surrogate = surrogate,
        acq_function = acq_function,
        acq_optimizer = acq_optimizer,
        random_interleave_iter = random_interleave_iter,
        init_design_size = init_design_size)
    } else {
      bayesopt_ego_log(
        optim_instance,
        surrogate = surrogate,
        acq_function = acq_function,
        acq_optimizer = acq_optimizer,
        random_interleave_iter = random_interleave_iter,
        init_design_size = init_design_size)
    }

    score = optim_instance$archive$best()[[job$problem$data$args$target]]

    data.table(
      id = id,
      replication = job$repl,
      problem = job$problem$name,
      instance = job$problem$data$args$instance,
      scenario = job$problem$data$args$scenario,
      target = job$problem$data$args$target,
      score = score
    )
  }
)

init = data.table(
  log_scale = FALSE,
  init = "random",
  init_size_fraction = "0.25",
  random_interleave_iter = "0",
  rf_type = "standard",
  acqf = "EI",
  lambda = NA_character_,
  acqopt = "RS_1000",
  epsilon_decay = FALSE,
  lambda_decay = NA)

constants = ps(
  reg = p_uty(),
  rs_result = p_uty(),
  rs_result_200 = p_uty()
)

objective = ObjectiveRFunDt$new(
  fun = function(
    xdt,
    reg,
    rs_result,
    rs_result_200
    ) {
    n_repls = 1
    xdt[, id := .I]
    set(xdt, j = "config_hash", value = uuid::UUIDgenerate(n = nrow(xdt))) # make experiments unique to avoid skipping

    ades = list(
      mbo = xdt
    )
    ids = addExperiments(algo.designs = ades, repls = n_repls, reg = reg)

    ids[, chunk := batchtools::chunk(job.id, chunk.size = 96, shuffle = FALSE)]

    job_ids = submitJobs(ids = ids, reg = reg)$job.id
    waitForJobs(ids = job_ids, reg = reg)

    while(TRUE) {
      if (length(findExpired()$job.id)) {
        message("Resubmitting expired jobs")
        expired_ids = findExpired()
        expired_ids[, chunk := batchtools::chunk(job.id, chunk.size = 96, shuffle = FALSE)]
        resubmitted_ids = submitJobs(ids = expired_ids, reg = reg)
        waitForJobs(ids = resubmitted_ids, reg = reg)
      } else {
        break
      }
    }

    res = rbindlist(reduceResultsList(ids = intersect(job_ids, findDone()$job.id), reg = reg))

    # average best over replications
    agg = res[, list(
      mean_score = mean(score),
      raw_score = list(score),
      n_na = sum(is.na(score)),
      n = .N,
      target = target), by = list(id, problem)]

    # determine k
    ks = pmap_dbl(agg, function(problem, mean_score, ...) {
      .problem = problem
      score_min = rs_result_200[list(.problem), mean_score, on = "problem"]
      score_max = rs_result[list(.problem), score, on = "problem"]

      (score_min - mean_score) / (score_min - score_max)
    })
    set(agg, j = "k", value = ks)

    # average k over problems
    agg_k = agg[, .(mean_k = mean(k), raw_k = list(k), n_na = sum(is.na(k)), n = .N, raw_mean_score = list(mean_score)), by = .(id)]
    # if no k on all instances, set to -Inf
    agg_k[n < length(reg$problems), mean_k := -Inf]
    agg_k
  },
  domain = search_space,
  codomain = ps(mean_k = p_dbl(tags = "maximize")),
  constants = constants,
  check_values = FALSE
)

objective$constants$set_values(
  reg = reg,
  rs_result = fread("random_search/random_search_results.csv"),
  rs_result_200 = fread("random_search/random_search_200_results.csv")
)

callback_backup = callback_batch("bbotk.backup",
  label = "Backup Archive Callback",
  man = "bbotk::bbotk.backup",

  on_optimizer_after_eval = function(callback, context) {
    start_time = Sys.time()
    tmp_file = tempfile(tmpdir = dirname(callback$state$path), fileext = ".rds")
    saveRDS(context$instance$archive$data, tmp_file)
    unlink(callback$state$path)
    file.rename(tmp_file, callback$state$path)
    message(sprintf("Saving intermediate results took %s seconds", difftime(Sys.time(), start_time, units = "s")))
  }
)

callback_backup$state$path = "/gscratch/mbecke16/mbo_config/intermediate_instance.rds"

optim_instance = oi(
  objective = objective,
  terminator = trm("none"),
  search_space = search_space,
  check_values = FALSE,
  callbacks = list(callback_backup)
)

if (file.exists(callback_backup$state$path)) {
  data = readRDS(callback_backup$state$path)
  optim_instance$archive$data = data
} else {
  optim_instance$eval_batch(init)
}

optimizer = OptimizerBatchCoordinateDescent$new()
optimizer$optimize(optim_instance)

saveRDS(optim_instance, "/gscratch/mbecke16/mbo_config/coordinate_descent.rds")
