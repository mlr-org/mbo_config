#Sys.setenv(DEBUGME = "batchtools")

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

# gscratch/mbecke16/mbo_config/registry_coordinate_descen

unlink("registry_coordinate_descent", recursive = TRUE)

reg = makeExperimentRegistry(
  file.dir = "registry_coordinate_descent",
  conf.file = "/home/mbecke16/mbo_config/batchtools.conf.R",
)

# add problems
## yahpo
loader_yahpo = function(scenario, instance, target, budget) {
  library(reticulate)
  library(yahpogym)

  use_condaenv("yahpo_gym", required = TRUE)
  yahpo_gym = import("yahpo_gym")

  benchmark = BenchmarkSet$new(scenario)
  benchmark$subset_codomain(target)
  objective = benchmark$get_objective(instance, multifidelity = FALSE)

  OptimInstanceSingleCrit$new(
    objective, 
    search_space = benchmark$get_search_space(drop_fidelity_params = TRUE), 
    terminator = trm("evals", n_evals = budget), 
    check_values = FALSE)
}

benchmarks = yahpogym::list_benchmarks()
scenarios_rbv2 = grep("^rbv2", names(benchmarks$configs), value = TRUE)

walk(scenarios_rbv2, function(scenario) {
  b = BenchmarkSet$new(scenario)
  walk(b$instances[1:5], function(instance) { #!!!!!!!!!!
    addProblem(
      name = sprintf("%s_%s", scenario, instance),
      data = list(
        loader = loader_yahpo,
        args = list(scenario = scenario, instance = instance, target = "acc", budget = 200)
      )
    )
  })
})

scenarios_lcbench = grep("^lcbench", names(benchmarks$configs), value = TRUE)

walk(scenarios_lcbench, function(scenario) {
  b = BenchmarkSet$new(scenario)
  walk(b$instances[1:5], function(instance) { #!!!!!!!!!!
    addProblem(
      name = sprintf("%s_%s", scenario, instance),
      data = list(
        loader = loader_yahpo,
        args = list(scenario = scenario, instance = instance, target = "val_accuracy", budget = 200)
      )
    )
  })
})

# add algorithms
addAlgorithm(
  name = "mbo",
  fun = function(
    data,
    job,
    instance,
    loop_function,
    init,
    init_size_fraction,
    random_interleave,
    random_interleave_iter,
    rf_type,
    acqf,
    acqf_ei_log,
    lambda,
    acqopt,
    id
    ) {

    message("Test")

    library(batchtools)
    library(mlr3misc)
    library(data.table)
    library(paradox)
    library(bbotk)
    library(mlr3learners)
    library(mlr3mbo)
    library(mlr3pipelines)

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

    random_interleave_iter = if (random_interleave) as.numeric(random_interleave_iter) else 0L

    learner = LearnerRegrRangerCustom$new()
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
    } else if (rf_type == "smaclike_boot") {
      learner$param_set$values$se.method = "simple"
      learner$param_set$values$splitrule = "extratrees"
      learner$param_set$values$num.random.splits = 1L
      learner$param_set$values$num.trees = 10L
      learner$param_set$values$replace = TRUE
      learner$param_set$values$sample.fraction = 1
      learner$param_set$values$min.node.size = 1
      learner$param_set$values$mtry.ratio = 1
    } else if (rf_type == "smaclike_no_boot") {
      learner$param_set$values$se.method = "simple"
      learner$param_set$values$splitrule = "extratrees"
      learner$param_set$values$num.random.splits = 1L
      learner$param_set$values$num.trees = 10L
      learner$param_set$values$replace = FALSE
      learner$param_set$values$sample.fraction = 1
      learner$param_set$values$min.node.size = 1
      learner$param_set$values$mtry.ratio = 1
    }

    surrogate = SurrogateLearner$new(GraphLearner$new(po("imputesample", affect_columns = selector_type("logical")) %>>%
      po("imputeoor", multiplier = 3, affect_columns = selector_type(c("integer", "numeric", "character", "factor", "ordered"))) %>>%
      po("colapply", applicator = as.factor, affect_columns = selector_type("character")) %>>%
      learner))
    surrogate$param_set$values$catch_errors = FALSE

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
      optimizer = OptimizerChain$new(list(opt("local_search", n_points = 100L), opt("random_search", batch_size = 1000L)), terminators = list(trm("evals", n_evals = 10000L), trm("evals", n_evals = 10000L)))
      acq_optimizer = AcqOptimizer$new(optimizer, terminator = trm("evals", n_evals = 20000L))
      acq_optimizer$param_set$values$warmstart = TRUE
      acq_optimizer$param_set$values$warmstart_size = "all"
      acq_optimizer
    }
    acq_optimizer$param_set$values$catch_errors = FALSE

    acq_function = if (acqf == "EI") {
      if (isTRUE(acqf_ei_log)) {
        AcqFunctionLogEI$new()
      } else {
        AcqFunctionEI$new()
      }
    } else if (acqf == "CB") {
      AcqFunctionCB$new(lambda = as.numeric(lambda))
    } else if (acqf == "PI") {
      AcqFunctionPI$new()
    } else if (acqf == "Mean") {
      AcqFunctionMean$new()
    }

    if (loop_function == "ego") {
      bayesopt_ego(
        optim_instance,
        surrogate = surrogate,
        acq_function = acq_function,
        acq_optimizer = acq_optimizer,
        random_interleave_iter = random_interleave_iter)
    } else if (loop_function == "ego_log") {
      bayesopt_ego_log(
        optim_instance,
        surrogate = surrogate,
        acq_function = acq_function,
        acq_optimizer = acq_optimizer,
        random_interleave_iter = random_interleave_iter)
    }

    target = optim_instance$archive$cols_y
    best = optim_instance$archive$best()[[target]]
    data.table(best = best, target = target, problem = job$prob.name, id = id, repl = job$repl)
  }
)

# optimization
get_k = function(best, .problem, budget, fs_average, fs_extrapolation) {
  fs_average = fs_average[list(.problem), , on = "problem"]
  fs_extrapolation = fs_extrapolation[list(.problem), , on = "problem"]

  estimate_iter = function(mean_best, intercept, iter, max_iter) {
    iter = ceiling((mean_best - intercept) / iter)
    if (!isTRUE(iter > max_iter)) {
      iter = max_iter + 1
    }
    iter
  }

  # assumes maximization
  if (best > max(fs_average[["mean_best"]])) {
    extrapolate = TRUE
    k = estimate_iter(best, fs_extrapolation[, intercept], fs_extrapolation[, iter], fs_extrapolation[, max_iter])
  } else {
    extrapolate = FALSE
    k = min(fs_average[mean_best >= best]$iter) # min k so that mean_best_fs[k] >= best_mbo[final]
  }
  k = k / budget # sample efficiency compared to fs
  attr(k, "extrapolate") = extrapolate
  k
}

search_space = ps(
  loop_function = p_fct(c("ego", "ego_log"), default = "ego"),
  init = p_fct(c("random", "lhs", "sobol"), default = "random"),
  init_size_fraction = p_fct(c("0.05", "0.10", "0.25"), default = "0.25"),
  random_interleave = p_lgl(default = FALSE),
  random_interleave_iter = p_fct(c("2", "5", "10"), depends = random_interleave == TRUE, default = "10"),
  rf_type = p_fct(c("standard", "extratrees", "smaclike_boot", "smaclike_no_boot"), default = "standard"),
  acqf = p_fct(c("EI", "CB", "PI", "Mean"), default = "EI"),
  acqf_ei_log = p_lgl(depends = loop_function == "ego_log" && acqf == "EI", default = FALSE),
  lambda = p_fct(c("1", "3", "10"), depends = acqf == "CB", default = "1"),
  acqopt = p_fct(c("RS_1000", "RS", "FS", "LS"), default = "RS_1000")
)

constants = ps(
  reg = p_uty(),
  fs_average = p_uty(),
  fs_extrapolation = p_uty()
)

objective = ObjectiveRFunDt$new(
  fun = function(xdt, reg, fs_average, fs_extrapolation) {
    n_repls = 2
    xdt[, id := .I]
    budget = 200

    ades = list(
      mbo = xdt
    )
    addExperiments(algo.designs = ades, repls = n_repls, reg = reg)

    job_ids = submitJobs(reg = reg)$job.id

    waitForJobs(ids = job_ids, reg = reg)

    res = rbindlist(reduceResultsList(ids = job_ids, reg = reg))

    #setorderv(res, col = "problem")
    #setorderv(res, col = "id")
    #setorderv(res, col = "repl")

    # average best over replications and determine ks
    agg = res[, .(mean_best = mean(best), raw_best = list(best), n_na = sum(is.na(best)), n = .N), by = .(id, problem)]


    ks = map_dbl(seq_row(agg), function(i) {
      if (agg[i, ][["n"]] < n_repls) {
        0
      } else {
        tryCatch(
          get_k(
            best = agg[i, ][["mean_best"]],
            .problem = agg[i, ][["problem"]],
            budget = budget,
            fs_average = fs_average,
            fs_extrapolation = fs_extrapolation
          ),
         error = function(ec) 0
        )
      }
    })
    agg[, k := ks]

    # average k over instances and determine mean_k
    agg_k = agg[, .(mean_k = exp(mean(log(k))), raw_k = list(k), n_na = sum(is.na(k)), n = .N, raw_mean_best = list(mean_best)), by = .(id)]
    # if no k on all instances, set to
    agg_k[n < length(reg$problems), mean_k := 0]
    agg_k
  },
  domain = search_space,
  codomain = ps(mean_k = p_dbl(tags = "maximize")),
  constants = constants,
  check_values = FALSE
)

objective$constants$set_values(
  reg = reg,
  fs_average = fread("/gscratch/mbecke16/mbo_config/focus_search/focus_search_average.gz"),
  fs_extrapolation = fread("/gscratch/mbecke16/mbo_config/focus_search/focus_search_extrapolation.gz")
)

if (FALSE) {
  xdt = generate_design_random(search_space, 1)$data
  init = data.table(loop_function = "ego", init = "random", init_size_fraction = "0.25", random_interleave = FALSE, random_interleave_iter = NA_character_, rf_type = "standard", acqf = "EI", acqf_ei_log = NA, lambda = NA_character_, acqopt = "RS_1000")
  objective$eval(init)
  objective$eval(xdt)
}

optim_instance = OptimInstanceSingleCrit$new(
  objective = objective,
  terminator = trm("none"),
  check_values = FALSE
)

 init = data.table(
  loop_function = "ego",
  init = "random",
  init_size_fraction = "0.25",
  random_interleave = FALSE,
  random_interleave_iter = NA_character_,
  rf_type = "standard",
  acqf = "EI",
  acqf_ei_log = NA,
  lambda = NA_character_,
  acqopt = "RS_1000")

optimizer = OptimizerCoordinateDescent$new()
optimizer$param_set$values$max_gen = 5L

optim_instance$eval_batch(init)
optimizer$optimize(optim_instance)

saveRDS(optim_instance, "coordinate_descent.rds")

