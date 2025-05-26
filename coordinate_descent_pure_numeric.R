library(batchtools)
library(data.table)
library(mlr3)
library(mlr3misc)
library(mlr3mbo)
library(mlr3pipelines)
library(bbotk)
library(paradox)
library(R6)
library(checkmate)

source("submit_ncar.R")

YAHPO_BENCHMARK = "pure_numeric"  # "pure_numeric", "mixed", "mixed_deps"

reticulate::use_virtualenv("/glade/u/home/lschneider/mbo_config/yahpo_venv", required = TRUE)
library(reticulate)
yahpo_gym = import("yahpo_gym")

packages = c("data.table", "mlr3", "mlr3learners", "mlr3misc", "mlr3mbo", "mlr3pipelines", "bbotk", "paradox", "ranger", "R6", "checkmate")

root = here::here()
experiments_dir = file.path(root)

source_files = map_chr(c("helper.R"), function(x) file.path(experiments_dir, x))
for (source_file in source_files) {
  source(source_file)
}

registry_name = gsub("YAHPO_BENCHMARK", replacement = YAHPO_BENCHMARK, x = "/glade/derecho/scratch/lschneider/yahpo_YAHPO_BENCHMARK_coordinate_descent")
if (!file.exists(file.path(registry_name, "registry.rds"))) {
  reg = makeExperimentRegistry(
    file.dir = registry_name,
    conf.file = "batchtools.conf.main.R",
    packages = packages,
    source = source_files
  )
  saveRegistry(reg)
} else {
  reg = loadRegistry(
    file.dir = registry_name,
    conf.file = "batchtools.conf.main.R",
    writeable = TRUE
  )
}

source("OptimizerCoordinateDescent.R")

if (YAHPO_BENCHMARK == "pure_numeric") {
  setup = data.table(
    benchmark = YAHPO_BENCHMARK,
    scenario = rep(c("lcbench", paste0("rbv2_", c("glmnet", "rpart", "ranger", "xgboost"))), c(3L, 2L, 2L, 2L, 4L)),
    instance = c(
        "167168", "189873", "189906",
        "375", "458",
        "14", "40499",
        "16", "42",
        "12", "1501", "16", "40499"
    ),
    target_variable = rep(c("val_accuracy", "acc"), c(3L, 10L)),
    direction = rep("maximize", 13L),
    budget = rep(c(126L, 77L, 100L, 100L, 147L), c(3L, 2L, 2L, 2L, 4L))
  )
} else if (YAHPO_BENCHMARK == "mixed") {
    stop("TBD")
} else if (YAHPO_BENCHMARK == "") {
    setup = data.table(
    benchmark = YAHPO_BENCHMARK,
    scenario = rep(c("lcbench", "nb301", paste0("rbv2_", c("glmnet", "rpart", "ranger", "xgboost", "super"))), c(3L, 1L, 2L, 2L, 2L, 4L, 6L)),
    instance = c(
        "167168", "189873", "189906",
        "CIFAR10",
        "375", "458",
        "14", "40499",
        "16", "42",
        "12", "1501", "16", "40499",
        "1053", "1457", "1063", "1479", "15", "1468"
    ),
    target_variable = rep(c("val_accuracy", "acc"), c(4L, 16L)),
    direction = rep("maximize", 20L),
    budget = rep(c(126L, 254L, 90L, 110L, 134L, 170L, 267L), c(3L, 1L, 2L, 2L, 2L, 4L, 6L))
  )
}

setup[, id := seq_len(.N)]

# add problems
prob_designs = map(seq_len(nrow(setup)), function(i) {
  prob_id = paste0(setup[i, ]$scenario, "_", setup[i, ]$instance, "_", setup[i, ]$target_variable)
  addProblem(prob_id, data = list(benchmark = setup[i, ]$benchmark, scenario = setup[i, ]$scenario, instance = setup[i, ]$instance, target_variable = setup[i, ]$target_variable, direction = setup[i, ]$direction, budget = setup[i, ]$budget))
  setNames(list(setup[i, ]), nm = prob_id)
})
prob_names = sapply(prob_designs, names)
prob_designs = unlist(prob_designs, recursive = FALSE, use.names = FALSE)
names(prob_designs) = prob_names

search_space = ps(
  input_trafo = p_fct(c("none", "unitcube")),
  output_trafo = p_fct(c("none", "standardize", "log")),
  init = p_fct(c("random", "lhs", "sobol")),
  init_size_fraction = p_fct(c("0.05", "0.10", "0.25")),
  random_interleave_iter = p_fct(c("0", "2", "5", "10")),
  surrogate = p_fct(c("rf_standard", "rf_extratrees", "rf_smaclike_simple", "rf_smaclike_law_of_total_variance", "gp_rbf", "gp_3_2", "gp_5_2")),
  acqf = p_fct(c("EI", "CB", "PI", "Mean")),
  lambda = p_fct(c("1", "3", "10"), depends = acqf == "CB"),
  acqopt = p_fct(c("RS_1000", "RS", "FS", "LS", "DIRECT", "CMAES", "LBFGSB")),
  epsilon_decay = p_lgl(depends = acqf == "EI"),
  lambda_decay = p_lgl(depends = acqf == "CB")
)

addAlgorithm(
  name = "mbo",
  fun = function(
    job,
    data,
    instance,
    input_trafo,
    output_trafo,
    init,
    init_size_fraction,
    random_interleave_iter,
    surrogate,
    acqf,
    lambda,
    acqopt,
    epsilon_decay,
    lambda_decay,
    id,
    config_hash
    ) {

    reticulate::use_virtualenv("/glade/u/home/lschneider/mbo_config/yahpo_venv", required = TRUE)
    library(yahpogym)
    logger = lgr::get_logger("bbotk")
    logger$set_threshold("warn")
    future::plan("sequential")

    optim_instance = make_optim_instance(instance)

    random_interleave_iter = as.numeric(random_interleave_iter)
    init_size_fraction = as.numeric(init_size_fraction)
    lambda = as.numeric(lambda)
    init_design_size = ceiling(as.numeric(init_size_fraction) * instance$budget)
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

    if (surrogate == "rf_standard") {
      learner = LearnerRegrRangerMbo$new()
      learner$predict_type = "se"
      learner$param_set$values$keep.inbag = TRUE
      learner$param_set$values$se.method = "jack"
      learner$param_set$values$splitrule = "variance"
      learner$param_set$values$num.trees = 1000L
    } else if (surrogate == "rf_extratrees") {
      learner = LearnerRegrRangerMbo$new()
      learner$predict_type = "se"
      learner$param_set$values$keep.inbag = TRUE
      learner$param_set$values$se.method = "jack"
      learner$param_set$values$splitrule = "extratrees"
      learner$param_set$values$num.random.splits = 1L
      learner$param_set$values$num.trees = 1000L
    } else if (surrogate == "rf_smaclike_simple") {
      learner = LearnerRegrRangerMbo$new()
      learner$predict_type = "se"
      learner$param_set$values$se.method = "simple"
      learner$param_set$values$splitrule = "variance"
      learner$param_set$values$num.trees = 10L
      learner$param_set$values$replace = TRUE
      learner$param_set$values$sample.fraction = 1
      learner$param_set$values$min.node.size = 3
      learner$param_set$values$min.bucket = 3
      learner$param_set$values$mtry.ratio = 5/6
    } else if (surrogate == "rf_smaclike_law_of_total_variance") {
      learner = LearnerRegrRangerMbo$new()
      learner$predict_type = "se"
      learner$param_set$values$se.method = "law_of_total_variance"
      learner$param_set$values$splitrule = "variance"
      learner$param_set$values$num.trees = 10L
      learner$param_set$values$replace = TRUE
      learner$param_set$values$sample.fraction = 1
      learner$param_set$values$min.node.size = 3
      learner$param_set$values$min.bucket = 3
      learner$param_set$values$mtry.ratio = 5/6
    } else if (surrogate == "gp_rbf") {
      learner = LearnerRegrKM$new()
      learner$predict_type = "se"
      learner$param_set$values$control = list(trace = FALSE)
      learner$param_set$values$optim.method = "gen"
      learner$param_set$values$covtype = "gauss"
    } else if (surrogate == "gp_3_2") {
      learner = LearnerRegrKM$new()
      learner$predict_type = "se"
      learner$param_set$values$control = list(trace = FALSE)
      learner$param_set$values$optim.method = "gen"
      learner$param_set$values$covtype = "matern3_2"
    } else if (surrogate == "gp_5_2") {
      learner = LearnerRegrKM$new()
      learner$predict_type = "se"
      learner$param_set$values$control = list(trace = FALSE)
      learner$param_set$values$optim.method = "gen"
      learner$param_set$values$covtype = "matern5_2"
    }

    surrogate = SurrogateLearner$new(learner)
    surrogate$param_set$values$catch_errors = FALSE

    if (input_trafo == "unitcube") {
      surrogate$input_trafo = InputTrafoUnitcube$new()
    }

    if (output_trafo == "standardize") {
      surrogate$output_trafo = OutputTrafoStandardize$new()
    } else if (output_trafo == "log") {
      surrogate$output_trafo = OutputTrafoLog$new(invert_posterior = FALSE)
    }

    acq_optimizer = if (acqopt == "RS_1000") {
      AcqOptimizer$new(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 1000L))
    } else if (acqopt == "RS") {
      AcqOptimizer$new(opt("random_search", batch_size = 1000L), terminator = trm("evals", n_evals = 30000L))
    } else if (acqopt == "FS") {
      n_repeats = 3L
      maxit = 9L
      batch_size = ceiling((30000L / n_repeats) / (1 + maxit)) # 1000L
      AcqOptimizer$new(opt("focus_search", n_points = batch_size, maxit = maxit), terminator = trm("evals", n_evals = 30000L))
    } else if (acqopt == "LS") {
      AcqOptimizer$new(opt("local_search", n_initial_points = 10L, initial_random_sample_size = 1000L, neighbors_per_point = 100L), terminator = trm("evals", n_evals = 30000L))
    } else if (acqopt == "DIRECT") {
      optimizer = opt("chain",
        optimizers = rep(list(opt("random_search", batch_size = 1000L), opt("nloptr", algorithm = "NLOPT_GN_DIRECT_L")), times = 10L),
        terminators = rep(list(trm("evals", n_evals = 1000L), trm("combo", terminators = list(trm("evals", n_evals = 2000L), trm("stagnation", iters = 100L, threshold = 1e-12)))), times = 10L))
      cb = callback_batch("start_values",
        on_optimization_begin = function(callback, context) {
        if (class(context$optimizer)[1L] == "OptimizerBatchNLoptr") {
          start = unlist(context$result[, context$instance$archive$cols_x, with = FALSE])  # previous random search
          context$optimizer$param_set$values$start_values = "custom"
          context$optimizer$param_set$values$start = start
        }
      })
      acq_optimizer = AcqOptimizer$new(optimizer, terminator = trm("evals", n_evals = 30000L), callbacks = list(cb))
      acq_optimizer
    } else if (acqopt == "CMAES") {
      optimizer = opt("chain",
        optimizers = rep(list(opt("random_search", batch_size = 1000L), opt("cmaes")), times = 10L),
        terminators = rep(list(trm("evals", n_evals = 1000L), trm("combo", terminators = list(trm("evals", n_evals = 2000L), trm("stagnation", iters = 100L, threshold = 1e-12)))), times = 10L))
      cb = callback_batch("start_values",
        on_optimization_begin = function(callback, context) {
        if (class(context$optimizer)[1L] == "OptimizerBatchCmaes") {
          start = unlist(context$result[, context$instance$archive$cols_x, with = FALSE])  # previous random search
          context$optimizer$param_set$values$start_values = "custom"
          context$optimizer$param_set$values$start = start
        }
      })
      acq_optimizer = AcqOptimizer$new(optimizer, terminator = trm("evals", n_evals = 30000L), callbacks = list(cb))
      acq_optimizer
    } else if (acqopt == "LBFGSB") {
      optimizer = opt("chain",
        optimizers = rep(list(opt("random_search", batch_size = 1000L), opt("nloptr", algorithm = "NLOPT_LD_LBFGS", approximate_eval_grad_f = TRUE)), times = 10L),
        terminators = rep(list(trm("evals", n_evals = 1000L), trm("combo", terminators = list(trm("evals", n_evals = 2000L), trm("stagnation", iters = 100L, threshold = 1e-12)))), times = 10L))
      cb = callback_batch("start_values",
        on_optimization_begin = function(callback, context) {
        if (class(context$optimizer)[1L] == "OptimizerBatchNLoptr") {
          start = unlist(context$result[, context$instance$archive$cols_x, with = FALSE])  # previous random search
          context$optimizer$param_set$values$start_values = "custom"
          context$optimizer$param_set$values$start = start
        }
      })
      acq_optimizer = AcqOptimizer$new(optimizer, terminator = trm("evals", n_evals = 30000L), callbacks = list(cb))
      acq_optimizer
    }
    acq_optimizer$param_set$values$catch_errors = FALSE

    acq_function = if (acqf == "EI" && output_trafo == "log") {
      AcqFunctionEILog$new()
    } else if (acqf == "EI" && output_trafo != "log") {
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

    bayesopt_ego(
        optim_instance,
        surrogate = surrogate,
        acq_function = acq_function,
        acq_optimizer = acq_optimizer,
        random_interleave_iter = random_interleave_iter,
        init_design_size = init_design_size)

    score = optim_instance$archive$best()[[instance$target_variable]]
    if (instance$direction == "maximize") {
      score = - score
    }

    data.table(
      id = id,
      replication = job$repl,
      problem = job$problem$name,
      scenario = instance$scenario,
      instance = instance$instance,
      target_variable = instance$target_variable,
      direction= instance$direction,
      budget = instance$budget,
      score = score
    )
  }
)

init = data.table(
  input_trafo = "none",
  output_trafo = "none",
  init = "random",
  init_size_fraction = "0.25",
  random_interleave_iter = "0",
  surrogate = "gp_5_2",
  acqf = "EI",
  lambda = NA_character_,
  acqopt = "RS_1000",
  epsilon_decay = FALSE,
  lambda_decay = NA)

constants = ps(
  reg = p_uty(),
  rs_reference = p_uty()
)

objective = ObjectiveRFunDt$new(
  fun = function(
    xdt,
    reg,
    rs_reference
    ) {
    n_repls = 15L
    xdt[, id := .I]
    set(xdt, j = "config_hash", value = uuid::UUIDgenerate(n = nrow(xdt)))  # make experiments unique to avoid skipping

    ades = list(mbo = xdt)
    job_ids = addExperiments(algo.designs = ades, repls = n_repls, reg = reg)
    #ids[, chunk := batchtools::chunk(job.id, chunk.size = 96L, shuffle = FALSE)]
    job_ids = submit_ncar(job_ids$job.id, reg, template = "pbs_derecho_main.tmpl", n_jobs = 128L)
    waitForJobs(ids = job_ids, reg = reg)

    while(TRUE) {
      if (length(findExpired()$job.id)) {
        message("Resubmitting expired jobs")
        expired_ids = findExpired()
        #expired_ids[, chunk := batchtools::chunk(job.id, chunk.size = 96L, shuffle = FALSE)]
        resubmitted_ids = submit_ncar(expired_ids$job.id, reg, template = "pbs_derecho_main.tmpl", n_jobs = 128L)
        waitForJobs(ids = resubmitted_ids, reg = reg)
      } else {
        break
      }
    }

    res = rbindlist(reduceResultsList(ids = intersect(job_ids, findDone()$job.id), reg = reg))

    # average score over replications
    agg = res[, list(mean_score = mean(score), raw_score = list(score), n_na = sum(is.na(score)), n = .N), by = list(id, problem)]

    # determine meta score
    meta_scores = pmap_dbl(agg, function(problem, mean_score, ...) {
      .problem = problem
      score_rs_small = rs_reference[list(.problem), mean_best, on = "problem"]
      score_rs_large= rs_reference[list(.problem), best, on = "problem"]

      (score_rs_small - mean_score) / (score_rs_small - score_rs_large)
    })
    set(agg, j = "meta_score", value = meta_scores)

    # average meta score over problems
    agg_meta_score = agg[, list(
      mean_meta_score = mean(meta_score),
      raw_meta_score = list(set_names(meta_score, problem)),
      n_na = sum(is.na(meta_score)),
      n = .N,
      raw_mean_score = list(set_names(mean_score, problem)),
      missing_instances = list(setdiff(reg$problems, problem))),
      by = .(id)]
    agg_meta_score
  },
  domain = search_space,
  codomain = ps(mean_meta_score = p_dbl(tags = "maximize")),
  constants = constants,
  check_values = FALSE
)

if (YAHPO_BENCHMARK == "pure_numeric") {
  objective$constants$set_values(
    reg = reg,
    rs_reference = readRDS("yahpo_pure_numeric_rs_reference.rds")
  )
} else if (YAHPO_BENCHMARK == "mixed") {
  stop("TBD")
} else if (YAHPO_BENCHMARK == "") {
  objective$constants$set_values(
    reg = reg,
    rs_reference = readRDS("yahpo_rs_reference.rds")
  )
}

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

state_path = "/glade/derecho/scratch/lschneider/YAHPO_BENCHMARK_intermediate_instance.rds"
callback_backup$state$path = gsub("YAHPO_BENCHMARK", replacement = YAHPO_BENCHMARK, x = state_path)

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

save_path = "/glade/derecho/scratch/lschneider/YAHPO_BENCHMARK_coordinate_descent.rds"
saveRDS(optim_instance, gsub("YAHPO_BENCHMARK", replacement = YAHPO_BENCHMARK, x = save_path))
