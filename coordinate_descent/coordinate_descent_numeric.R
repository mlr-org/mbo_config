con = file("coordinate_descent_numeric.log")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

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
source("coordinate_descent/OptimizerCoordinateDescent.R")

registry_name = "/glade/derecho/scratch/marcbecker/mbo_config/registries/coordinate_descent_numeric"
unlink(registry_name, recursive = TRUE)
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

if (!file.exists(file.path(registry_name, "registry.rds"))) {
  reg = makeExperimentRegistry(
    file.dir = registry_name,
    conf.file = NA,
    packages = packages,
    source = "common/numeric_objective.R",
    seed = 7832
  )
  reg$cluster.functions = makeClusterFunctionsHyperQueue()
  saveRegistry(reg)
} else {
  reg = loadRegistry(
    file.dir = registry_name,
    conf.file = NA,
    writeable = TRUE
  )
  reg$cluster.functions = makeClusterFunctionsHyperQueue()
}

# problems
instances = fread("common/numeric_instances.csv", colClasses = c("instance" = "character"))
instances[, budget := as.integer(100 + 40 * sqrt(dim))]
pwalk(instances, function(scenario, instance, target_variable, budget, direction, ...) {
  id = sprintf("%s_%s", scenario, instance)
  addProblem(id, data = list(scenario = scenario, instance = instance, target_variable = target_variable, budget = budget, direction = direction))
})

# algorithm
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
    extratrees,
    trees,
    variance_estimator,
    kernel,
    nugget,
    scaling,
    acqf,
    lambda,
    acqopt,
    epsilon_decay,
    lambda_decay,
    id,
    config_hash
    ) {
    renv::load(".")

    optim_instance = numeric_objective(
      scenario = instance$scenario,
      instance = instance$instance,
      target_variable = instance$target_variable,
      budget = instance$budget,
      input_trafo = input_trafo,
      output_trafo = output_trafo,
      init = init,
      init_size_fraction = init_size_fraction,
      random_interleave_iter = random_interleave_iter,
      surrogate = surrogate,
      extratrees = extratrees,
      trees = trees,
      variance_estimator = variance_estimator,
      kernel = kernel,
      nugget = nugget,
      scaling = scaling,
      acqf = acqf,
      lambda = lambda,
      acqopt = acqopt,
      epsilon_decay = epsilon_decay,
      lambda_decay = lambda_decay)

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

# coordinate descent search space
search_space = readRDS("common/numeric_search_space.rds")

# coordinate descent objective
objective = ObjectiveRFunDt$new(
  fun = function(
    xdt,
    reg,
    rs_reference
    ) {
    xdt_path = "/glade/derecho/scratch/marcbecker/pure_numeric_intermediate_xdt.rds"
    job_ids_path = "/glade/derecho/scratch/marcbecker/pure_numeric_intermediate_job_ids.rds"

    n_repls = 30L
    xdt[, id := .I]
    # make experiments unique to avoid skipping
    set(xdt, j = "config_hash", value = uuid::UUIDgenerate(n = nrow(xdt)))

    ades = list(mbo = xdt)
    job_ids = addExperiments(algo.designs = ades, repls = n_repls, reg = reg)$job.id
    submitJobs(job_ids, resources = list(ncpus = 1L, walltime = 14400L), reg = reg)

    tmp_file = tempfile(tmpdir = dirname(xdt_path), fileext = ".rds")
    saveRDS(xdt, tmp_file)
    unlink(xdt_path)
    file.rename(tmp_file, xdt_path)

    tmp_file = tempfile(tmpdir = dirname(job_ids_path), fileext = ".rds")
    saveRDS(job_ids, tmp_file)
    unlink(job_ids_path)
    file.rename(tmp_file, job_ids_path)

    # job_ids = readRDS(job_ids_path)
    # xdt = readRDS(xdt_path)

    waitForJobs(ids = job_ids, reg = reg)

    res = rbindlist(reduceResultsList(ids = intersect(job_ids, findDone()$job.id), reg = reg))

    # average score over replications
    agg = res[, list(mean_score = mean(score), raw_score = list(score), n_na = sum(is.na(score)), n = .N), by = list(id, problem)]

    # determine meta score
    rs_reference[, problem := paste0(scenario, "_", instance)]
    meta_scores = pmap_dbl(agg, function(problem, mean_score, ...) {
      .problem = problem
      score_rs_small = rs_reference[list(.problem), rs_small, on = "problem"]
      score_rs_large= rs_reference[list(.problem), rs_large, on = "problem"]

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

    # if no meta score on all instances, set to -Inf
    agg_meta_score[n < 8, mean_meta_score := -Inf]

    agg_meta_score
  },
  domain = search_space,
  codomain = ps(mean_meta_score = p_dbl(tags = "maximize")),
  constants = ps(reg = p_uty(), rs_reference = p_uty()),
  check_values = FALSE
)

objective$constants$set_values(
  reg = reg,
  rs_reference = fread("random_search/results/numeric_rs_reference_100.csv", colClasses = c("instance" = "character"))
)

# backup archive after each coordinate descent iteration
callback_backup = callback_batch("bbotk.backup",
  label = "Backup Archive Callback",
  man = "bbotk::bbotk.backup",

  on_optimizer_after_eval = function(callback, context) {
    tmp_file = tempfile(tmpdir = dirname(callback$state$path), fileext = ".rds")
    saveRDS(context$instance$archive$data, tmp_file)
    unlink(callback$state$path)
    file.rename(tmp_file, callback$state$path)
  }
)
state_path = "/glade/derecho/scratch/marcbecker/numeric_intermediate_instance.rds"
callback_backup$state$path = state_path

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
}

init = data.table(
  input_trafo = "none",
  output_trafo = "none",
  init = "random",
  init_size_fraction = "0.25",
  random_interleave_iter = "0",
  surrogate = "gp",
  extratrees = NA,
  trees = NA_character_,
  variance_estimator = NA_character_,
  kernel = "gauss",
  nugget = "0",
  scaling = FALSE,
  acqf = "EI",
  lambda = NA_character_,
  epsilon_decay = FALSE,
  lambda_decay = NA,
  acqopt = "RS_1000"
)

optimizer = OptimizerBatchCoordinateDescent$new()
optimizer$param_set$set_values(
  n_generations = 8L,
  start = init
)
optimizer$optimize(optim_instance)

save_path = "/glade/derecho/scratch/marcbecker/numeric_coordinate_descent.rds"
saveRDS(optim_instance, save_path)
