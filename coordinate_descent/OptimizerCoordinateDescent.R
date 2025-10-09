library(R6)
library(checkmate)
library(data.table)
library(mlr3misc)


OptimizerBatchCoordinateDescent = R6Class("OptimizerBatchCoordinateDescent",
  inherit = bbotk::OptimizerBatch,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "coordinate_descent",
        param_set = ps(
          n_generations = p_int(lower = 1L, tags = "required"),
          start = p_uty()),
        param_classes = c("ParamLgl", "ParamFct"),
        properties = c("single-crit", "dependencies"),
        label = "Coordinate Descent",
        man = ""
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      n_generations = self$param_set$values$n_generations

      if (inst$archive$n_evals) {
        # restore from previous state
        n_batches = inst$archive$n_batch

        # check if search space is already fully explored
        if (n_batches == n_generations) return()

        # set incumbent to best configuration of last batch
        incumbent = inst$archive$best(batch = n_batches)[, inst$archive$cols_x, with = FALSE]
      } else {
        # fresh state
        # set incumbent to start configuration
        incumbent = self$param_set$values$start
        n_batches = 0L
      }

      # iterate over all generations
      for (i in seq(n_batches + 1, n_generations)) {

        xdt = get_generation(inst, incumbent)

        # remove duplicates
        xdt = unique(xdt, by = inst$search_space$ids())

        # remove incumbent but keep start configuration in first generation
        if (i > 1) {
          xdt = xdt[!incumbent, on = inst$search_space$ids()]
        }

        set(xdt, j = "iteration", value = i)
        inst$eval_batch(xdt)

        best = inst$archive$best(batch = i, n_select = 1L)

        # TODO: Terminate if improvement is below threshold
        if (best$parameter == "incumbent") {
          message("Terminate optimization because incumbent is the best configuration")
          break
        }

        incumbent = best[, inst$archive$cols_x, with = FALSE]
      }
    }
  )
)

get_generation = function(inst, incumbent) {
  dependent_parameters = inst$search_space$deps$id
  parameters = setdiff(inst$search_space$ids(), dependent_parameters)

  xdt = map_dtr(inst$search_space$subspaces(ids = parameters), function(subset) {
    param_id = subset$ids()
    param_levels = subset$levels[[1]]

    # copy incumbent n times where n is the number of levels of the active parameter
    xdt_subspace = incumbent[rep(1, length(param_levels)), ]
    set(xdt_subspace, j = param_id, value = param_levels)

    # activate dependent parameters and try all levels of them
    if (inst$search_space$has_deps && param_id %in% inst$search_space$deps$on) {
      # get dependencies that are on the active parameter
      deps = inst$search_space$deps[on == param_id]
      deps[, rhs := map_chr(cond, "rhs")]

      xdt_deps = map_dtr(unique(deps$rhs), function(.rhs) {
        # find configuration where the dependency is satisfied
        xdt_dep = xdt_subspace[list(.rhs), , on = param_id]

        # get levels of depedent parameters
        ids = deps[rhs == .rhs, id]
        levels = inst$search_space$levels[ids]
        values = expand.grid(levels, stringsAsFactors = FALSE)

        # copy the configuration n times where n is the number of levels of the dependent parameters
        xdt_dep  = xdt_dep[rep(1, nrow(values)), ]

        # set the dependent parameters to all levels
        set(xdt_dep, j = ids, value = values)

        xdt_dep
      })

      xdt_subspace = rbindlist(list(xdt_subspace[!deps$rhs, , on = param_id], xdt_deps))
    }

    # deactivate parameters with unsatisfiable dependencies
    xdt_subspace = Design$new(inst$search_space, data = xdt_subspace, remove_dupl = TRUE)$data
    set(xdt_subspace, j = "parameter", value = param_id)
    xdt_subspace
  })

  if (!isTRUE(inst$search_space$check_dt(xdt[, -c("parameter")], check_strict = TRUE))) {
    stop("New generation contrains invalid configurations")
  }

  return(xdt)
}

if (FALSE) {

  library(bbotk)
  library(paradox)
  inst = list(search_space = ps(
    surrogate = p_fct(c("rf", "gp")),
    trees = p_fct(c("10", "500"), depends = surrogate == "rf"),
    kernel = p_fct(c("rbf", "matern3_2", "matern5_2", "exp", "powexp"), depends = surrogate == "gp"),
    nugget = p_fct(c("0", "1e-8"), depends = surrogate == "gp")
  ))

  incumbent = data.table(
    surrogate = "rf",
    trees = "10",
    kernel = NA_character_,
    nugget = NA_character_
  )

  xdt = get_generation(inst, incumbent)
  assert_data_table(xdt, nrows = 12)

  # incumbent
  assert_data_table(xdt[!incumbent, on = inst$search_space$ids()], nrows = 11)

  ##
  inst = list(search_space = ps(
    acquisition_function = p_fct(c("ei", "mean")),
    surrogate = p_fct(c("rf", "gp")),
    trees = p_fct(c("10", "500"), depends = surrogate == "rf"),
    kernel = p_fct(c("rbf", "matern3_2", "matern5_2", "exp", "powexp"), depends = surrogate == "gp"),
    nugget = p_fct(c("0", "1e-8"), depends = surrogate == "gp")
  ))

  incumbent = data.table(
    acquisition_function = "ei",
    surrogate = "rf",
    trees = "10",
    kernel = "rbf",
    nugget = "0"
  )

  xdt = get_generation(inst, incumbent)
  assert_data_table(xdt, nrows = 14)

  ##
  inst = list(search_space = ps(
    acquisition_function = p_fct(c("cb" ,"ei", "mean")),
    lambda = p_fct(c("1", "3", "10"), depends = acquisition_function == "cb"),
    surrogate = p_fct(c("rf", "gp")),
    trees = p_fct(c("10", "500"), depends = surrogate == "rf"),
    kernel = p_fct(c("rbf", "matern3_2", "matern5_2", "exp", "powexp"), depends = surrogate == "gp"),
    nugget = p_fct(c("0", "1e-8"), depends = surrogate == "gp")
  ))

  incumbent = data.table(
    acquisition_function = "ei",
    lambda = NA_character_,
    surrogate = "rf",
    trees = "10",
    kernel = "rbf",
    nugget = "0"
  )

  xdt = get_generation(inst, incumbent)
  assert_data_table(xdt, nrows = 17)

  ##
  inst = list(search_space = ps(
    acqf = p_fct(c("cb" ,"ei", "mean")),
    lambda = p_fct(c("1", "3", "10"), depends = acqf == "cb"),
    lambda_decay = p_lgl(depends = acqf == "cb"),
    surrogate = p_fct(c("rf", "gp")),
    trees = p_fct(c("10", "500"), depends = surrogate == "rf"),
    kernel = p_fct(c("rbf", "matern3_2", "matern5_2", "exp", "powexp"), depends = surrogate == "gp"),
    nugget = p_fct(c("0", "1e-8"), depends = surrogate == "gp")
  ))

  incumbent = data.table(
    acqf = "ei",
    lambda = NA_character_,
    lambda_decay = FALSE,
    surrogate = "rf",
    trees = "10",
    kernel = "rbf",
    nugget = "0"
  )

  xdt = get_generation(inst, incumbent)
  assert_data_table(xdt, nrows = 20)

  ##
  inst = list(search_space = ps(
    input_trafo            = p_fct(c("none", "unitcube")),
    output_trafo           = p_fct(c("none", "standardize", "log")),
    init                   = p_fct(c("random", "lhs", "sobol")),
    init_size_fraction     = p_fct(c("0.05", "0.10", "0.25")),
    random_interleave_iter = p_fct(c("0", "2", "4")),
    # surrogate
    surrogate = p_fct(c("rf", "gp")),
    extratrees = p_lgl(depends = surrogate == "rf"),
    trees = p_fct(c("10", "500"), depends = surrogate == "rf"),
    variance_estimator = p_fct(c("jackknife", "simple", "law_of_total_variance"), depends = surrogate == "rf"),
    kernel = p_fct(c("rbf", "matern3_2", "matern5_2", "exp", "powexp"), depends = surrogate == "gp"),
    nugget = p_fct(c("0", "1e-3", "1e-8"), depends = surrogate == "gp"),
    scaling = p_lgl(depends = surrogate == "gp"),
    # acqf
    acqf = p_fct(c("EI", "CB", "PI", "Mean")),
    lambda = p_fct(c("1", "3", "10"), depends = acqf == "CB"),
    epsilon_decay = p_lgl(depends = acqf == "EI"),
    lambda_decay = p_lgl(depends = acqf == "CB"),
    # acqopt
    acqopt = p_fct(c("RS_1000", "RS", "FS", "LS", "DIRECT", "CMAES", "LBFGSB"))
  ))

   incumbent = data.table(
    input_trafo = "none",
    output_trafo = "none",
    init = "random",
    init_size_fraction = "0.25",
    random_interleave_iter = "0",
    surrogate = "rf",
    extratrees = FALSE,
    trees = "10",
    variance_estimator = "jackknife",
    kernel = NA_character_,
    nugget = NA_character_,
    scaling = NA,
    acqf = "EI",
    lambda = NA_character_,
    epsilon_decay = FALSE,
    lambda_decay = NA,
    acqopt = "RS_1000"
  )

  xdt = get_generation(inst, incumbent)
  assert_data_table(xdt, nrows = 73)

  # remove dubplicated and incumbent
  xdt = unique(xdt, by = inst$search_space$ids())
  assert_data_table(xdt, nrows = 66)
  assert_data_table(xdt[!incumbent, on = inst$search_space$ids()], nrows = 65)

  ##
  search_space = ps(
    input_trafo            = p_fct(c("none", "unitcube")),
    output_trafo           = p_fct(c("none", "standardize", "log")),
    init                   = p_fct(c("random", "lhs", "sobol")),
    init_size_fraction     = p_fct(c("0.05", "0.10", "0.25")),
    random_interleave_iter = p_fct(c("0", "2", "4")),
    # surrogate
    surrogate              = p_fct(c("rf", "gp")),
    extratrees             = p_lgl(depends = surrogate == "rf"),
    trees                  = p_fct(c("10", "500"), depends = surrogate == "rf"),
    variance_estimator     = p_fct(c("jack", "simple", "law_of_total_variance"), depends = surrogate == "rf"),
    kernel                 = p_fct(c("gauss", "matern3_2", "matern5_2", "exp"), depends = surrogate == "gp"),
    nugget                 = p_fct(c("0", "1e-3", "1e-8"), depends = surrogate == "gp"),
    scaling                = p_lgl(depends = surrogate == "gp"),
    # acqf
    acqf                   = p_fct(c("EI", "CB", "PI", "Mean")),
    lambda                 = p_fct(c("1", "3", "10"), depends = acqf == "CB"),
    epsilon_decay          = p_lgl(depends = acqf == "EI"),
    lambda_decay           = p_lgl(depends = acqf == "CB"),
    # acqopt
    acqopt                 = p_fct(c("RS_1000", "RS", "LS", "DIRECT", "CMAES", "LBFGSB"))
  )

  library(bbotk)
  objective = ObjectiveRFunDt$new(
    fun = function(xdt) {
      data.table(y = sample(100, nrow(xdt), replace = TRUE))
    },
    domain = search_space
  )

  start = data.table(
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
  optimizer$param_set$values$n_generations = 1
  optimizer$param_set$values$start = init

  instance = oi(
    objective = objective,
    search_space = search_space,
    terminator = trm("none"),
  )

  optimizer$optimize(instance)

  instance$archive$data[batch_nr == 1]
  instance$archive$data[batch_nr == 2]

  # best_1 = instance$archive$best(batch = 1)
  # #incumbent_2 = instance$archive$data[batch_nr == 2 & parameter == "incumbent"]

  # #all.equal(best_1[, inst$search_space$ids(), with = FALSE], incumbent_2[, inst$search_space$ids(), with = FALSE])

  # best_2 = instance$archive$best(batch = 2)
  # #incumbent_3 = instance$archive$data[batch_nr == 3 & parameter == "incumbent"]

  # all.equal(best_2[, inst$search_space$ids(), with = FALSE], incumbent_3[, inst$search_space$ids(), with = FALSE])
}
