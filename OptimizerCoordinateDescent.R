library(R6)
library(checkmate)
library(data.table)
library(mlr3misc)

# dependent parameter must contain the default of the parent parameter

OptimizerBatchCoordinateDescent = R6Class("OptimizerBatchCoordinateDescent",
  inherit = bbotk::OptimizerBatch,
  public = list(

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "coordinate_descent",
        param_set = ps(),
        param_classes = c("ParamLgl", "ParamFct"),
        properties = c("single-crit", "dependencies"),
        label = "Coordinate Descent",
        man = ""
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      dependent_parameters = inst$search_space$deps$id
      parameters = setdiff(inst$search_space$ids(), dependent_parameters)

      iterations = length(parameters)

      if (!inst$archive$n_evals) {
        # evaluate initial design
        inital_xdt = generate_design_random(inst$search_space, n = 1L)$data
        inst$eval_batch(inital_xdt)
      }

      if (inst$archive$n_evals > 1L) {
        # restore from previous state
        n_batches = inst$archive$n_batch

        # check if search space is already fully explored
        if (n_batches == inst$search_space$length + 1) return()

        # get explored parameters from incumbents
        parameters_explored = map_chr(seq(2, n_batches), function(batch) inst$archive$best(batch = batch)$parameter)
        parameters = parameters[parameters %nin% parameters_explored]

        # set incumbent to best configuration of last batch
        incumbent = inst$archive$best(batch = n_batches)[, inst$archive$cols_x, with = FALSE]
      } else {
        # fresh state
        # set incumbent to initial design
        incumbent = inst$archive$data[1, inst$archive$cols_x, with = FALSE]

      }

      # iterate over all parameters
      for (i in seq(inst$archive$n_batch + 1, iterations + 1)) {

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
        # revaluate incumbent
        set(incumbent, j = "parameter", value = "incumbent")
        xdt = rbindlist(list(incumbent, xdt))

        set(xdt, j = "iteration", value = i)
        browser()
        inst$eval_batch(xdt)

        # dont go in the direction of the incumbent
        top_2 = inst$archive$best(batch = i, n_select = 2L)
        incumbent = if (top_2[1, parameter] == "incumbent") top_2[2, ] else top_2[1, ]

        # remove parameter from list of parameters to be evaluated
        parameters = parameters[parameters %nin% incumbent$parameter]
        incumbent = incumbent[, inst$archive$cols_x, with = FALSE]
      }
    }
  )
)
