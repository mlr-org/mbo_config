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
      parameters = inst$search_space$ids()

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
      for (i in seq_along(parameters)) {

        xdt = map_dtr(inst$search_space$subspaces(ids = parameters), function(subset) {
          param_id = subset$ids()
          param_levels = subset$levels[[1]]

          # copy incumbent n times where n is the number of levels of the active parameter
          xdt_param = incumbent[rep(1, length(param_levels)), ]
          set(xdt_param, j = param_id, value = param_levels)

          # activate parameters
          if (inst$search_space$has_deps && param_id %in% inst$search_space$deps$on) {
            # get dependencies that are on the active parameter
            deps = inst$search_space$deps[on == param_id]

            for (j in seq_row(deps)) {
              # find parameter that depend on the active parameter and are not set
              to_replace = which(map_lgl(xdt_param[[param_id]], function(x) paradox:::condition_test(deps$cond[[j]], x)) & is.na(xdt_param[[deps$id[[j]]]]))
              if (!length(to_replace)) next
              # copy the incumbent with active parameter n times where n is the number of levels of the dependent parameter
              tmp_xdt = xdt_param[to_replace, ]
              levels = inst$search_space$subspaces(ids = deps$id[[j]])[[1]]$levels[[1]]
              tmp_xdt = tmp_xdt[rep(1, length(levels)), ]
              set(tmp_xdt, j = deps$id[[j]], value = levels)
              xdt_param = rbindlist(list(xdt_param, tmp_xdt))
            }
          }

          # deactivate parameters
          xdt_param = Design$new(inst$search_space, data = xdt_param, remove_dupl = TRUE)$data
          set(xdt_param, j = "parameter", value = param_id)
          xdt_param
        })

        set(xdt, j = "iteration", value = i)

        inst$eval_batch(xdt)

        incumbent = inst$archive$best(batch = i + 1L)
        parameters = parameters[parameters %nin% incumbent$parameter]
        incumbent = incumbent[, inst$archive$cols_x, with = FALSE]
      }
    }
  )
)
