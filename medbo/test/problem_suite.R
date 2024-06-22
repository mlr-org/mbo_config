## branin
loader_branin = function(budget) {
  objective = ObjectiveRFun$new(
    fun = function(xs) {
      branin(xs$x1, xs$x2)
    },
    domain = ps(
      x1 = p_dbl(lower = -5, upper = 10),
      x2 = p_dbl(lower = 0, upper = 15)
    ),
    codomain = ps(
      y = p_dbl(tags = "minimize")
    ),
    check_values = FALSE
  )

  OptimInstanceSingleCrit$new(
    objective, 
    terminator = trm("evals", n_evals = budget), 
    check_values = FALSE)
}

addProblem(
  name = "branin",
  data = list(
    loader = loader_branin,
    args = list(budget = 200 * 3000)
  )
)