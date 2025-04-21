make_optim_instance_rs = function(instance) {
  rs_budget = 10^6L
  benchmark = BenchmarkSet$new(instance$scenario, instance = instance$instance)
  benchmark$subset_codomain(instance$target)
  objective = benchmark$get_objective(instance$instance, multifidelity = FALSE)
  optim_instance = OptimInstanceBatchSingleCrit$new(objective, search_space = benchmark$get_search_space(drop_fidelity_params = TRUE), terminator = trm("evals", n_evals = rs_budget))
  optim_instance
}
