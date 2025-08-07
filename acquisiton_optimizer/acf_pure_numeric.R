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
library(reticulate)
library(mlr3learners)
library(yahpogym)

data.table::setDTthreads(1L)

source("coordinate_descent/helper.R")

use_condaenv("yahpo_gym", required = TRUE)
yahpo_gym = import("yahpo_gym")

# add problems
instances_desc = mlr3misc::rowwise_table(
  ~benchmark, ~scenario, ~instance, ~target_variable, ~direction,
  "pure_numeric", "lcbench", "167168", "val_accuracy", "maximize",
  "pure_numeric", "lcbench", "189873", "val_accuracy", "maximize"
  # "pure_numeric", "lcbench", "189906", "val_accuracy", "maximize",
  # "pure_numeric", "rbv2_rpart", "14", "acc", "maximize",
  # "pure_numeric", "rbv2_rpart", "40499", "acc", "maximize",
  # "pure_numeric", "rbv2_xgboost", "12", "acc", "maximize",
  # "pure_numeric", "rbv2_xgboost", "1501", "acc", "maximize",
  # "pure_numeric", "rbv2_xgboost", "40499", "acc", "maximize"
)

instances = pmap(instances_desc, function(benchmark, scenario, instance, target_variable, direction) {
    print(instance)

    benchmark = BenchmarkSet$new(scenario, instance = instance)
    benchmark$subset_codomain(target_variable)
    objective = benchmark$get_objective(instance, multifidelity = FALSE)
    search_space = benchmark$get_search_space(drop_fidelity_params = TRUE)
    objective = fix_objective_domain_constants_pure_numeric(scenario, objective=objective)
    search_space = get_search_space_pure_numeric(scenario)

    optim_instance = oi(objective, search_space = search_space, terminator = trm("none"))

    initial_design = generate_design_random(search_space, n = 10000)$data

    optim_instance$eval_batch(initial_design)

    list(
      benchmark = benchmark,
      scenario = scenario,
      instance = instance,
      target_variable = target_variable,
      direction = direction,
      initial_design = optim_instance$archive$data
    )
})


    surrogate = get_surrogate_pure_numeric("gp", FALSE, NA, NA, "gauss", "0", FALSE)
    acq_function = AcqFunctionEI$new()

    surrogate$archive = optim_instance$archive
    acq_function$surrogate = surrogate
    acq_function$surrogate$update()
    acq_function$update()



prob_designs = pmap(instances, function(benchmark, scenario, instance, target_variable, direction) {
  prob_id = paste0(scenario, "_", instance, "_", target_variable)
  addProblem(prob_id, data = list(
    benchmark = benchmark, scenario = scenario, instance = instance, target_variable = target_variable, direction = direction))
})

mbo = function(
  data,
  job,

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


instances = pmap(setup, function(benchmark, scenario, instance, target_variable, direction) {
    benchmark = BenchmarkSet$new(scenario, instance = instance)
    benchmark$subset_codomain(target_variable)
    objective = benchmark$get_objective(instance, multifidelity = FALSE)
    search_space = benchmark$get_search_space(drop_fidelity_params = TRUE)
    objective = fix_objective_domain_constants_pure_numeric(scenario, objective=objective)
    search_space = get_search_space_pure_numeric(scenario)

    optim_instance = oi(objective, search_space = search_space, terminator = trm("none"))

    init = generate_design_lhs(search_space, n = 50)$data # 10% to full budget

    optim_instance$eval_batch(init)

    surrogate = get_surrogate_pure_numeric("gp", FALSE, NA, NA, "gauss", "0", FALSE)
    acq_function = AcqFunctionEI$new()

    surrogate$archive = optim_instance$archive
    acq_function$surrogate = surrogate
    acq_function$surrogate$update()
    acq_function$update()

    list(
      benchmark = benchmark,
      scenario = scenario,
      instance = instance,
      target_variable = target_variable,
      direction = direction,
      surrogate = surrogate,
      acq_function = acq_function
    )
})


x = map_dtr(instances, function(instance) {
  map_dtr(100 * 2^(0:5), function(budget) {
    budget = budget * instance$acq_function$domain$length

    # random search
    acq_optimizer_random = AcqOptimizer$new(opt("random_search", batch_size = budget), terminator = trm("evals", n_evals = budget))
    acq_optimizer_random$acq_function = instance$acq_function

    rt_random_search = system.time({res_random_search = acq_optimizer_random$optimize()})

    # direct
    acq_optimizer_direct = AcqOptimizerDirect$new()
    acq_optimizer_direct$acq_function = instance$acq_function
    acq_optimizer_direct$param_set$set_values(
      maxeval = budget,
      restart_strategy = "random",
      n_restarts = 5L,
      ftol_rel = 1e-4
    )

    rt_direct = system.time({res_direct = acq_optimizer_direct$optimize()})

    # local search
    acq_optimizer_local_search = AcqOptimizerLocalSearch$new()
    acq_optimizer_local_search$acq_function = instance$acq_function
    acq_optimizer_local_search$param_set$set_values(
      n_searches = 10L,
      n_steps = ceiling(budget / 1000L),
      n_neighs = 100L
    )
    rt_local_search = system.time({res_local_search = acq_optimizer_local_search$optimize()})

    # lbfgs
    # acq_optimizer_lbfgs = AcqOptimizerLbfgsb$new()
    # acq_optimizer_lbfgs$param_set$set_values(
    #   maxeval = 30000L,
    #   restart_strategy = "random",
    #   n_restarts = 5L,
    #   ftol_rel = 1e-4
    # )
    # acq_optimizer_lbfgs$acq_function = instance$acq_function
    # acq_optimizer_lbfgs$optimize()

    # cmaes
    acq_optimizer_cmaes = AcqOptimizerCmaes$new()
    acq_optimizer_cmaes$param_set$set_values(
      maxEvals = budget,
      xtol = 1e-4
    )
    acq_optimizer_cmaes$acq_function = instance$acq_function
    rt_cmaes = system.time({res_cmaes = acq_optimizer_cmaes$optimize()})

    # focus search
    acq_optimizer_focus_search = AcqOptimizer$new(opt("focus_search", n_points = 1000L, maxit = ceiling(budget / 1000L)), terminator = trm("evals", n_evals = budget))
    acq_optimizer_focus_search$acq_function = instance$acq_function
    rt_focus_search = system.time({res_focus_search = acq_optimizer_focus_search$optimize()})

    data.table(
      instance = sprintf("%s_%s_%s", instance$scenario, instance$instance, instance$target_variable),
      d = instance$acq_function$domain$length,
      budget = budget,
      rt_random_search = rt_random_search[["elapsed"]],
      rt_direct = rt_direct[["elapsed"]],
      rt_local_search = rt_local_search[["elapsed"]],
      rt_cmaes = rt_cmaes[["elapsed"]],
      rt_focus_search = rt_focus_search[["elapsed"]],
      ei_random_search = res_random_search$acq_ei,
      ei_direct = res_direct$acq_ei,
      ei_local_search = res_local_search$acq_ei,
      ei_cmaes = res_cmaes$acq_ei,
      ei_focus_search = res_focus_search$acq_ei,
      n_iterations_direct = length(acq_optimizer_direct$state)
    )
  })
})


saveRDS(x, "acquisiton_optimizer/acq_optimizer_pure_numeric.rds")


data = readRDS("acquisiton_optimizer/acq_optimizer_pure_numeric.rds")

