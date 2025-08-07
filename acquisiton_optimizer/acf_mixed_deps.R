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

options(datatable.print.nrows = 1000L)

data.table::setDTthreads(1L)

source("coordinate_descent/helper.R")

use_condaenv("yahpo_gym", required = TRUE)
yahpo_gym = import("yahpo_gym")

setup = mlr3misc::rowwise_table(
    ~benchmark, ~scenario, ~instance, ~target_variable, ~direction,
    "mixed_deps", "lcbench", "167168", "val_accuracy", "maximize",
    "mixed_deps", "lcbench", "189873", "val_accuracy", "maximize",
    "mixed_deps", "lcbench", "189906", "val_accuracy", "maximize",
    "mixed_deps", "nb301", "CIFAR10", "val_accuracy", "maximize",
    "mixed_deps", "rbv2_rpart", "14", "acc", "maximize",
    "mixed_deps", "rbv2_rpart", "40499", "acc", "maximize",
    "mixed_deps", "rbv2_ranger", "16", "acc", "maximize",
    "mixed_deps", "rbv2_ranger", "42", "acc", "maximize",
    "mixed_deps", "rbv2_xgboost", "12", "acc", "maximize",
    "mixed_deps", "rbv2_xgboost", "1501", "acc", "maximize",
    "mixed_deps", "rbv2_xgboost", "16", "acc", "maximize",
    "mixed_deps", "rbv2_super", "1457", "acc", "maximize",
    "mixed_deps", "rbv2_super", "1063", "acc", "maximize",
    "mixed_deps", "rbv2_super", "15", "acc", "maximize")

instances = pmap(setup, function(benchmark, scenario, instance, target_variable, direction) {
    benchmark = BenchmarkSet$new(scenario, instance = instance)
    benchmark$subset_codomain(target_variable)
    objective = benchmark$get_objective(instance, multifidelity = FALSE)
    search_space = benchmark$get_search_space(drop_fidelity_params = TRUE)

    optim_instance = oi(objective, search_space = search_space, terminator = trm("none"))

    init = generate_design_lhs(search_space, n = 50)$data # 10% to full budget

    optim_instance$eval_batch(init)

    surrogate = get_surrogate_mixed_deps("rf", FALSE, 500, "simple", NA, NA, NA)
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

  print(instance$instance)

  map_dtr(100 * 2^(0:10), function(budget) {
    budget = budget * instance$acq_function$domain$length

    print(budget)

    # random search
    acq_optimizer_random = AcqOptimizer$new(opt("random_search", batch_size = budget), terminator = trm("evals", n_evals = budget))
    acq_optimizer_random$acq_function = instance$acq_function

    rt_random_search = system.time({res_random_search = acq_optimizer_random$optimize()})

    # local search
    acq_optimizer_local_search = AcqOptimizerLocalSearch$new()
    acq_optimizer_local_search$acq_function = instance$acq_function
    acq_optimizer_local_search$param_set$set_values(
      n_searches = 10L,
      n_steps = ceiling(budget / 1000L),
      n_neighs = 100L
    )
    rt_local_search = system.time({res_local_search = acq_optimizer_local_search$optimize()})


    sigma = (instance$acq_function$domain$upper - instance$acq_function$domain$lower) / 3

    #focus search

    # acq_optimizer_focus_search = AcqOptimizer$new(opt("focus_search", n_points = 1000L, maxit = ceiling(budget / 1000L)), terminator = trm("evals", n_evals = budget))
    # acq_optimizer_focus_search$acq_function = instance$acq_function
    # rt_focus_search = system.time({res_focus_search = acq_optimizer_focus_search$optimize()})

    data.table(
      instance = sprintf("%s_%s_%s", instance$scenario, instance$instance, instance$target_variable),
      d = instance$acq_function$domain$length,
      budget = budget,
      rt_random_search = rt_random_search[["elapsed"]],
      rt_local_search = rt_local_search[["elapsed"]],
      #rt_focus_search = rt_focus_search[["elapsed"]],
      ei_random_search = res_random_search$acq_ei,
      ei_local_search = res_local_search$acq_ei
      #ei_focus_search = res_focus_search$acq_ei
    )
  })
})

saveRDS(x, "acquisiton_optimizer/acq_optimizer_mixed_deps.rds")

data = readRDS("acquisiton_optimizer/acq_optimizer_mixed_deps.rds")

library(ggplot2)

ggplot(data, aes(x = budget, y = ei_random_search)) +
  geom_point() +
  geom_line() +
  facet_wrap(~instance, scales = "free")
