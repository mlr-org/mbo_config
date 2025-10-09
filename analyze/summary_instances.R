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
library(yahpogym)
library(mlr3oml)

options(width = 250)


use_condaenv("yahpo_gym", required = TRUE)
yahpo_gym = import("yahpo_gym")

setup = mlr3misc::rowwise_table(
     ~benchmark, ~scenario, ~instance, ~target_variable, ~direction, ~budget,
     "mixed_deps", "lcbench", "167168", "val_accuracy", "maximize", 126,
     "mixed_deps", "lcbench", "189873", "val_accuracy", "maximize", 126,
     "mixed_deps", "lcbench", "189906", "val_accuracy", "maximize", 126,
     "mixed_deps", "nb301", "CIFAR10", "val_accuracy", "maximize", 254,
     "mixed_deps", "rbv2_rpart", "14", "acc", "maximize", 110,
     "mixed_deps", "rbv2_rpart", "40499", "acc", "maximize", 110,
     "mixed_deps", "rbv2_ranger", "16", "acc", "maximize", 134,
     "mixed_deps", "rbv2_ranger", "42", "acc", "maximize", 134,
     "mixed_deps", "rbv2_xgboost", "12", "acc", "maximize", 170,
     "mixed_deps", "rbv2_xgboost", "1501", "acc", "maximize", 170,
     "mixed_deps", "rbv2_xgboost", "16", "acc", "maximize", 170,
     "mixed_deps", "rbv2_super", "1457", "acc", "maximize", 267,
     "mixed_deps", "rbv2_super", "1063", "acc", "maximize", 267,
     "mixed_deps", "rbv2_super", "15", "acc", "maximize", 267)

otasks = list_oml_tasks(task_id = c(167168, 189873, 189906))
data_source = as.data.table(otasks)[, list(task_id, name)]
data_source[, task_id := as.character(task_id)]

data_source = rbindlist(list(data_source, data.table(task_id = "CIFAR10", name = "CIFAR10")))

odata = list_oml_data(data_id = c(14, 40499, 16, 42, 12, 1501, 1457, 1063, 15))
data_source = rbindlist(list(data_source, as.data.table(odata)[, list(task_id = data_id, name = name)]))
setup = setup[data_source, , on = c("instance" = "task_id")]

# get dimension
d = pmap_int(setup, function(scenario, instance, ...) {
  benchmark = BenchmarkSet$new(scenario, instance = instance)
  objective = benchmark$get_objective(instance, multifidelity = FALSE)
  objective$domain$length
})

setup[, dimension := d]

setcolorder(setup, c("benchmark", "scenario", "instance", "name", "target_variable", "direction", "budget", "dimension"))
setup[, benchmark := NULL]
setup[, direction := NULL]

fwrite(setup[order(scenario, name)], "yapho_instances_mixed_deps.csv")

setup = mlr3misc::rowwise_table(
  ~benchmark, ~scenario, ~instance, ~target_variable, ~direction, ~budget,
  "pure_numeric", "lcbench", "167168", "val_accuracy", "maximize", 126,
  "pure_numeric", "lcbench", "189873", "val_accuracy", "maximize", 126,
  "pure_numeric", "lcbench", "189906", "val_accuracy", "maximize", 126,
  "pure_numeric", "rbv2_rpart", "14", "acc", "maximize", 100,
  "pure_numeric", "rbv2_rpart", "40499", "acc", "maximize", 100,
  "pure_numeric", "rbv2_xgboost", "12", "acc", "maximize", 147,
  "pure_numeric", "rbv2_xgboost", "1501", "acc", "maximize", 147,
  "pure_numeric", "rbv2_xgboost", "40499", "acc", "maximize", 147)

otasks = list_oml_tasks(task_id = c(167168, 189873, 189906))
data_source = as.data.table(otasks)[, list(task_id, name)]
data_source[, task_id := as.character(task_id)]

odata = list_oml_data(data_id = c(14, 40499, 12, 1501, 40499))
data_source = rbindlist(list(data_source, as.data.table(odata)[, list(task_id = data_id, name = name)]))
setup = setup[data_source, , on = c("instance" = "task_id")]

# get dimension
d = pmap_int(setup, function(scenario, instance, ...) {
  benchmark = BenchmarkSet$new(scenario, instance = instance)
  objective = benchmark$get_objective(instance, multifidelity = FALSE)
  objective$domain$length
})

setup[, dimension := d]

setcolorder(setup, c("benchmark", "scenario", "instance", "name", "target_variable", "direction", "budget", "dimension"))
setup[, benchmark := NULL]
setup[, direction := NULL]

fwrite(setup[order(scenario, name)], "yapho_instances_pure_numeric.csv")




