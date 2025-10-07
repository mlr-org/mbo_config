setup = mlr3misc::rowwise_table(
     ~benchmark, ~scenario, ~instance, ~target_variable, ~direction, ~budget,
     "mixed_deps", "lcbench", "167168", "val_accuracy", "maximize", 400L,
     "mixed_deps", "lcbench", "189873", "val_accuracy", "maximize", 400L,
     "mixed_deps", "lcbench", "189906", "val_accuracy", "maximize", 400L,
     "mixed_deps", "nb301", "CIFAR10", "val_accuracy", "maximize", 400L,
     "mixed_deps", "rbv2_rpart", "14", "acc", "maximize", 400L,
     "mixed_deps", "rbv2_rpart", "40499", "acc", "maximize", 400L,
     "mixed_deps", "rbv2_ranger", "16", "acc", "maximize", 400L,
     "mixed_deps", "rbv2_ranger", "42", "acc", "maximize", 400L,
     "mixed_deps", "rbv2_xgboost", "12", "acc", "maximize", 400L,
     "mixed_deps", "rbv2_xgboost", "1501", "acc", "maximize", 400L,
     "mixed_deps", "rbv2_xgboost", "16", "acc", "maximize", 400L,
     "mixed_deps", "rbv2_super", "1457", "acc", "maximize", 400L,
     "mixed_deps", "rbv2_super", "1063", "acc", "maximize", 400L,
     "mixed_deps", "rbv2_super", "15", "acc", "maximize", 400L)
setup[, id := seq_len(.N)]
saveRDS(setup, "common/mixed_deps_instances.rds")