setup = mlr3misc::rowwise_table(
    ~benchmark, ~scenario, ~instance, ~target_variable, ~direction, ~budget,
    "pure_numeric", "lcbench", "167168", "val_accuracy", "maximize", 400L,
    "pure_numeric", "lcbench", "189873", "val_accuracy", "maximize", 400L,
    "pure_numeric", "lcbench", "189906", "val_accuracy", "maximize", 400L,
    "pure_numeric", "rbv2_rpart", "14", "acc", "maximize", 400L,
    "pure_numeric", "rbv2_rpart", "40499", "acc", "maximize", 400L,
    "pure_numeric", "rbv2_xgboost", "12", "acc", "maximize", 400L,
    "pure_numeric", "rbv2_xgboost", "1501", "acc", "maximize", 400L,
    "pure_numeric", "rbv2_xgboost", "40499", "acc", "maximize", 400L
)
setup[, id := seq_len(.N)]
saveRDS(setup, "common/pure_numeric_instances.rds")
