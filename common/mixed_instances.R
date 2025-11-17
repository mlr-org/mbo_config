instances = mlr3misc::rowwise_table(
     ~scenario,      ~instance, ~target_variable,  ~direction,   ~dim,     ~name,
     "lcbench",      "167168",  "val_accuracy",    "maximize",   7,        "vehicle",
     "lcbench",      "189873",  "val_accuracy",    "maximize",   7,        "dionis",
     "lcbench",      "189906",  "val_accuracy",    "maximize",   7,        "segment",
     "nb301",        "CIFAR10", "val_accuracy",    "maximize",   34,       "CIFAR10",
     "rbv2_rpart",   "14",      "acc",             "maximize",   5,        "mfeat-fourier",
     "rbv2_rpart",   "40499",   "acc",             "maximize",   5,        "texture",
     "rbv2_ranger",  "16",      "acc",             "maximize",   8,        "mfeat-karhunen",
     "rbv2_ranger",  "42",      "acc",             "maximize",   8,        "soybean",
     "rbv2_xgboost", "12",      "acc",             "maximize",   14,       "mfeat-factors",
     "rbv2_xgboost", "1501",    "acc",             "maximize",   14,       "semeion",
     "rbv2_xgboost", "16",      "acc",             "maximize",   14,       "mfeat-karhunen",
     "rbv2_super",   "1457",    "acc",             "maximize",   38,       "amazon-commerce-reviews",
     "rbv2_super",   "1063",    "acc",             "maximize",   38,       "kc2",
     "rbv2_super",   "15",      "acc",             "maximize",   38,       "breast-w")

fwrite(instances, "common/mixed_instances.csv", quote = TRUE)

