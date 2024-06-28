library(bbotk)

instance = readRDS("/home/marc/repositories/mbo_config/coordinate_descent.rds")

archive = instance$archive$data

archive[, setdiff(names(archive), c("raw_k", "n_na", "raw_mean_best", "incumbent")), with = FALSE]
