library(data.table)
library(mlr3misc)
library(batchtools)

reg = loadRegistry(
  file.dir = "/gscratch/mbecke16/mbo_config/registry_coordinate_descent",
  conf.file = "/home/mbecke16/mbo_config/coordinate_descent/batchtools.conf.R",
  writeable = TRUE
)

problems = reg$problems


data = readRDS("/gscratch/mbecke16/mbo_config/intermediate_instance.rds")

var = rbindlist(map(seq(nrow(data)), function(i) as.list(summary(data$raw_k[[i]]))))


which(data$n < 1286)


raw_k = data$raw_k[[46]]


instances = fread("random_search/instances.csv")
instances[, instance := as.character(instance)]
rbv2 = instances[grep("rbv2", scenario)]
lcbench = instances[grep("lcbench", scenario)]

#rbv2 = rbv2[, .SD[sample(.N, min(.N, 16))], by = scenario]





list_cols = which(sapply(data, is.list))
set(data, j = list_cols, value = NULL)
fwrite(data, "archive.csv")