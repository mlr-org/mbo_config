library(data.table)
library(mlr3misc)
library(batchtools)

options(width = 300)

archive = readRDS("/gscratch/mbecke16/mbo_config/intermediate_instance.rds")


var = rbindlist(map(seq(nrow(archive)), function(i) as.list(summary(archive$raw_k[[i]]))))

archive = cbind(archive, var)


fwrite(archive[, -c("raw_k", "raw_mean_score", "x_domain")], "archive.csv")


archive[order(mean_k)]


archive[id == 14][3][, raw_k]


reg = loadRegistry(
  file.dir = "/gscratch/mbecke16/mbo_config/registry_coordinate_descent",
  conf.file = "/home/mbecke16/mbo_config/coordinate_descent/batchtools.conf.R",
  writeable = TRUE
)

getStatus()
findErrors()


32554