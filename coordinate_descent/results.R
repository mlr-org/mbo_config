library(data.table)
library(mlr3misc)

instance = readRDS("/glade/derecho/scratch/marcbecker/pure_numeric_coordinate_descent.rds")

data = instance$archive$data
rows = seq_row(data)
rows = rows[rows %nin% 118]
data = data[rows]
data$x_domain = NULL

instance$archive$data = data
saveRDS(instance, "coordinate_descent/results/instance_pure_numeric.rds")


saveRDS(data, "coordinate_descent/results/archive_pure_numeric.rds")
fwrite(data[, -c("raw_meta_score", "raw_mean_score", "missing_instances", "timestamp")], "coordinate_descent/results/archive_pure_numeric.csv")


init = data[1]
set(init, j = "batch_nr", value = 0L)
set(init, j = "parameter", value = "start_config")
optimization_path = data[order(mean_meta_score)][, tail(.SD, 1), by = batch_nr][order(batch_nr)]
optimization_path = rbind(init, optimization_path)

saveRDS(optimization_path, "coordinate_descent/results/optimization_path_pure_numeric.rds")
fwrite(optimization_path[, -c("raw_meta_score", "raw_mean_score", "missing_instances",  "timestamp")], "coordinate_descent/results/optimization_path_pure_numeric.csv")

# mixed deps

instance = readRDS("/glade/derecho/scratch/marcbecker/mixed_deps_coordinate_descent.rds")
data = instance$archive$data
data$x_domain = NULL
instance$archive$data = data

saveRDS(instance, "coordinate_descent/results/instance_mixed_deps.rds")
saveRDS(data, "coordinate_descent/results/archive_mixed_deps.rds")
fwrite(data[, -c("raw_meta_score", "raw_mean_score", "missing_instances", "timestamp")], "coordinate_descent/results/archive_mixed_deps.csv")

init = data[1]
set(init, j = "batch_nr", value = 0L)
set(init, j = "parameter", value = "start_config")
optimization_path = data[order(mean_meta_score)][, tail(.SD, 1), by = batch_nr][order(batch_nr)]
optimization_path = rbind(init, optimization_path)

saveRDS(optimization_path, "coordinate_descent/results/optimization_path_mixed_deps.rds")
fwrite(optimization_path[, -c("raw_meta_score", "raw_mean_score", "missing_instances",  "timestamp")], "coordinate_descent/results/optimization_path_mixed_deps.csv")
