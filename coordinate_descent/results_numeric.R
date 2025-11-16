library(data.table)
library(mlr3misc)
library(openxlsx)

# instance and archive
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

data_paper = data[, -c("id", "batch_nr", "n_na", "n")]
setnames(data_paper, c("iteration","mean_meta_score"), c("generation", "mean_rsns"))
write.xlsx(data_paper, file = "coordinate_descent/results/archive_pure_numeric.xlsx", sheetName = "archive_pure_numeric", col.names = TRUE, append = FALSE)

# optimization path
init = data[1]
set(init, j = "batch_nr", value = 0L)
set(init, j = "parameter", value = "start_config")
optimization_path = data[order(mean_meta_score)][, tail(.SD, 1), by = batch_nr][order(batch_nr)]
optimization_path = rbind(init, optimization_path)

saveRDS(optimization_path, "coordinate_descent/results/optimization_path_pure_numeric.rds")
fwrite(optimization_path[, -c("raw_meta_score", "raw_mean_score", "missing_instances",  "timestamp")], "coordinate_descent/results/optimization_path_pure_numeric.csv")

data = fread("coordinate_descent/results/archive_pure_numeric.csv")
data_2 = melt(
  data,
  id.vars = "mean_meta_score",
  measure.vars = c("input_trafo", "output_trafo", "init", "init_size_fraction", "random_interleave_iter", "surrogate", "extratrees", "trees", "variance_estimator", "kernel", "nugget", "scaling", "acqf", "lambda", "epsilon_decay", "lambda_decay", "acqopt"),
  variable.name = "parameter",
  value.name = "value"
)

data_2 = data_2[data_2[, .I[mean_meta_score == max(mean_meta_score)], by=c("parameter", "value")]$V1]
setnames(data_2, "mean_meta_score", "max_mean_meta_score")
setcolorder(data_2, c("parameter", "value", "max_mean_meta_score"))

data_2 = data_2[value != ""]
data_2[, max_mean_meta_score := round(max_mean_meta_score, 2)]

fwrite(data_2, "coordinate_descent/results/best_scores_pure_numeric.csv")

knitr::kable(data_2, format = "latex", booktabs = TRUE)
