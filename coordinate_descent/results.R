library(data.table)
library(mlr3misc)
library(openxlsx)

walk(c("numeric", "mixed"), function(benchmark) {
  search_space = readRDS(sprintf("common/%s_search_space.rds", benchmark))
  instance = readRDS(sprintf("/glade/derecho/scratch/marcbecker/%s_coordinate_descent.rds", benchmark))
  file.copy(sprintf("/glade/derecho/scratch/marcbecker/%s_coordinate_descent.rds", benchmark), sprintf("coordinate_descent/results/%s_instance.rds", benchmark))

  data = instance$archive$data
  data$x_domain = NULL
  fwrite(data[, -c("raw_meta_score", "raw_mean_score", "missing_instances", "timestamp")], sprintf("coordinate_descent/results/%s_archive.csv", benchmark))
  saveRDS(data, sprintf("coordinate_descent/results/%s_archive.rds", benchmark))

  data_paper = data[, -c("id", "batch_nr", "n_na", "n")]
  setnames(data_paper, c("iteration","mean_meta_score"), c("generation", "mean_rsns"))
  write.xlsx(data_paper, file = sprintf("coordinate_descent/results/%s_archive.xlsx", benchmark), sheetName = sprintf("archive_%s", benchmark), col.names = TRUE, append = FALSE)

  # optimization path
  init = data[1]
  set(init, j = "batch_nr", value = 0L)
  set(init, j = "parameter", value = "start_config")
  optimization_path = data[order(mean_meta_score)][, tail(.SD, 1), by = batch_nr][order(batch_nr)]
  optimization_path = rbind(init, optimization_path)

  fwrite(optimization_path[, -c("raw_meta_score", "raw_mean_score", "missing_instances",  "timestamp")], sprintf("coordinate_descent/results/%s_optimization_path.csv", benchmark))

  data_long = melt(
    data,
    id.vars = "mean_meta_score",
    measure.vars = search_space$ids(),
    variable.name = "parameter",
    value.name = "value"
  )

  data_long_best = data_long[data_long[, .I[mean_meta_score == max(mean_meta_score)], by=c("parameter", "value")]$V1]
  setnames(data_long_best, "mean_meta_score", "max_mean_meta_score")
  setcolorder(data_long_best, c("parameter", "value", "max_mean_meta_score"))

  data_long_best = data_long_best[!is.na(value)]

  fwrite(data_long_best, sprintf("coordinate_descent/results/%s_best_scores.csv", benchmark))
})
