library(batchtools)
library(data.table)
library(mlr3misc)

options(datatable.print.nrows = 500)
options(width = 200)

reg = loadRegistry(
  file.dir = "/glade/derecho/scratch/lschneider/yahpo_mixed_deps_competitors",
  conf.file = "batchtools.conf.main.R",
  writeable = FALSE
)


summary_instances = fread("/glade/u/home/marcbecker/mbo_config/analyze/yapho_instances_mixed_deps.csv")
summary_instances[, problem := paste0(scenario, "_", instance, "_", target_variable)]

job_table = getJobTable()
job_table = job_table[summary_instances, on = "problem"]

job_table[, time.running := as.numeric(time.running / 60)]
tab = job_table[, list(mean_runtime = mean(time.running), min_runtime = min(time.running), max_runtime = max(time.running)), by = c("dimension", "algorithm")][order(dimension,max_runtime, decreasing = TRUE, na.last = FALSE)]

knitr::kable(tab, digits = 0)

# pure numeric
reg = loadRegistry(
  file.dir = "/glade/derecho/scratch/lschneider/yahpo_pure_numeric_competitors",
  conf.file = "batchtools.conf.main.R",
  writeable = FALSE
)

summary_instances = fread("/glade/u/home/marcbecker/mbo_config/analyze/yapho_instances_pure_numeric.csv")
summary_instances[, problem := paste0(scenario, "_", instance, "_", target_variable)]

job_table = getJobTable()
job_table = job_table[summary_instances, on = "problem"]

job_table[, time.running := as.numeric(time.running / 60)]
tab = job_table[, list(mean_runtime = mean(time.running), min_runtime = min(time.running), max_runtime = max(time.running)), by = c("dimension", "algorithm")][order(dimension,max_runtime, decreasing = TRUE, na.last = FALSE)]

knitr::kable(tab, digits = 0)








