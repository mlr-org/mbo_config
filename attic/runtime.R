library(batchtools)
library(data.table)

# mixed deps
registry_name = "/glade/derecho/scratch/marcbecker/yahpo_mixed_deps_coordinate_descent"

reg = loadRegistry(
  file.dir = registry_name,
  writeable = FALSE
)

job_table = getJobTable()
job_table = unnest(job_table, c("algo.pars", "prob.pars"))

search_space = readRDS("common/mixed_deps_search_space.rds")
ids = search_space$ids()

runtimes = job_table[, list(mean_runtime = as.integer(mean(time.running, na.rm = TRUE) / 60), min_runtime = as.integer(min(time.running, na.rm = TRUE) / 60), max_runtime = as.integer(max(time.running, na.rm = TRUE) / 60)), by = ids]

fwrite(runtimes, "coordinate_descent/results/mixed_deps_runtime.csv")
saveRDS(runtimes, "coordinate_descent/results/mixed_deps_runtime.rds")

# pure numeric
registry_name = "/glade/derecho/scratch/marcbecker/yahpo_pure_numeric_coordinate_descent"

reg = loadRegistry(
  file.dir = registry_name,
  writeable = FALSE
)

job_table = getJobTable()
job_table = unnest(job_table, c("algo.pars", "prob.pars"))

search_space = readRDS("common/pure_numeric_search_space.rds")
ids = search_space$ids()

runtimes = job_table[, list(mean_runtime = as.integer(mean(time.running, na.rm = TRUE) / 60), min_runtime = as.integer(min(time.running, na.rm = TRUE) / 60), max_runtime = as.integer(max(time.running, na.rm = TRUE) / 60)), by = ids]

fwrite(runtimes, "coordinate_descent/results/pure_numeric_runtime.csv")
saveRDS(runtimes, "coordinate_descent/results/pure_numeric_runtime.rds")