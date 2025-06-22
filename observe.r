library(batchtools)
library(data.table)

reg = loadRegistry(
  file.dir = "/glade/derecho/scratch/marcbecker/yahpo_pure_numeric_coordinate_descent",
  conf.file = "batchtools.conf.main.R",
  writeable = FALSE
)

instance = readRDS("/glade/derecho/scratch/marcbecker/mixed_deps_intermediate_instance.rds")

instance[1]

tmp = copy(instance[, setdiff(names(instance), c("raw_mean_score", "raw_score", "raw_meta_score", "x_domain", "timestamp", "batch_nr", "missing_instances", "id")), with = FALSE])

setorder(tmp, iteration, mean_meta_score)
tmp

instance = readRDS("/glade/derecho/scratch/marcbecker/pure_numeric_intermediate_instance.rds")

instance[1]

tmp = copy(instance[, setdiff(names(instance), c("raw_mean_score", "raw_score", "raw_meta_score", "x_domain", "timestamp", "batch_nr", "missing_instances", "id")), with = FALSE])

setorder(tmp, iteration, mean_meta_score)
tmp


getJobTable()[order(time.running, decreasing = TRUE)][1, algo.pars]


##############################

 unique(rbindlist(getJobTable(findRunning())$algo.pars))
