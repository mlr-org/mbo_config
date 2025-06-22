library(batchtools)
library(data.table)
library(mlr3misc)

reg = loadRegistry(
  file.dir = "/glade/derecho/scratch/marcbecker/yahpo__coordinate_descent/",
  conf.file = "batchtools.conf.main.R",
  writeable = FALSE
)

rs_reference = readRDS("yahpo_mixed_deps_rs_reference.rds")

archive = rbindlist(reduceResultsList(ids = seq(10000), fun = function(res, job) {
  res[, config_hash := job$algo.pars$config_hash]
  res
}, missing.val = NULL))

archive[, meta_score := pmap_dbl(list(score, problem), function(x, problem) {
  .problem = problem
  score_rs_small = rs_reference[list(.problem), mean_best, on = "problem"]
  score_rs_large = rs_reference[list(.problem), best, on = "problem"]
  (score_rs_small - x) / (score_rs_small - score_rs_large)
})]

aggr = archive[, list(var_meta_score = var(meta_score)), by = c("id", "problem", "config_hash")]

aggr = aggr[, list(
  n = .N,
  min_var_meta_score = min(var_meta_score),
  max_var_meta_score = max(var_meta_score),
  mean_var_meta_score = mean(var_meta_score)
), by = c("problem")][order(mean_var_meta_score, decreasing = TRUE)]


ref = rs_reference[, list(rs_small = mean_best, rs_large = best), by = "problem"]

aggr[ref, on = "problem"][order(mean_var_meta_score, decreasing = TRUE)]


tab = getJobTable()
