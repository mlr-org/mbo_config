library(batchtools)
library(data.table)
library(mlr3misc)

options(datatable.print.nrows = 200)
options(width = 200)

reg = loadRegistry(
  file.dir = "/glade/derecho/scratch/marcbecker/yahpo_mixed_deps_coordinate_descent_2",
  conf.file = "batchtools.conf.main.R",
  writeable = FALSE
)

rs_reference = readRDS("yahpo_mixed_deps_rs_reference.rds")

archive = rbindlist(reduceResultsList(fun = function(res, job) {
  cbind(res, as.data.table(job$algo.pars))
}, missing.val = NULL), use.names = TRUE)


# add meta score per evaluation
archive[, meta_score := pmap_dbl(list(score, problem), function(x, problem) {
  .problem = problem
  score_rs_small = rs_reference[list(.problem), mean_best, on = "problem"]
  score_rs_large = rs_reference[list(.problem), best, on = "problem"]
  (score_rs_small - x) / (score_rs_small - score_rs_large)
})]


archive[, config_hash := pmap_chr(list(input_trafo, output_trafo, init, init_size_fraction, random_interleave_iter, surrogate, acqf, lambda, acqopt, epsilon_decay, lambda_decay), function(input_trafo, output_trafo, init, init_size_fraction, random_interleave_iter, surrogate, acqf, lambda, acqopt, epsilon_decay, lambda_decay) {
  mlr3misc::calculate_hash(list(input_trafo, output_trafo, init, init_size_fraction, random_interleave_iter, surrogate, acqf, lambda, acqopt, epsilon_decay, lambda_decay))
})]

fwrite(archive, "mixed_deps_archive.csv")

fwrite(archive[, list(problem, config_hash, replication, meta_score)], "mixed_deps_results.csv")

# number of evaluations per config
archive[, list(n = .N), by = "config_hash"]

sd_data = archive[, list(sd_meta_score = sd(meta_score)), by = c("problem", "config_hash")]

summary_sd_data = sd_data[, list(
  mean_sd_meta_score = mean(sd_meta_score, na.rm = TRUE),
  min_sd_meta_score = min(sd_meta_score, na.rm = TRUE),
  max_sd_meta_score = max(sd_meta_score, na.rm = TRUE)
), by = c("problem")][order(mean_sd_meta_score, decreasing = TRUE)]











