library(batchtools)
library(data.table)
library(mlr3misc)

options(datatable.print.nrows = 500)
options(width = 200)

reg = loadRegistry(
  file.dir = "/glade/derecho/scratch/marcbecker/yahpo_mixed_deps_coordinate_descent_2",
  conf.file = "batchtools.conf.main.R",
  writeable = FALSE
)

summary_instances = fread("/glade/u/home/marcbecker/mbo_config/yapho_instances_mixed_deps.csv")
summary_instances[, problem := paste0(scenario, "_", instance, "_", target_variable)]

job_table = getJobTable()
job_table = unnest(job_table, "algo.pars")
job_table[, config_hash := pmap_chr(list(input_trafo, output_trafo, init, init_size_fraction, random_interleave_iter, surrogate, acqf, lambda, acqopt, epsilon_decay, lambda_decay), function(input_trafo, output_trafo, init, init_size_fraction, random_interleave_iter, surrogate, acqf, lambda, acqopt, epsilon_decay, lambda_decay) {
  mlr3misc::calculate_hash(list(input_trafo, output_trafo, init, init_size_fraction, random_interleave_iter, surrogate, acqf, lambda, acqopt, epsilon_decay, lambda_decay))
})]

job_table = job_table[summary_instances, on = "problem"]


x = job_table[acqf %nin% "Mean" & random_interleave_iter == 0 & init_size_fraction == 0.25]
x[, list(mean_runtime = mean(time.running), min_runtime = min(time.running), max_runtime = max(time.running)), by = c("surrogate", "acqopt", "dimension", "budget")][order(dimension,max_runtime, decreasing = TRUE, na.last = FALSE)]

knitr::kable(x[, list(mean_runtime = mean(time.running), min_runtime = min(time.running), max_runtime = max(time.running)), by = c("surrogate", "acqopt", "dimension", "budget")][order(dimension,max_runtime, decreasing = TRUE, na.last = FALSE)])
