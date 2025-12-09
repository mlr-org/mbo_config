library(batchtools)
library(paradox)
library(mlr3misc)
library(data.table)
library(jsonlite)

# set paths
hq = "/glade/u/home/marcbecker/mbo_config/hyperqueue/target/release/hq"
conda_dir = "/glade/work/marcbecker/conda-envs"
results_dir = "/glade/derecho/scratch/marcbecker/mbo_config/registries/competitors_numeric"
log_dir = "/glade/derecho/scratch/marcbecker/mbo_config/logs/competitors_numeric"

unlink(results_dir, recursive = TRUE)
unlink(log_dir, recursive = TRUE)
dir.create(results_dir, recursive = TRUE)
dir.create(log_dir, recursive = TRUE)

# problems
job_table = fread("common/numeric_instances.csv", colClasses = c("instance" = "character"))
job_table[, budget := as.integer(20 + 40 * sqrt(dim))]

# algorithms
algorithms = mlr3misc::rowwise_table(
  ~algorithm, ~conda_env, ~python_script, ~extra,
  "ax",       "ax",       "ax",           "",
  "hebo",     "hebo",     "hebo",         "",
  "smac4bb",  "smac",     "smac",         "--facade bb",
  "smac4hpo", "smac",     "smac",         "--facade hpo",
  "optuna",   "optuna",   "optuna",       ""
)

# expand algorithms and problems
n_probs = nrow(job_table)
job_table = job_table[rep(seq_row(job_table), nrow(algorithms)), ]
algorithms = algorithms[rep(seq_row(algorithms), each = n_probs), ]
job_table = cbind(job_table, algorithms)

# add repls and seeds
job_table = job_table[rep(seq_row(job_table), each = 30L), ]
job_table[, repl := seq_len(30L), by = c("scenario", "instance", "algorithm")]
job_table[, seed := 7832 + seq_len(.N)]

# hyperqueue commands
cmds = pmap_chr(job_table, function(scenario, instance, target_variable, direction, dim, name, budget, algorithm, conda_env, python_script, extra, repl, seed) { 
  experiment_id = sprintf("%s_%s_%s_%s", algorithm, scenario, instance, repl)
  if (log_dir != "none") {
    stdout = sprintf("%s/%s.out", log_dir, experiment_id)
    stderr = sprintf("%s/%s.err", log_dir, experiment_id)
  } else {
    stdout = "none"
    stderr = "none"
  }

  sprintf("%s submit --name %s --stdout %s --stderr %s -- %s/%s/bin/python competitors/wrapper_%s.py --benchmark pure_numeric --scenario %s --instance %s --target_variable %s --direction %s --budget %s --seed %s --output_path %s/%s.csv %s",
    hq,
    experiment_id,
    stdout,
    stderr,
    conda_dir,
    conda_env,
    python_script,
    scenario,
    instance,
    target_variable,
    direction,
    budget,
    seed,
    results_dir,
    experiment_id,
    extra)
})

#submit
batch_ids = map_chr(cmds, system, intern = TRUE)
batch_ids = as.integer(sub(".*job ID: ([0-9]+).*", "\\1", batch_ids))

set(job_table, j = "batch_id", value = batch_ids)
set(job_table, j = "cmd", value = cmds)
saveRDS(job_table, file = sprintf("%s/numeric_job_table_competitors.rds", results_dir))


# # restart failed jobs
# job_table = readRDS(sprintf("%s/numeric_job_table_competitors.rds", results_dir))
# failed_jobs = map_lgl(job_table$batch_id, function(i) {
#   print(i)
#   res = system(sprintf("hq job info %i --output-mode json", i), intern = TRUE)
#   res = fromJSON(res)
#   res$tasks[[1]]$state == "failed"
# })

# failed_job_table = job_table[failed_jobs]
# cmds = failed_job_table$cmd
# batch_ids = map_chr(cmds, system, intern = TRUE)
# batch_ids = as.integer(sub(".*job ID: ([0-9]+).*", "\\1", batch_ids))

# set(job_table, i = which(failed_jobs), j = "batch_id", value = batch_ids)
# saveRDS(job_table, file = sprintf("%s/numeric_job_table_competitors.rds", results_dir))
