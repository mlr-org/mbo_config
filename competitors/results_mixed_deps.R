library(batchtools)
library(paradox)
library(mlr3misc)
library(data.table)
library(jsonlite)

setup = readRDS("common/mixed_deps_instances.rds")
job_table = readRDS("competitors/job_table_competitors_mixed_deps.rds")
rs_reference = readRDS("random_search/yahpo_mixed_deps_rs_reference.rds")
results_dir = "/glade/derecho/scratch/marcbecker/mbo_config/registries/competitors_mixed_deps"

# competitors
results_competitors = pmap_dtr(job_table, function(benchmark, scenario, instance, target_variable, budget, seed, repl, algorithm, ...) {
    experiment_id = sprintf("%s_%s_%s_%s_%s", benchmark, algorithm, scenario, instance, repl)
    print(experiment_id)
    file = sprintf("%s/%s.csv", results_dir, experiment_id)

    if (!file.exists(file)) {
      print(sprintf("File %s does not exist", file))
      return(NULL)
    }

    archive = fread(file)
    set(archive, j = "repl", value = repl)
    archive[, iter := seq_len(.N)]
    setnames(archive, old = target_variable, new = "score")
    archive[, incumbent := cummin(score)]
    set(archive, j = "scenario", value = scenario)
    set(archive, j = "instance", value = instance)
    set(archive, j = "algorithm", value = algorithm)
    archive[, list(algorithm, scenario, instance, iter, repl, incumbent, score)]
})

runtimes_competitors = map_dtr(job_table$batch_id, function(i) {
  print(i)
  res = system(sprintf("hq job info %i --output-mode json", i), intern = TRUE)
  res = fromJSON(res)

  start_time = sub("Z$", "", res$started_at)
  start_time = sub("(\\.[0-9]{6})[0-9]*", "\\1", start_time)
  start_time = as.POSIXct(start_time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

  end_time = sub("Z$", "", res$finished_at) 
  end_time = sub("(\\.[0-9]{6})[0-9]*", "\\1", end_time)
  end_time = as.POSIXct(end_time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

  data.table(
    batch_id = i,
    state = res$tasks[[1]]$state,
    runtime = difftime(end_time, start_time, units = "hours")
  )
})

job_table = job_table[runtimes_competitors, on = "batch_id"]
saveRDS(job_table, file = "competitors/job_table_competitors_mixed_deps.rds")

# mlr3mbo
reg = loadRegistry("/glade/derecho/scratch/marcbecker/competitors_mlr3mbo_mixed_deps")

results_mlr3mbo = rbindlist(reduceResultsList(fun = function(archive, job) {
  archive = setDT(archive)
  set(archive, j = "repl", value = job$repl)
  archive[, iter := seq_len(.N)]
  setnames(archive, old = job$instance$target_variable, new = "score")
  archive[, score := - score] # instance$direction == "maximize"
  archive[, incumbent := cummin(score)]
  set(archive, j = "scenario", value = job$instance$scenario)
  set(archive, j = "instance", value = job$instance$instance)
  set(archive, j = "algorithm", value = "mlr3mbo")
  archive[, list(algorithm, scenario, instance, iter, repl, incumbent, score)]
}, missing.val = NULL), use.names = TRUE)

bt_job_table = getJobTable()
bt_job_table = unnest(bt_job_table, c("algo.pars", "prob.pars"))
bt_job_table[, runtime := as.numeric(time.running) / 3600]
bt_job_table = bt_job_table[setup[, list(problem = paste0(scenario, "_", instance), dim = dim)], on = "problem"]
saveRDS(bt_job_table[, list(problem, runtime, dim)], "competitors/job_table_mlr3mbo_mixed_deps.rds")

results = rbindlist(list(results_competitors, results_mlr3mbo), use.names = TRUE)

# average over replications
aggr_results = results[, list(mean_incumbent = mean(incumbent)), by = c("iter", "algorithm", "scenario", "instance")]

# determine meta score of mean incumbent
aggr_results[, meta_score := pmap_dbl(list(mean_incumbent, scenario, instance), function(mean_incumbent, scenario, instance) {
  .scenario = scenario
  .instance = instance
  score_rs_small = rs_reference[list(.scenario, .instance), mean_best, on = c("scenario", "instance")]
  score_rs_large = rs_reference[list(.scenario, .instance), best, on = c("scenario", "instance")]
  (score_rs_small - mean_incumbent) / (score_rs_small - score_rs_large)
})]

# determine rank
aggr_results[, rank := rank(-meta_score), by = c("scenario", "instance", "iter")]

# average over scenarios and instances
data = aggr_results[, list(
  mean_meta_score = mean(meta_score), 
  se_meta_score = sd(meta_score) / sqrt(.N),
  mean_rank = mean(rank), 
  se_rank = sd(rank) / sqrt(.N)), by = c("algorithm", "iter")]

fwrite(data, "competitors/results/mixed_deps.csv")

library(ggplot2)
pdf("competitors/results/mixed_deps_mean_meta_score.pdf", width = 10, height = 10)
ggplot(data, aes(x = iter, y = mean_meta_score, color = algorithm, fill = algorithm)) +
  geom_line() +
  geom_ribbon(aes(min = mean_meta_score - se_meta_score, max = mean_meta_score + se_meta_score), colour = NA, alpha = 0.3) +
  ylim(-1, 1) +
  labs(x = "Iteration", y = "Mean Meta Score") +
  theme_minimal()
dev.off()

pdf("competitors/results/mixed_deps_ranking.pdf", width = 10, height = 10)
ggplot(data, aes(x = iter, y = mean_rank, color = algorithm)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_rank - se_rank, ymax = mean_rank + se_rank, fill = algorithm), colour = NA, alpha = 0.2) +
  labs(x = "Iteration", y = "Mean Rank") +
  theme_minimal()
dev.off()

job_table = job_table[setup[, list(benchmark, scenario, instance, dim)], on = c("benchmark", "scenario", "instance")]
mean_runtimes_competitors = job_table[, list(mean_runtime = as.numeric(mean(runtime))), by = c("algorithm", "dim")]
mean_runtimes_mlr3mbo = bt_job_table[, list(mean_runtime = as.numeric(mean(runtime, na.rm = TRUE))), by = "dim"]
set(mean_runtimes_mlr3mbo, j = "algorithm", value = "mlr3mbo")
mean_runtimes = rbindlist(list(mean_runtimes_competitors, mean_runtimes_mlr3mbo), use.names = TRUE)
mean_runtimes[, dim := as.factor(dim)]

pdf("competitors/results/mixed_deps_runtimes.pdf", width = 10, height = 10)
ggplot(mean_runtimes, aes(x = dim, y = mean_runtime, fill = algorithm)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Dimension", y = "Mean Runtime", fill = "Algorithm") +
  theme_minimal()
dev.off()
