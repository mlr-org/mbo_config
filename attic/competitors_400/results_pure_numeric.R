library(batchtools)
library(paradox)
library(mlr3misc)
library(data.table)
library(jsonlite)

setup = readRDS("common/pure_numeric_instances.rds")
job_table = readRDS("competitors/job_table_competitors_pure_numeric.rds")
rs_reference = readRDS("random_search/yahpo_pure_numeric_rs_reference.rds")
results_dir = "/glade/derecho/scratch/marcbecker/mbo_config/registries/competitors_pure_numeric"

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
saveRDS(job_table, file = "competitors/job_table_competitors_pure_numeric.rds")

# mlr3mbo
reg = loadRegistry("/glade/derecho/scratch/marcbecker/mbo_config/registries/competitors_mlr3mbo_pure_numeric")

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
saveRDS(bt_job_table[, list(problem, runtime, dim)], "competitors/job_table_mlr3mbo_pure_numeric.rds")


results = rbindlist(list(results_competitors, results_mlr3mbo), use.names = TRUE)

fwrite(results, "competitors/results/pure_numeric_archive.csv")

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

fwrite(aggr_results, "competitors/results/pure_numeric_aggr.csv")

# average over scenarios and instances
data = aggr_results[, list(
  mean_meta_score = mean(meta_score),
  se_meta_score = sd(meta_score) / sqrt(.N),
  mean_rank = mean(rank),
  se_rank = sd(rank) / sqrt(.N)), by = c("algorithm", "iter")]

fwrite(data, "competitors/results/pure_numeric.csv")

# friedman test
data = fread("competitors/results/pure_numeric_aggr.csv")

data = data[iter == 200]
data[, problem := paste0(scenario, "_", instance)]
data = data[, list(algorithm, problem, meta_score)]
data[, algorithm := factor(algorithm)]
data[, problem := factor(problem)]

friedman.test(meta_score ~ algorithm | problem, data = data)

frdAllPairsNemenyiTest(meta_score ~ algorithm | problem, data = data)

