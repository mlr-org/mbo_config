library(batchtools)
library(paradox)
library(mlr3misc)
library(data.table)
library(jsonlite)

pwalk(list(
  benchmark = c("numeric", "mixed", "budget_mixed"),
  instance = c("numeric", "mixed", "mixed"),
  rs_reference = c("numeric", "mixed", "mixed"),
  hq_registry_dir = c("numeric", "mixed", "mixed"),
  hq_job_table = c("numeric", "mixed", "mixed"),
  bt_registry = c("numeric", "mixed", "budget_mixed")), 
  function(benchmark, instance, rs_reference, hq_registry_dir, hq_job_table, bt_registry) {

  instances = fread(sprintf("common/%s_instances.csv", instance), colClasses = c("instance" = "character"))
  rs_reference = fread(sprintf("random_search/results/%s_rs_reference_20.csv", rs_reference), colClasses = c("instance" = "character"))
  hq_registry_dir = sprintf("/glade/derecho/scratch/marcbecker/mbo_config/registries/competitors_%s_2", hq_registry_dir)
  hq_job_table = readRDS(sprintf("%s/%s_job_table_competitors.rds", hq_registry_dir, hq_job_table))
  bt_registry = loadRegistry(sprintf("/glade/derecho/scratch/marcbecker/mbo_config/registries/competitors_mlr3mbo_%s", bt_registry))

  # competitors
  results_competitors = pmap_dtr(hq_job_table, function(benchmark, scenario, instance, target_variable, budget, seed, repl, algorithm, ...) {
      experiment_id = sprintf("%s_%s_%s_%s_%s", benchmark, algorithm, scenario, instance, repl)
      print(experiment_id)
      file = sprintf("%s/%s.csv", hq_registry_dir, experiment_id)

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

  # runtimes_competitors = map_dtr(job_table$batch_id, function(i) {
  #   print(i)
  #   res = system(sprintf("hq job info %i --output-mode json", i), intern = TRUE)
  #   res = fromJSON(res)

  #   start_time = sub("Z$", "", res$tasks[[1]]$started_at)
  #   start_time = sub("(\\.[0-9]{6})[0-9]*", "\\1", start_time)
  #   start_time = as.POSIXct(start_time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

  #   end_time = sub("Z$", "", res$tasks[[1]]$finished_at)
  #   end_time = sub("(\\.[0-9]{6})[0-9]*", "\\1", end_time)
  #   end_time = as.POSIXct(end_time, format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC")

  #   data.table(
  #     batch_id = i,
  #     state = res$tasks[[1]]$state,
  #     runtime = difftime(end_time, start_time, units = "mins")
  #   )
  # })

  # hq_job_table = hq_job_table[runtimes_competitors, on = "batch_id"]
  hq_job_table[, runtime := as.integer(round(as.numeric(runtime, units = "secs")))]

  # mlr3mbo
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
  }, missing.val = NULL, reg = bt_registry), use.names = TRUE)

  # mlr3mbo runtimes
  bt_job_table = getJobTable(reg = bt_registry)
  bt_job_table = bt_job_table[, runtime := as.integer(round(as.numeric(time.running, units = "secs")))]
  bt_job_table = bt_job_table[instances[, list(problem = paste0(scenario, "_", instance), dim, scenario, instance)], on = "problem"]
  set(bt_job_table, j = "algorithm", value = "mlr3mbo")

  # save runtimes
  runtimes = rbindlist(list(hq_job_table[, list(algorithm, scenario, instance, runtime, dim, repl)], bt_job_table[, list(algorithm, scenario, instance, runtime, dim, repl)]), use.names = TRUE)
  fwrite(runtimes, file = sprintf("competitors/results/%s_runtimes.csv", benchmark))

  # save archive
  results = rbindlist(list(results_competitors, results_mlr3mbo), use.names = TRUE)
  fwrite(results, sprintf("competitors/results/%s_archive.csv", benchmark))

  # average over replications
  aggr_results = results[, list(mean_incumbent = mean(incumbent)), by = c("iter", "algorithm", "scenario", "instance")]

  # determine meta score of mean incumbent
  aggr_results[, meta_score := pmap_dbl(list(mean_incumbent, scenario, instance), function(mean_incumbent, scenario, instance) {
    .scenario = scenario
    .instance = instance
    score_rs_small = rs_reference[list(.scenario, .instance), rs_small, on = c("scenario", "instance")]
    score_rs_large = rs_reference[list(.scenario, .instance), rs_large, on = c("scenario", "instance")]
    (score_rs_small - mean_incumbent) / (score_rs_small - score_rs_large)
  })]

  fwrite(aggr_results, sprintf("competitors/results/%s_aggr.csv", benchmark))

  # average over scenarios and instances
  aggr_results[, fraction_budget := iter / max(iter), by = .(algorithm, scenario, instance)]

  get_best = function(meta_scores, fraction_budget) {
    budgets = seq(0, 1, length.out = 101L)
    map_dbl(budgets, function(budget) {
      indices = which(fraction_budget <= budget)
      if (length(indices) == 0L) {
        min(meta_scores)
      } else {
        max(meta_scores[indices])
      }
    })
  }

  aggr_result_scaled = aggr_results[, list(meta_score = get_best(meta_score, fraction_budget), fraction_budget = seq(0, 1, length.out = 101L)), by = .(algorithm, scenario, instance)]
  aggr_result_scaled[, rank := rank(-meta_score), by = c("scenario", "instance", "fraction_budget")]

  data = aggr_result_scaled[, list(
    mean_meta_score = mean(meta_score),
    se_meta_score = sd(meta_score) / sqrt(.N),
    mean_rank = mean(rank),
    se_rank = sd(rank) / sqrt(.N)),
    by = c("algorithm", "fraction_budget")]

  fwrite(data, sprintf("competitors/results/%s_result.csv", benchmark))
})


