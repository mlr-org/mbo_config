library(batchtools)
library(data.table)
library(mlr3misc)

pwalk(list(
  registry_name = c("/glade/derecho/scratch/lschneider/yahpo_pure_numeric_rs", "/glade/derecho/scratch/lschneider/yahpo_mixed_deps_rs"),
  benchmark = c("numeric", "mixed")
), function(registry_name, benchmark) {

  set.seed(7832)

  reg = loadRegistry(registry_name)

  instances = fread(sprintf("common/%s_instances.csv", benchmark), colClasses = c("instance" = "character"))
  instances[, budget_20 := as.integer(20 + 40 * sqrt(dim))]
  instances[, budget_100 := as.integer(100 + 40 * sqrt(dim))]

  # read in results from large random search
  results = reduceResultsList(findDone(), function(result, job) {
    data = result$archive$data
    pars = job$pars
    target_variable = pars$prob.pars$target_variable
    tmp = data[, eval(target_variable), with = FALSE]
    colnames(tmp) = "target"
    tmp[, orig_direction := pars$prob.pars$direction]
    if (pars$prob.pars$direction == "maximize") {
      tmp[, target := - target]
    }
    tmp[, best := cummin(target)]
    tmp[, scenario := pars$prob.pars$scenario]
    tmp[, instance := pars$prob.pars$instance]
    tmp[, target_variable := pars$prob.pars$target_variable]
    tmp[, repl := job$repl]
    tmp[, iter := seq_len(.N)]
    tmp
  })
  results = rbindlist(results, fill = TRUE)

  fwrite(results, sprintf("random_search/results/%s_rs_archive.csv", benchmark))

  # simulate small random search
  results_simulated_20 = pmap_dtr(instances, function(scenario, instance, target_variable, budget_20, ...) {
    .scenario = scenario
    .instance = instance

    tmp = results[list(.scenario, .instance), on = c("scenario", "instance")]

    map_dtr(seq_len(30L), function(repl) {
      subset = tmp[sample(.N, size = budget_20, replace = FALSE), ]
      subset[, best := cummin(target)]
      subset[, repl := repl]
      subset[, iter := seq_len(.N)]
      subset
    })
  })

  fwrite(results_simulated_20, sprintf("random_search/results/%s_rs_simulated_20.csv", benchmark))

  results_simulated_100 = pmap_dtr(instances, function(scenario, instance, target_variable, budget_100, ...) {
    .scenario = scenario
    .instance = instance

    tmp = results[list(.scenario, .instance), on = c("scenario", "instance")]

    map_dtr(seq_len(30L), function(repl) {
      subset = tmp[sample(.N, size = budget_100, replace = FALSE), ]
      subset[, best := cummin(target)]
      subset[, repl := repl]
      subset[, iter := seq_len(.N)]
      subset
    })
  })

  fwrite(results_simulated_100, sprintf("random_search/results/%s_rs_simulated_100.csv", benchmark))

  # extract best value from small and large random search
  results_reference_20 = pmap_dtr(instances, function(scenario, instance, target_variable, budget_20, ...) {
    .scenario = scenario
    .instance = instance
    tmp_small = results_simulated_20[list(.scenario, .instance, budget_20), , on = c("scenario", "instance", "iter")][, list(rs_small = mean(best), se_rs_small = sd(best) / sqrt(.N)), by = c("scenario", "instance", "target_variable", "orig_direction")]
    tmp_large = results[list(.scenario, .instance, 1e6L), list(rs_large = best), on = c("scenario", "instance", "iter")]
    cbind(tmp_small, tmp_large)
  })

  fwrite(results_reference_20, sprintf("random_search/results/%s_rs_reference_20.csv", benchmark))

  results_reference_100 = pmap_dtr(instances, function(scenario, instance, target_variable, budget_100, ...) {
    .scenario = scenario
    .instance = instance
    tmp_small = results_simulated_100[list(.scenario, .instance, budget_100), , on = c("scenario", "instance", "iter")][, list(rs_small = mean(best), se_rs_small = sd(best) / sqrt(.N)), by = c("scenario", "instance", "target_variable", "orig_direction")]
    tmp_large = results[list(.scenario, .instance, 1e6L), list(rs_large = best), on = c("scenario", "instance", "iter")]
    cbind(tmp_small, tmp_large)
  })

  fwrite(results_reference_100, sprintf("random_search/results/%s_rs_reference_100.csv", benchmark))
})



