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
  instances[, budget := as.integer(20 + 40 * sqrt(dim))]

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
  results_simulated = pmap_dtr(instances, function(scenario, instance, target_variable, budget, ...) {
    .scenario = scenario
    .instance = instance

    tmp = results[list(.scenario, .instance), on = c("scenario", "instance")]

    map_dtr(seq_len(30L), function(repl) {
      subset = tmp[sample(.N, size = budget, replace = FALSE), ]
      subset[, best := cummin(target)]
      subset[, repl := repl]
      subset[, iter := seq_len(.N)]
      subset
    })
  })

  fwrite(results_simulated, sprintf("random_search/results/%s_rs_simulated.csv", benchmark))

  results_simulated_400 = pmap_dtr(instances, function(scenario, instance, target_variable, budget, ...) {
    .scenario = scenario
    .instance = instance

    tmp = results[list(.scenario, .instance), on = c("scenario", "instance")]

    map_dtr(seq_len(30L), function(repl) {
      subset = tmp[sample(.N, size = 400L, replace = FALSE), ]
      subset[, best := cummin(target)]
      subset[, repl := repl]
      subset[, iter := seq_len(.N)]
      subset
    })
  })

  fwrite(results_simulated_400, sprintf("random_search/results/%s_rs_simulated_400.csv", benchmark))

  # extract best value from small and large random search
  results_reference = pmap_dtr(instances, function(scenario, instance, target_variable, budget, ...) {
    .scenario = scenario
    .instance = instance
    tmp_small = results_simulated[list(.scenario, .instance, budget), , on = c("scenario", "instance", "iter")][, list(rs_small = mean(best), se_rs_small = sd(best) / sqrt(.N)), by = c("scenario", "instance", "target_variable", "orig_direction")]
    tmp_large = results[list(.scenario, .instance, 1e6L), list(rs_large = best), on = c("scenario", "instance", "iter")]
    cbind(tmp_small, tmp_large)
  })

  fwrite(results_reference, sprintf("random_search/results/%s_rs_reference.csv", benchmark))

  results_reference_400 = pmap_dtr(instances, function(scenario, instance, target_variable, budget, ...) {
    .scenario = scenario
    .instance = instance
    tmp_small = results_simulated_400[list(.scenario, .instance, budget), , on = c("scenario", "instance", "iter")][, list(rs_small = mean(best), se_rs_small = sd(best) / sqrt(.N)), by = c("scenario", "instance", "target_variable", "orig_direction")]
    tmp_large = results[list(.scenario, .instance, 1e6L), list(rs_large = best), on = c("scenario", "instance", "iter")]
    cbind(tmp_small, tmp_large)
  })

  fwrite(results_reference_400, sprintf("random_search/results/%s_rs_reference_400.csv", benchmark))
})



