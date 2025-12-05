library(data.table)
library(mlr3misc)
library(batchtools)

pwalk(list(
  benchmark = c("numeric", "budget_mixed"), 
  rs_reference = c("numeric", "mixed")),
  function(benchmark, rs_reference) {

  registry_name = sprintf("/glade/derecho/scratch/marcbecker/mbo_config/registries/competitors_mlr3mbo_%s_variance", benchmark)

  reg = loadRegistry(registry_name, writeable = FALSE)
  reg$cluster.functions = makeClusterFunctionsHyperQueue()

  results = rbindlist(reduceResultsList(fun = function(archive, job) {
    data.table(
      algorithm = "mlr3mbo",
      scenario = job$instance$scenario,
      instance = job$instance$instance,
      best = - max(archive[[job$instance$target_variable]]),
      repl = job$repl
    )
  }, missing.val = NULL, reg = reg), use.names = TRUE)

  fwrite(results, file = sprintf("variance_best/results/%s_best.csv", benchmark))

  results = fread(sprintf("variance_best/results/%s_best.csv", benchmark), colClasses = c("scenario" = "character", "instance" = "character"))

  rs_reference = fread(sprintf("random_search/results/%s_rs_reference_100.csv", rs_reference), colClasses = c("scenario" = "character", "instance" = "character"))

  results_simulated = map_dbl(seq(1000), function(i) {
    result_i = results[, .SD[sample(.N, size = 30, replace = FALSE)], by = c("scenario", "instance")]
    aggr_result_i = result_i[, list(mean_best = mean(best)), by = c("scenario", "instance")]
    aggr_result_i[, meta_score := pmap_dbl(list(mean_best, scenario, instance), function(mean_best, scenario, instance) {
      .scenario = scenario
      .instance = instance
      score_rs_small = rs_reference[list(.scenario, .instance), rs_small, on = c("scenario", "instance")]
      score_rs_large = rs_reference[list(.scenario, .instance), rs_large, on = c("scenario", "instance")]
      (score_rs_small - mean_best) / (score_rs_small - score_rs_large)
    })]
    mean(aggr_result_i$meta_score)
  })

  fwrite(data.table(meta_score = results_simulated), file = sprintf("variance_best/results/%s_simulated.csv", benchmark))

  pdf(sprintf("variance_best/results/%s_simulated.pdf", benchmark))
    ggplot(data.table(meta_score = results_simulated), aes(x = meta_score)) +
      geom_histogram(binwidth = 0.01) +
      theme_minimal() +
      labs(x = "Meta Score", y = "Count")
  dev.off()
})

budget_mixed_simulated = fread("variance_best/results/budget_mixed_simulated.csv")

mean(budget_mixed_simulated$meta_score)
sd(budget_mixed_simulated$meta_score)

numeric_simulated = fread("variance_best/results/numeric_simulated.csv")

mean(numeric_simulated$meta_score)
sd(numeric_simulated$meta_score)


