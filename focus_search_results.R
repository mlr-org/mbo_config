# renv::load(".")

library(batchtools)
library(data.table)
library(mlr3misc)
library(bbotk)

setDTthreads(48)

reg = loadRegistry("/gscratch/mbecke16/registry_focus_search")

done = findDone()
results = reduceResultsList(done, function(result, job) {
  message(job$job.id)
  data = result$data
  target = result$target
  tab = data[, target, with = FALSE]
  tab[, best := cummax(target), env = list(target = target)]
  tab[, problem := job$prob.name]
  tab[, repl := job$repl]
  tab[, iter := .I]
  rm(data)
  rm(target)
  gc()
  tab
})

message("Save results")

results = rbindlist(results, fill = TRUE)
saveRDS(results, "/gscratch/mbecke16/mbo_config/results_focus_search.rds")

results_average = results[, .(mean_best = mean(best), se_best = sd(best) / sqrt(.N)), by = .(problem, iter)]
saveRDS(results_average, "/gscratch/mbecke16/mbo_config/results_focus_search_average.rds")
