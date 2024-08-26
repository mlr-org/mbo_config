library(batchtools)
library(mlr3misc)
library(data.table)

reg = loadRegistry(
  file.dir = "/gscratch/mbecke16/mbo_config/registry_random_search",
  conf.file = "random_search/batchtools.conf.R",
)

results = reduceResultsList(fun = function(archive, job) {
  print(job$job.id)

  y = archive[[job$problem$data$args$target]]

  if (job$problem$data$args$target %nin% c("logloss", "val_cross_entropy")) {
    y = -y
  }

  scores = replicate(5000, min(sample(y, 200, replace = TRUE)))

  rs_200 = data.table(
    job_id = job$job.id,
    problem = job$problem$name,
    instance = job$problem$data$args$instance,
    scenario = job$problem$data$args$scenario,
    target = job$problem$data$args$target,
    mean_score = mean(scores),
    sd_score = sd(scores)
  )

  quantiles = quantile(y, c(0.001, 0.01, 0.05, 0.1, 0.2))

  rs = data.table(
    job_id = job$job.id,
    problem = job$problem$name,
    instance = job$problem$data$args$instance,
    scenario = job$problem$data$args$scenario,
    target = job$problem$data$args$target,
    score =  min(y),
    quantiles_001 = quantiles[1],
    quantiles_01 = quantiles[2],
    quantiles_05 = quantiles[3],
    quantiles_10 = quantiles[4],
    quantiles_20 = quantiles[5]
  )

  list(rs_200 = rs_200, rs = rs)
})

rs_200 = rbindlist(lapply(results, function(x) x$rs_200))
rs = rbindlist(lapply(results, function(x) x$rs))

fwrite(rs_200, "random_search/raw_random_search_200_results.csv")
fwrite(rs, "random_search/raw_random_search_results.csv")

rs_result =  fread("random_search/raw_random_search_results.csv")
rs_200_result = fread("random_search/raw_random_search_200_results.csv")

result = rs_result[rs_200_result, , on = c("problem", "instance", "scenario", "target", "job_id")]
result[target == "val_accuracy", score := score / 100]
result[target == "val_accuracy", quantiles_01 := quantiles_01 / 100]

# instance auswahl: alles minimierung. differenz von min(y) zu quantile(y, 0.1) >= 0.001 --> das sind die instanzen, die wir nehmen

result[, benchmark := "lcbench"]
result[grep("rbv2", scenario), benchmark := "rbv2"]

# 3285 instances
result[, diff := quantiles_01 - score]

# 2158 instances
result = result[diff >= 0.01]

# 1315 instances
result[, n := .N , by = .(instance, scenario)]
result = result[(benchmark == "rbv2" & n == 4) | (scenario == "lcbench" & n == 3),]

# 336 instances
instances = unique(result[, list(instance, scenario)])
instances[, instance := as.character(instance)]

# validation instances

library(jsonlite)

validation = read_json("random_search/validation_instances.json")
validation = rbindlist(validation)[, list(scenario, instance)]
setcolorder(validation, c("instance", "scenario"))

# 328
instances = data.table::fsetdiff(instances, validation, all = TRUE)
fwrite(instances, "random_search/instances.csv")


# fix random search results
rs_result =  fread("random_search/raw_random_search_results.csv")
rs_200_result = fread("random_search/raw_random_search_200_results.csv")

rs_result = rs_result[, list(problem, instance, scenario, target, score)]
rs_result[target == "acc", score := -score]
rs_result[target == "bac", score := -score]
rs_result[target == "auc", score := -score]
rs_result[target == "val_balanced_accuracy", score := -score]
rs_result[target == "val_accuracy", score := -score]

rs_200_result = rs_200_result[, list(problem, instance, scenario, target, mean_score)]
rs_200_result[target == "acc", mean_score := -mean_score]
rs_200_result[target == "bac", mean_score := -mean_score]
rs_200_result[target == "auc", mean_score := -mean_score]
rs_200_result[target == "val_balanced_accuracy", mean_score := -mean_score]
rs_200_result[target == "val_accuracy", mean_score := -mean_score]

fwrite(rs_200_result, "random_search/random_search_200_results.csv")
fwrite(rs_result, "random_search/random_search_results.csv")
