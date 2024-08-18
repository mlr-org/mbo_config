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

  scores = replicate(5000, {
    y_200 = sample(y, 200, replace = TRUE)
    score = if (job$problem$data$args$target %in% c("logloss", "val_cross_entropy")) {
      min(y_200)
    } else {
      max(y_200)
    }
  })

  rs_200 = data.table(
    job_id = job$job.id,
    problem = job$problem$name,
    instance = job$problem$data$args$instance,
    scenario = job$problem$data$args$scenario,
    target = job$problem$data$args$target,
    mean_score = mean(scores)
  )

  score = if (job$problem$data$args$target %in% c("logloss", "val_cross_entropy")) {
    min(y)
  } else {
    max(y)
  }

  rs = data.table(
    job_id = job$job.id,
    problem = job$problem$name,
    instance = job$problem$data$args$instance,
    scenario = job$problem$data$args$scenario,
    target = job$problem$data$args$target,
    score = score
  )

  list(rs_200 = rs_200, rs = rs)
})

rs_200 = rbindlist(lapply(results, function(x) x$rs_200))
rs = rbindlist(lapply(results, function(x) x$rs))

fwrite(rs_200, "random_search/random_search_200_results.csv")
fwrite(rs, "random_search/random_search_results.csv")

# rs_result =  fread("random_search/random_search_results.csv")
# rs_200_result = fread("random_search/random_search_200_results.csv")

# result = rs_result[rs_200_result, , on = "problem"]
# result[, equal_score := score == i.score]
# result[, equal_score := any(.SD$equal_score), by = .(scenario, instance)]

# instances = result[!(equal_score), list(instance, scenario)]
# instances[, instance := as.character(instance)]

# fwrite(instances, "random_search/rbv2_instances.csv")
