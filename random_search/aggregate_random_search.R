library(batchtools)
library(mlr3misc)
library(data.table)

reg = loadRegistry(
  file.dir = "/gscratch/mbecke16/mbo_config/registry_random_search",
  conf.file = "random_search/batchtools.conf.R",
)

# best 200
results = rbindlist(reduceResultsList(fun = function(archive, job) {
  print(job$job.id)
  data_best = imap_dtr(seq(1, 1e6, by = 200), function(i, j) {
    data = archive[seq(i, i + 199)]

    y = data[[job$problem$data$args$target]]
    score = if (job$problem$data$args$target == "logloss") {
      min(y)
    } else {
      max(y)
    }
    data.table(
      job_id = job$job.id,
      replication = j,
      problem = job$problem$name,
      instance = job$problem$data$args$instance,
      scenario = job$problem$data$args$scenario,
      target = job$problem$data$args$target,
      score = score
    )
  })
  data_best[, .(score = mean(score)), by = .(job_id, problem, instance, scenario, target)]
}))

fwrite(results, "random_search/random_search_200_results.csv")

# best 
results = rbindlist(reduceResultsList(fun = function(archive, job) {
  print(job$job.id)

  y = archive[[job$problem$data$args$target]]
  score = if (job$problem$data$args$target == "logloss") {
    min(y)
  } else {
    max(y)
  }
  data.table(
    job_id = job$job.id,
    problem = job$problem$name,
    instance = job$problem$data$args$instance,
    scenario = job$problem$data$args$scenario,
    target = job$problem$data$args$target,
    score = score
  )
}))

fwrite(results, "random_search/random_search_results.csv")

rs_result =  fread("random_search/random_search_results.csv")
rs_200_result = fread("random_search/random_search_200_results.csv")

result = rs_result[rs_200_result, , on = "problem"]
result[, equal_score := score == i.score]
result[, equal_score := any(.SD$equal_score), by = .(scenario, instance)]

instances = result[!(equal_score), list(instance, scenario)]
instances[, instance := as.character(instance)]

fwrite(instances, "random_search/rbv2_instances.csv")
