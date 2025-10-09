library(batchtools)
library(data.table)

reg = loadRegistry(
  file.dir = "/glade/derecho/scratch/marcbecker/mbo_config/acquisition_optimizer_pure_numeric"
)

results = rbindlist(reduceResultsList(reg = reg, fun = function(job, res) {
  print(job$prob.name)
  set(setDT(res), j = "problem", value = job$prob.name)
  set(res, j = "acqopt", value = NULL)
  res
}))

fwrite(results, "/glade/u/home/marcbecker/mbo_config/acquisiton_optimizer/results_pure_numeric.csv")