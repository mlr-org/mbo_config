library(batchtools)
library(data.table)

reg = loadRegistry("/glade/derecho/scratch/lschneider/yahpo_mixed_deps_rs")

instances_desc = mlr3misc::rowwise_table(
  ~scenario, ~instance, ~target_variable,
  "lcbench", "167168", "val_accuracy",
  "lcbench", "189873", "val_accuracy",
  "lcbench", "189906", "val_accuracy",
  "nb301", "CIFAR10", "val_accuracy",
  "rbv2_rpart", "14", "acc",
  "rbv2_rpart", "40499", "acc",
  "rbv2_ranger", "16", "acc",
  "rbv2_ranger", "42", "acc",
  "rbv2_xgboost", "12", "acc",
  "rbv2_xgboost", "1501", "acc",
  "rbv2_xgboost", "16", "acc",
  "rbv2_super", "1457", "acc",
  "rbv2_super", "1063", "acc",
  "rbv2_super", "15", "acc")

job_table = getJobTable()

pwalk(instances_desc, function(scenario, instance, target_variable) {
  problem_id = sprintf("%s_%s_%s", scenario, instance, target_variable)
  job_id = job_table[problem == problem_id, job.id]
  instance = loadResult(job_id)
  data = instance$archive$data
  set(data, j = "x_domain", value = NULL)
  set(data, j = "batch_nr", value = NULL)
  fwrite(data, file = sprintf("/glade/work/marcbecker/mbo_config/random_search/archive/mixed_deps/%s.csv", problem_id))
})



