cluster.functions = batchtools::makeClusterFunctionsSlurm("/home/mbecke16/mbo_config/medbo/slurm_wyoming.tmpl", array.jobs = FALSE)
default.resources = list(walltime = 10800L, memory = 4000L, ntasks = 1L, ncpus = 1L, nodes = 1L, chunks.as.arrayjobs = FALSE, partition = "mb")
max.concurrent.jobs = 100000L
