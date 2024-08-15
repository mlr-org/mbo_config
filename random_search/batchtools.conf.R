cluster.functions = batchtools::makeClusterFunctionsSlurm("/home/mbecke16/mbo_config/slurm_medbo.tmpl", array.jobs = TRUE)
default.resources = list(walltime = 3600L * 4L, memory = 7000L, ntasks = 1L, ncpus = 1L, nodes = 1L, chunks.as.arrayjobs = TRUE, partition = "mb")
max.concurrent.jobs = 100000L
