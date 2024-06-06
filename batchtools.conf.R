cluster.functions = batchtools::makeClusterFunctionsSlurm("slurm_wyoming.tmpl", array.jobs = TRUE)
default.resources = list(walltime = 32400L, memory = 4000L, ntasks = 1L, ncpus = 1L, nodes = 1L, chunks.as.arrayjobs = TRUE, partition = "mb")
max.concurrent.jobs = 100000L
