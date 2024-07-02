cluster.functions = batchtools::makeClusterFunctionsSlurm("beartooth/slurm_wyoming.tmpl", array.jobs = TRUE)
default.resources = list(walltime = 18000L, memory = 4000L, ntasks = 1L, ncpus = 1L, nodes = 1L, chunks.as.arrayjobs = TRUE, partition = "teton")
max.concurrent.jobs = 5000L
