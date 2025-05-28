library(batchtools)
library(brew)
library(mlr3misc)
library(data.table)

# n_jobs is the number of jobs per node
submit_ncar = function(job_ids, reg, template, n_jobs = 128L) {
  chunks = split(job_ids, ceiling(seq_along(job_ids) / n_jobs))

  time = format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

  walk(chunks, function(chunk) {
    env = new.env()
    set(reg$status, i = chunk, j = "started", value = NA_integer_)
    set(reg$status, i = chunk, j = "done", value = NA_integer_)
    set(reg$status, i = chunk, j = "error", value = NA)

    hash = sprintf("%s_%s_%s", time, chunk[1L], chunk[length(chunk)])

    assign("job.name", sprintf("job_%s", hash), env = env)
    assign("log.file", sprintf("/glade/derecho/scratch/lschneider/log_nodes/job_%s.log", hash), env = env)

    iwalk(chunk, function(id, i) {
        jc = makeJobCollection(id)
        saveRDS(jc, jc$uri)
        assign(sprintf("uri_%i", i), jc$uri, env = env)
    })

    # fix if not 128 jobs
    if (length(chunk) < n_jobs) {
      walk(seq(length(chunk) + 1L, n_jobs), function(i)  assign(sprintf("uri_%i", i), "", env = env))
    }

    tmp = tempfile()
    brew(template, output = tmp, envir = env)
    batch.id = system2("qsub", tmp, stdout = TRUE)

    message(batch.id)

    set(reg$status, i = chunk, j = "log.file", value = sprintf("%s.log", chunk))
    set(reg$status, i = chunk, j = "job.name", value = sprintf("job_%s", hash))
    set(reg$status, i = chunk, j = "submitted", value = Sys.time())
    set(reg$status, i = chunk, j = "batch.id", value = batch.id)
    saveRegistry(reg)
  })

  return(job_ids)
}


