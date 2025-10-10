library(batchtools)
library(brew)
library(data.table)

#' @title Submit Jobs To Nodes
#' 
#' @description
#' Submits more than 1 batchtools job to the same node.
#' The slurm controles sees one job per node.
#' Multiple batchtools jobs run in the same slurm job.
#' Batchtools jobs can be further aggregated into chunks.
#' Useful when individual batchtools jobs finish fast but we want to keep the node as long as possible.
#' 
#' @param job_ids `integer`\cr
#'   Job IDs to submit.
#' @param reg `ExperimentRegistry`\cr
#'   Experiment registry.
#' @param template `character`\cr
#'   Template file to use.
#'   The template file must start `jobs_per_node` R sessions.
#' @param jobs_per_node `integer`\cr
#'   Number of jobs per node.
#' @param chunk_size `integer`\cr
#'   Chunk size.
#' @param max_concurrent_nodes `integer`\cr
#'   Maximum number of concurrent nodes.
#'   No more than `max_concurrent_nodes` slurm jobs are submitted at the same time.
#'   LRZ cm4 nodes have a limit of 25 slurm jobs in the queue.
#'   Maximum 4 slurm jobs are running at the same time.
#' @param log_dir `character`\cr
#'   Log directory.
#' @param log_prefix `character`\cr
#'   Log prefix. 
#'   Defaults to "job".
#' @param shuffle `logical`\cr
#'   Shuffle job IDs.
#'   Defaults to FALSE.
submit = function(
  job_ids, 
  reg, 
  template, 
  jobs_per_node = 128L, 
  chunk_size = 1L, 
  max_concurrent_nodes = 5000L,  
  log_dir = ".", 
  log_prefix = "job", 
  shuffle = FALSE
  ) {
  if (shuffle) {
    job_ids = sample(job_ids)
  }

  job_chunks = split(job_ids, ceiling(seq_along(job_ids) / chunk_size))
  node_chunks = split(job_chunks, ceiling(seq_along(job_chunks) / jobs_per_node))

  time = format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

  n_nodes = min(max_concurrent_nodes, length(node_chunks))
  mlr3misc::iwalk(unname(node_chunks[1:n_nodes]), function(node_chunk, i) {
    env = new.env()
    set(reg$status, i = unlist(node_chunk), j = "started", value = NA_integer_)
    set(reg$status, i = unlist(node_chunk), j = "done", value = NA_integer_)
    set(reg$status, i = unlist(node_chunk), j = "error", value = NA)

    job_name = sprintf("%s_node_%i", log_prefix, i)

    assign("job.name", job_name, env = env)
    assign("log.file", sprintf("%s/%s.log", log_dir, job_name), env = env)

    mlr3misc::iwalk(unname(node_chunk), function(ids, i) {
        jc = makeJobCollection(ids)
        saveRDS(jc, jc$uri)
        assign(sprintf("uri_%i", i), jc$uri, env = env)
    })

    # fix if not n_jobs jobs
    if (length(node_chunk) < jobs_per_node) {
      mlr3misc::walk(seq(length(node_chunk) + 1L, jobs_per_node), function(i)  assign(sprintf("uri_%i", i), "", env = env))
    }

    tmp = tempfile()
    brew(template, output = tmp, envir = env)
    batch.id = system2("qsub", tmp, stdout = TRUE)

    message(batch.id)

    set(reg$status, i = unlist(node_chunk), j = "log.file", value = sprintf("%s.log", job_name))
    set(reg$status, i = unlist(node_chunk), j = "job.name", value = job_name)
    set(reg$status, i = unlist(node_chunk), j = "submitted", value = Sys.time())
    set(reg$status, i = unlist(node_chunk), j = "batch.id", value = batch.id)
    saveRegistry(reg)
  })

  return(unname(unlist(node_chunks[1:n_nodes])))
}

