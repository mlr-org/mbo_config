options(width = 300)

job_table = getJobTable()
algo_pars = names(job_table$algo.pars[[1]])
job_table = unnest(job_table, "algo.pars")
cns = setdiff(c("problem", "time.running", algo_pars), c("id", "config_hash"))
head(job_table[, cns, with = FALSE][order(time.running, decreasing = TRUE)], 100)

errors = findErrors()

job_table[is.na(time.running)][, cns, with = FALSE]
