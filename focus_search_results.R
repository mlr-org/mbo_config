# renv::load(".")

library(batchtools)
library(data.table)
library(mlr3misc)
library(bbotk)
library(ggplot2)
library(gridExtra)

setDTthreads(48)

reg = loadRegistry("/gscratch/mbecke16/registry_focus_search", writeable = TRUE)

job_table = getJobTable()
done = findDone()

walk(reg$problems, function(.problem) {
  message(.problem)
  job_ids = job_table[list(.problem), job.id, on = "problem"]
  job_ids = intersect(job_ids, done$job.id)

  results = map(job_ids, function(id) {
    message(id)
    job = makeJob(id)
    result = loadResult(id)
    message("loaded")
    data = result$data
    target = result$target
    tab = data[, target, with = FALSE]
    tab[, best := cummax(target), env = list(target = target)]
    tab[, problem := job$prob.name]
    tab[, repl := job$repl]
    tab[, iter := .I]
    rm(data)
    rm(target)
    gc()
    tab
  })

  results = rbindlist(results, fill = TRUE)
  results_average = results[, list(mean_best = mean(best), se_best = sd(best) / sqrt(.N)), by = list(problem, iter)]
  saveRDS(results_average, sprintf("/gscratch/mbecke16/mbo_config/focus_search/focus_search_average_%s.rds", .problem))

  # extrapolation
  tmp = results_average
  values = tail(unique(tmp$mean_best), 2L)
  dat = map_dtr(values, function(value) {
    tmp[mean_best == value][.N, ]
  })
  model = lm(mean_best ~ iter, data = dat)
  coefs = coef(model)
  if (coefs[2L] < .Machine$double.eps) {
    message("Almost constant linear model")
  }
  max_iter = max(tmp$iter)
  estimate_iter = function(mean_best) {
    iter = ceiling((mean_best - coefs[1L]) / coefs[2L])
    if (!isTRUE(iter > max_iter)) {
      iter = max_iter + 1
    }
    iter
  }
  env = new.env()
  environment(estimate_iter) = env
  assign("max_iter", value = max_iter, envir = env)
  assign("coefs", value = coefs, envir = env)
  if (estimate_iter(1.00001 * max(tmp$mean_best)) < max(tmp$iter)) {
    # marginal improvements should require more iter than max iter
    message("Model does not interpolate latest iter well.")
  }
  tmp_p = data.table(iter = ((NROW(tmp) - 1L):ceiling(1.5 * NROW(tmp))))
  p = predict(model, tmp_p)
  tmp_p[, mean_best := p]
  tmp_p[, method := "interpolation"]
  tmp_plot = copy(tmp)[, c("iter", "mean_best")]
  tmp_plot[, method := "real"]
  tmp_plot = rbind(tmp_plot, tmp_p)
  g = ggplot(aes(x = iter, y = mean_best, colour = method), data = tmp_plot[ceiling(0.9 * NROW(tmp)):.N, ]) +
    scale_y_log10() +
    geom_step(direction = "vh") +
    geom_hline(yintercept = max(tmp$mean_best), linetype = 2L) +
    labs(title = .problem) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave(sprintf("/gscratch/mbecke16/mbo_config/focus_search/focus_search_extrapolation_%s.pdf", .problem))

  saveRDS(list(problem = .problem, coefs = coefs, max_iter = max_iter), sprintf("/gscratch/mbecke16/mbo_config/focus_search/focus_search_extrapolation_%s.rds", .problem))
})

