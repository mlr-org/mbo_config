library(data.table)
library(ggplot2)
library(scmamp)
library(mlr3misc)

walk(c("numeric", "mixed"), function(benchmark) {

  data = fread(sprintf("competitors_2/results/%s_result.csv", benchmark))
  data_aggr = fread(sprintf("competitors_2/results/%s_aggr.csv", benchmark), colClasses = c("instance" = "character"))
  instances = fread(sprintf("common/%s_instances.csv", benchmark), colClasses = c("instance" = "character"))
  data_aggr = data_aggr[instances, on = c("scenario", "instance")]
  runtimes = fread(sprintf("competitors_2/results/%s_runtimes.csv", benchmark))
  runtimes[, dim := as.factor(dim)]

  # rename algorithms
  data[algorithm == "smac4hpo", algorithm := "SMAC4HPO"]
  data[algorithm == "smac4bb", algorithm := "SMAC4BB"]
  data[algorithm == "ax", algorithm := "Ax"]
  data[algorithm == "hebo", algorithm := "HEBO"]
  data[algorithm == "optuna", algorithm := "Optuna"]
  data_aggr[algorithm == "smac4hpo", algorithm := "SMAC4HPO"]
  data_aggr[algorithm == "smac4bb", algorithm := "SMAC4BB"]
  data_aggr[algorithm == "ax", algorithm := "Ax"]
  data_aggr[algorithm == "hebo", algorithm := "HEBO"]
  data_aggr[algorithm == "optuna", algorithm := "Optuna"]
  runtimes[algorithm == "smac4hpo", algorithm := "SMAC4HPO"]
  runtimes[algorithm == "smac4bb", algorithm := "SMAC4BB"]
  runtimes[algorithm == "ax", algorithm := "Ax"]
  runtimes[algorithm == "hebo", algorithm := "HEBO"]
  runtimes[algorithm == "optuna", algorithm := "Optuna"]

  # define colors
  algo_colors = c("#00BA38", "#B79F00", "#F8766D", "#00BFC4", "#F564E3", "#619CFF")
  algo_colors = setNames(algo_colors, c("mlr3mbo", "Ax", "HEBO", "Optuna", "SMAC4HPO", "SMAC4BB"))

  # final meta score
  data_final = data_aggr[, tail(.SD, 1), by = .(algorithm, scenario, instance)]
  data_final[, problem := paste0(scenario, "_", instance)]
  data_final = data_final[, list(algorithm, problem, meta_score)]
  data_final[, algorithm := factor(algorithm)]
  data_final[, problem := factor(problem)]
  data_final = dcast(data_final, problem ~ algorithm, value.var = "meta_score")

  # mean runtime
  mean_runtimes = runtimes[, list(mean_runtime = mean(runtime) / 60), by = c("algorithm", "dim")]

  # mean RSNS
  pdf(sprintf("competitors_2/results/%s_performance.pdf", benchmark), width = 10, height = 10)
  gg = ggplot(data, aes(x = fraction_budget, y = mean_meta_score, color = algorithm, fill = algorithm)) +
    geom_line() +
    geom_ribbon(aes(min = mean_meta_score - se_meta_score, max = mean_meta_score + se_meta_score), colour = NA, alpha = 0.3) +
    ylim(-1, 1.5) +
    labs(x = "Fraction of Budget", y = "Mean RSNS", color = "Algorithm", fill = "Algorithm") +
    scale_color_manual(values = algo_colors) +
    scale_fill_manual(values = algo_colors) +
    theme_minimal(base_size = 22) +
    theme(legend.position = "bottom")
  print(gg)
  dev.off()

  # mean rank
  pdf(sprintf("competitors_2/results/%s_rank.pdf", benchmark), width = 10, height = 10)
  gg = ggplot(data, aes(x = fraction_budget, y = mean_rank, color = algorithm)) +
    geom_line() +
    geom_ribbon(aes(ymin = mean_rank - se_rank, ymax = mean_rank + se_rank, fill = algorithm), colour = NA, alpha = 0.2) +
    labs(x = "Fraction of Budget", y = "Mean Rank", color = "Algorithm", fill = "Algorithm") +
    scale_color_manual(values = algo_colors) +
    scale_fill_manual(values = algo_colors) +
    theme_minimal(base_size = 22) +
    theme(legend.position = "bottom")
  print(gg)
  dev.off()

  # per task RSNS
  pdf(sprintf("competitors_2/results/%s_performance_tasks.pdf", benchmark), width = 8, height = 6.5)
  gg = ggplot(data_aggr, aes(x = iter, y = meta_score, color = algorithm, fill = algorithm)) +
    geom_line() +
    facet_wrap(~scenario + instance + name + dim, scales = "free_x", labeller = function(x) pmap_dtr(x, function(scenario, instance, name, dim) data.table(label_1 = paste0(scenario, " ", instance), label_2 = paste0(name, ", ", dim, "D")))) +
    ylim(-1, 1.5) +
    labs(x = "Iteration", y = "Mean RSNS", color = "Algorithm", fill = "Algorithm") +
    scale_color_manual(values = algo_colors) +
    scale_fill_manual(values = algo_colors) +
    theme_minimal(base_size = 12)
  print(gg)
  dev.off()

  # runtimes
  pdf(sprintf("competitors_2/results/%s_runtimes.pdf", benchmark), width = 10, height = 10)
  gg = ggplot(mean_runtimes, aes(x = dim, y = mean_runtime, fill = algorithm)) +
    geom_bar(position = "dodge", stat = "identity") +
    labs(x = "Search Space Dimension", y = "Mean Runtime in Minutes", fill = "Algorithm") +
    scale_fill_manual(values = algo_colors) +
    theme_minimal(base_size = 22) +
    theme(legend.position = "bottom")
  print(gg)
  dev.off()

  # critical difference 
  pdf(sprintf("competitors_2/results/%s_cd.pdf", benchmark), width = 10, height = 5)
  plotCD(data_final[, -c("problem")], alpha = 0.05, cex = 1.8)
  dev.off()
})

