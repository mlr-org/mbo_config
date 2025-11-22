library(data.table)
library(ggplot2)
library(scmamp)

data_aggr = fread("competitors/results/mixed_deps_aggr.csv")
labels = fread("common/yapho_instances_mixed_deps.csv")
data_aggr = data_aggr[labels, on = c("scenario", "instance")]
data = fread("competitors/results/mixed_deps.csv")
job_table = readRDS("competitors/job_table_competitors_mixed_deps.rds")
bt_job_table = readRDS("competitors/job_table_mlr3mbo_mixed_deps.rds")

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
job_table[algorithm == "smac4hpo", algorithm := "SMAC4HPO"]
job_table[algorithm == "smac4bb", algorithm := "SMAC4BB"]
job_table[algorithm == "ax", algorithm := "Ax"]
job_table[algorithm == "hebo", algorithm := "HEBO"]
job_table[algorithm == "optuna", algorithm := "Optuna"]

# define colors
algo_colors = c("#00BA38", "#B79F00", "#F8766D", "#00BFC4", "#F564E3", "#619CFF")
algo_colors = setNames(algo_colors, c("mlr3mbo", "Ax", "HEBO", "Optuna", "SMAC4HPO", "SMAC4BB"))

mean_runtimes_competitors = job_table[, list(mean_runtime = as.numeric(mean(runtime))), by = c("algorithm", "dim")]
mean_runtimes_mlr3mbo = bt_job_table[, list(mean_runtime = as.numeric(mean(runtime, na.rm = TRUE))), by = "dim"]
set(mean_runtimes_mlr3mbo, j = "algorithm", value = "mlr3mbo")
mean_runtimes = rbindlist(list(mean_runtimes_competitors, mean_runtimes_mlr3mbo), use.names = TRUE)
mean_runtimes[, dim := as.factor(dim)]

data_final = data_aggr[iter == 400]
data_final[, problem := paste0(scenario, "_", instance)]
data_final = data_final[, list(algorithm, problem, meta_score)]
data_final[, algorithm := factor(algorithm)]
data_final[, problem := factor(problem)]
data_final_wide = dcast(data_final, problem ~ algorithm, value.var = "meta_score")

# mean RSNS
pdf("competitors/results/mixed_performance.pdf", width = 10, height = 10)
ggplot(data, aes(x = iter, y = mean_meta_score, color = algorithm, fill = algorithm)) +
  geom_line() +
  geom_ribbon(aes(min = mean_meta_score - se_meta_score, max = mean_meta_score + se_meta_score), colour = NA, alpha = 0.3) +
  ylim(-1, 1.2) +
  labs(x = "Iteration", y = "Mean RSNS", color = "Algorithm", fill = "Algorithm") +
  scale_color_manual(values = algo_colors) +
  scale_fill_manual(values = algo_colors) +
  theme_minimal(base_size = 24) +
  theme(legend.position = "bottom")
dev.off()

# mean Rank
pdf("competitors/results/mixed_rank.pdf", width = 10, height = 10)
ggplot(data, aes(x = iter, y = mean_rank, color = algorithm)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_rank - se_rank, ymax = mean_rank + se_rank, fill = algorithm), colour = NA, alpha = 0.2) +
  labs(x = "Iteration", y = "Mean Rank", color = "Algorithm", fill = "Algorithm") +
  scale_color_manual(values = algo_colors) +
  scale_fill_manual(values = algo_colors) +
  theme_minimal(base_size = 24) +
  theme(legend.position = "bottom")
dev.off()

# per task RSNS
pdf("competitors/results/mixed_performance_tasks.pdf", width = 8, height = 6.5)
ggplot(data_aggr, aes(x = iter, y = meta_score, color = algorithm, fill = algorithm)) +
  geom_line() +
  facet_wrap(~scenario + instance + name + dimension, labeller = function(x) pmap_dtr(x, function(scenario, instance, name, dimension) data.table(label_1 = paste0(scenario, " ", instance), label_2 = paste0(name, ", ", dimension, "D")))) +
  ylim(-1, 1.5) +
  labs(x = "Iteration", y = "Mean RSNS", color = "Algorithm", fill = "Algorithm") +
  scale_color_manual(values = algo_colors) +
  scale_fill_manual(values = algo_colors) +
  theme_minimal(base_size = 12)
dev.off()

# runtimes
pdf("competitors/results/mixed_runtimes.pdf", width = 10, height = 10)
ggplot(mean_runtimes, aes(x = dim, y = mean_runtime, fill = algorithm)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Search Space Dimension", y = "Mean Runtime in Hours", fill = "Algorithm") +
  scale_color_manual(values = algo_colors) +
  scale_fill_manual(values = algo_colors) +
  theme_minimal(base_size = 24) +
  theme(legend.position = "bottom")
dev.off()

# critical difference plot
pdf("competitors/results/mixed_cd.pdf", width = 10, height = 5)
plotCD(data_final_wide[, list(mlr3mbo, SMAC4HPO, Optuna)], alpha = 0.05, cex = 2)
dev.off()
