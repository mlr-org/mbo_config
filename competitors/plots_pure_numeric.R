library(data.table)
library(ggplot2)

data_aggr = fread("competitors/results/pure_numeric_aggr.csv")
labels = fread("common/yapho_instances_pure_numeric.csv")
data_aggr = data_aggr[labels, on = c("scenario", "instance")]
data = fread("competitors/results/pure_numeric.csv")
job_table = readRDS("competitors/job_table_competitors_pure_numeric.rds")
bt_job_table = readRDS("competitors/job_table_mlr3mbo_pure_numeric.rds")
algo_colors = c("#00BA38", "#B79F00", "#F8766D", "#00BFC4", "#F564E3", "#619CFF")
algo_colors = setNames(algo_colors, c("mlr3mbo", "ax", "hebo", "optuna", "smac4hpo", "smac4bb"))
mean_runtimes_competitors = job_table[, list(mean_runtime = as.numeric(mean(runtime))), by = c("algorithm", "dim")]
mean_runtimes_mlr3mbo = bt_job_table[, list(mean_runtime = as.numeric(mean(runtime, na.rm = TRUE))), by = "dim"]
set(mean_runtimes_mlr3mbo, j = "algorithm", value = "mlr3mbo")
mean_runtimes = rbindlist(list(mean_runtimes_competitors, mean_runtimes_mlr3mbo), use.names = TRUE)
mean_runtimes[, dim := as.factor(dim)]

# mean RSNS
pdf("competitors/results/pure_numeric_mean_meta_score.pdf", width = 10, height = 10)
ggplot(data, aes(x = iter, y = mean_meta_score, color = algorithm, fill = algorithm)) +
  geom_line() +
  geom_ribbon(aes(min = mean_meta_score - se_meta_score, max = mean_meta_score + se_meta_score), colour = NA, alpha = 0.3) +
  ylim(-1, 1.5) +
  labs(x = "Iteration", y = "Mean RSNS", color = "Algorithm", fill = "Algorithm") +
  scale_color_manual(values = algo_colors) +
  scale_fill_manual(values = algo_colors) +
  theme_minimal()
dev.off()

# mean Rank
pdf("competitors/results/pure_numeric_ranking.pdf", width = 10, height = 10)
ggplot(data, aes(x = iter, y = mean_rank, color = algorithm)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_rank - se_rank, ymax = mean_rank + se_rank, fill = algorithm), colour = NA, alpha = 0.2) +
  labs(x = "Iteration", y = "Mean Rank", color = "Algorithm", fill = "Algorithm") +
  scale_color_manual(values = algo_colors) +
  scale_fill_manual(values = algo_colors) +
  theme_minimal()
dev.off()

# per task RSNS
pdf("competitors/results/pure_numeric_mean_meta_score_per_task.pdf", width = 10, height = 10)
ggplot(data_aggr, aes(x = iter, y = meta_score, color = algorithm, fill = algorithm)) +
  geom_line() +
  facet_wrap(~scenario + instance + name + dimension, labeller = function(x) pmap_dtr(x, function(scenario, instance, name, dimension) data.table(label = paste0(scenario, " ", instance, " (", name, ", ", dimension, "D)")))) +
  ylim(-1, 1.5) +
  labs(x = "Iteration", y = "Mean RSNS", color = "Algorithm", fill = "Algorithm") +
  scale_color_manual(values = algo_colors) +
  scale_fill_manual(values = algo_colors) +
  theme_minimal()
dev.off()

# runtimes
pdf("competitors/results/pure_numeric_runtimes.pdf", width = 10, height = 10)
ggplot(mean_runtimes, aes(x = dim, y = mean_runtime, fill = algorithm)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Search Space Dimension", y = "Mean Runtime [h]", fill = "Algorithm") +
  scale_color_manual(values = algo_colors) +
  scale_fill_manual(values = algo_colors) +
  theme_minimal()
dev.off()

# Figures for paper ----------------------------------------------------------------------------------------------------

# mean RSNS
pdf("competitors/results/paper/pure_numeric_mean_meta_score.pdf", width = 10, height = 10)
ggplot(data, aes(x = iter, y = mean_meta_score, color = algorithm, fill = algorithm)) +
  geom_line(show.legend = FALSE) +
  geom_ribbon(aes(min = mean_meta_score - se_meta_score, max = mean_meta_score + se_meta_score), colour = NA, alpha = 0.3, show.legend = FALSE) +
  ylim(-1, 1.5) +
  labs(x = "Iteration", y = "Mean RSNS") +
  scale_color_manual(values = algo_colors) +
  scale_fill_manual(values = algo_colors) +
  theme_minimal(base_size = 16)
dev.off()

# mean Rank
pdf("competitors/results/paper/pure_numeric_ranking.pdf", width = 10, height = 10)
ggplot(data, aes(x = iter, y = mean_rank, color = algorithm)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_rank - se_rank, ymax = mean_rank + se_rank, fill = algorithm), colour = NA, alpha = 0.2) +
  labs(x = "Iteration", y = "Mean Rank", color = "Algorithm", fill = "Algorithm") +
  scale_color_manual(values = algo_colors) +
  scale_fill_manual(values = algo_colors) +
  theme_minimal(base_size = 16)
dev.off()

# per task RSNS
pdf("competitors/results/paper/pure_numeric_mean_meta_score_per_task.pdf", width = 11.69, height = 8.27)
ggplot(data_aggr, aes(x = iter, y = meta_score, color = algorithm, fill = algorithm)) +
  geom_line() +
  facet_wrap(~scenario + instance + name + dimension, labeller = function(x) pmap_dtr(x, function(scenario, instance, name, dimension) data.table(label_1 = paste0(scenario, " ", instance), label_2 = paste0(name, ", ", dimension, "D")))) +
  ylim(-1, 1.5) +
  labs(x = "Iteration", y = "Mean RSNS", color = "Algorithm", fill = "Algorithm") +
  scale_color_manual(values = algo_colors) +
  scale_fill_manual(values = algo_colors) +
  theme_minimal(base_size = 16)
dev.off()

# runtimes
pdf("competitors/results/paper/pure_numeric_runtimes.pdf", width = 10, height = 10)
ggplot(mean_runtimes, aes(x = dim, y = mean_runtime, fill = algorithm)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Search Space Dimension", y = "Mean Runtime [h]", fill = "Algorithm") +
  scale_color_manual(values = algo_colors) +
  scale_fill_manual(values = algo_colors) +
  theme_minimal(base_size = 16)
dev.off()
