library(data.table)
library(ggplot2)

data = fread("coordinate_descent/results/archive_mixed_deps.csv")

data_2 = melt(
  data,
  id.vars = "mean_meta_score",
  measure.vars = c("input_trafo", "output_trafo", "init", "init_size_fraction", "random_interleave_iter", "trees", "variance_estimator", "acqf", "lambda", "acqopt", "epsilon_decay", "lambda_decay", "acqopt"),
  variable.name = "parameter",
  value.name = "value"
)

pdf("coordinate_descent/results/mixed_deps_boxplots.pdf", width = 10, height = 10)
ggplot(data_2, aes(x = mean_meta_score, y = value)) +
  geom_boxplot() +
  facet_wrap(~ parameter, scales = "free_x") +
   coord_flip() +
  theme_minimal()
dev.off()

pdf("coordinate_descent/results/mixed_deps_barplots.pdf", width = 10, height = 10)
data_3 = data_2[, .(mean_meta_score = max(mean_meta_score), parameter = parameter[1]), by = value]
ggplot(data_3, aes(x = value, y = mean_meta_score)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ parameter, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
dev.off()
