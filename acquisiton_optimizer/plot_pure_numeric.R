library(ggplot2)
library(data.table)

options(width = 200)

results = fread("acquisiton_optimizer/results_pure_numeric.csv")

aggr = results[, 
 list(mean_y = mean(y), 
      n = .N), by = .(problem, id, d, budget)][order(problem, id, d, budget)]

results[, 
 list(mean_runtime = mean(runtime), 
      sd_runtime = sd(runtime)), by = .(problem, id, budget)][order(problem, id, budget)]

pdf("test.pdf", width = 20, height = 10)
ggplot(aggr, aes(x = budget, y = mean_y, color = id)) +
  geom_line() +
  facet_wrap(~problem, scales = "free")
dev.off()