library(data.table)
library(ggplot2)
library(pammtools)
library(mlr3misc)

dat = rbind(readRDS("yahpo_competitors_raw.rds"), readRDS("yahpo_mlr3mbo_raw.rds"), readRDS("yahpo_rs_simulated.rds"), fill=TRUE)
dat[, cumbudget := cumsum(iter), by = .(method, scenario, instance, target_variable, repl)]
dat[, cumbudget_scaled := cumbudget / max(cumbudget), by = .(method, scenario, instance, target_variable, repl)]
dat[, normalized_regret := (target - min(target)) / (max(target) - min(target)), by = .(scenario, instance, target_variable)]
dat[, best_normalized := cummin(normalized_regret), by = .(method, scenario, instance, target_variable, repl)]

get_best_normalized_cumbudget = function(best_normalized, cumbudget_scaled) {
  budgets = seq(0, 1, length.out = 101L)
  map_dbl(budgets, function(budget) {
    indices = which(cumbudget_scaled <= budget)
    if (length(indices) == 0L) {
      max(best_normalized)
    } else {
      min(best_normalized[indices])
    }
  })
}

dat_budget = dat[, .(best_normalized_budget = get_best_normalized_cumbudget(best_normalized, cumbudget_scaled), cumbudget_scaled = seq(0, 1, length.out = 101L)), by = .(method, scenario, instance, target_variable, repl)]
agg_budget = dat_budget[, .(mean = mean(best_normalized_budget), se = sd(best_normalized_budget) / sqrt(.N)), by = .(cumbudget_scaled, method, scenario, instance, target_variable)]

g = ggplot(aes(x = cumbudget_scaled, y = mean, colour = method, fill = method), data = agg_budget[cumbudget_scaled > 0.10]) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(min = mean - se, max = mean + se), colour = NA, alpha = 0.3) +
  labs(x = "Fraction of Budget Used", y = "Mean Normalized Regret", colour = "Optimizer", fill = "Optimizer") +
  facet_wrap(~ scenario + instance, scales = "free", ncol = 5) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(size = rel(0.75)), legend.text = element_text(size = rel(0.5)))
ggsave("anytime.png", plot = g, device = "png", width = 15, height = 10)

overall_budget = agg_budget[, .(mean = mean(mean), se = sd(mean) / sqrt(.N)), by = .(method, cumbudget_scaled)]

g = ggplot(aes(x = cumbudget_scaled, y = mean, colour = method, fill = method), data = overall_budget[cumbudget_scaled > 0.10]) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(min = mean - se, max = mean + se), colour = NA, alpha = 0.1) +
  labs(x = "Fraction of Budget Used", y = "Mean Normalized Regret", colour = "Optimizer", fill = "Optimizer") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(size = rel(0.75)), legend.text = element_text(size = rel(0.75)))
ggsave("anytime_average.png", plot = g, device = "png", width = 6, height = 4)
