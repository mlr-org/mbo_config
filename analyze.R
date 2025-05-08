library(data.table)
library(ggplot2)
library(pammtools)
library(mlr3misc)
library(scmamp)

dat = rbind(readRDS("yahpo_pure_numeric_competitors_raw.rds"), readRDS("yahpo_pure_numeric_mlr3mbo_raw.rds"), readRDS("yahpo_pure_numeric_rs_simulated.rds"), fill=TRUE)
dat[, cumbudget := iter, by = .(method, scenario, instance, target_variable, repl)]
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

g = ggplot(aes(x = cumbudget_scaled, y = mean, colour = method, fill = method), data = agg_budget) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(min = mean - se, max = mean + se), colour = NA, alpha = 0.3) +
  labs(x = "Fraction of Budget Used", y = "Mean Normalized Regret", colour = "Optimizer", fill = "Optimizer") +
  facet_wrap(~ scenario + instance, scales = "free", ncol = 5) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(size = rel(0.75)), legend.text = element_text(size = rel(0.5)))
ggsave("/tmp/anytime.png", plot = g, device = "png", width = 15, height = 10)

overall_budget = agg_budget[, .(mean = mean(mean), se = sd(mean) / sqrt(.N)), by = .(method, cumbudget_scaled)]

g = ggplot(aes(x = cumbudget_scaled, y = mean, colour = method, fill = method), data = overall_budget) +
  scale_y_log10() +
  geom_step() +
  geom_stepribbon(aes(min = mean - se, max = mean + se), colour = NA, alpha = 0.1) +
  labs(x = "Fraction of Budget Used", y = "Mean Normalized Regret", colour = "Optimizer", fill = "Optimizer") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(size = rel(0.75)), legend.text = element_text(size = rel(0.75)))
ggsave("/tmp/anytime_average.png", plot = g, device = "png", width = 6, height = 4)


methods = unique(agg_budget$method)
ranks = map_dtr(unique(agg_budget$scenario), function(scenario_) {
  map_dtr(unique(agg_budget$instance), function(instance_) {
    map_dtr(unique(agg_budget$target_variable), function(target_variable_) {
      map_dtr(unique(agg_budget$cumbudget_scaled), function(cumbudget_scaled_) {
        res = agg_budget[scenario == scenario_ & instance == instance_ & target_variable == target_variable_ & cumbudget_scaled == cumbudget_scaled_]
        if (nrow(res) == 0L) {
          return(data.table())
        }
        setorderv(res, "mean")
        data.table(rank = match(methods, res$method), method = methods, scenario = scenario_, instance = instance_, cumbudget_scaled = cumbudget_scaled_)
      })
    })
  })
})

ranks_overall = ranks[, .(mean = mean(rank), se = sd(rank) / sqrt(.N)), by = .(method, cumbudget_scaled)]

g = ggplot(aes(x = cumbudget_scaled, y = mean, colour = method, fill = method), data = ranks_overall) +
  geom_line() +
  geom_ribbon(aes(min = mean - se, max = mean + se), colour = NA, alpha = 0.3) +
  labs(x = "Fraction of Budget Used", y = "Mean Rank", colour = "Optimizer", fill = "Optimizer") +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_text(size = rel(0.75)), legend.text = element_text(size = rel(0.75)))
ggsave("/tmp/anytime_average_rank.png", plot = g, device = "png", width = 6, height = 4)

best_agg = agg_budget[cumbudget_scaled == 1]
best_agg[, problem := paste0(scenario, "_", instance, "_", target_variable)]
tmp = - as.matrix(dcast(best_agg, problem ~ method, value.var = "mean")[, -1L])
friedmanTest(tmp)
png("/tmp/cd.png", width = 600, height = 400, pointsize = 10)
plotCD(tmp, cex = 1)
dev.off()
