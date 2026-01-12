library(data.table)
library(mlr3misc)

# numeric optimization path
data = fread("coordinate_descent/results/numeric_optimization_path.csv")

data[, c("iteration", "parameter", "mean_meta_score")]

data[2:nrow(data), value := map_chr(parameter, function(x) as.character(.SD[[x]])), by = iteration]

data[, parameter := map_chr(parameter, fix_parameters)]
data[, value := map_chr(value, fix_values)]
data = data[, list(iteration, parameter, value, mean_meta_score)]

knitr::kable(data, format = "latex", booktabs = TRUE, linesep = "", digits = 2, col.names = c("Iteration", "Parameter", "Value", "Meta Score"))

# mixed optimization path
data = fread("coordinate_descent/results/mixed_optimization_path.csv")

data[, c("iteration", "parameter", "mean_meta_score")]

data[2:nrow(data), value := map_chr(parameter, function(x) as.character(.SD[[x]])), by = iteration]

data[, parameter := map_chr(parameter, fix_parameters)]
data[, value := map_chr(value, fix_values)]
data = data[, list(iteration, parameter, value, mean_meta_score)]

knitr::kable(data, format = "latex", booktabs = TRUE, linesep = "", digits = 2, col.names = c("Iteration", "Parameter", "Value", "Meta Score"))

# numeric ablation
data = readRDS("coordinate_descent/results/numeric_archive.rds")
search_space = readRDS("common/numeric_search_space.rds")

data[1, parameter := "start_config"]
start_config = data[1, c("mean_meta_score", search_space$ids()), with = FALSE]
data_1 = data[iteration == 1, c("mean_meta_score", search_space$ids()), with = FALSE]

# default configs for levels of parent parameters
default_configs = list(
  "rf" = list(extratrees = FALSE, trees = "500", variance_estimator = "jack"),
  "gp" = list(kernel = "gauss", nugget = "0", scaling = FALSE),
  "EI" = list(epsilon_decay = FALSE),
  "CB" = list(lambda = "1", lambda_decay = FALSE)
)

ablation_start = map_dtr(search_space$subspaces(), function(subspace) {
  param_id = subspace$ids()[1]
  default_level = start_config[[param_id]]

  if (is.na(default_level)) return(NULL)

  levels = subspace$levels[[1]]
  levels = setdiff(levels, default_level)

  map_dtr(levels, function(level) {
    data_i = data_1[list(level), , on = param_id]

    # match default config
    if (nrow(data_i) > 1) {

      if (level %in% names(default_configs)) {
        # parent parameter
        default = default_configs[[level]]
        args = names(default)
      } else {
        # dependent parameter
        # find parent parameter
        parent = deps[list(param_id), cond, on = "id"][[1]]$rhs
        default = default_configs[[parent]]
        default[param_id] = level
        args = names(default)
      }
      data_i = data_i[default, on = args]
    }

    data.table(parameter = param_id, level = level, meta_score = data_i$mean_meta_score, delta = data_i$mean_meta_score - start_config$mean_meta_score)
  })
})

final_config = data[iteration == 6][order(mean_meta_score, decreasing = TRUE)][1, c("mean_meta_score", search_space$ids()), with = FALSE]
data_7 = data[iteration == 7, c("mean_meta_score", search_space$ids()), with = FALSE]

ablation_final = map_dtr(search_space$subspaces(), function(subspace) {
  param_id = subspace$ids()[1]
  final_level = final_config[[param_id]]

  if (is.na(final_level)) return(NULL)

  default_level = start_config[[param_id]]
  levels = subspace$levels[[1]]
  levels = setdiff(levels, final_level)

  print(param_id)

  map_dtr(levels, function(level) {
    data_i = data_7[list(level), , on = param_id]

    if (nrow(data_i) > 1) {
      if (level %in% names(default_configs)) {
        default = default_configs[[level]]
        args = names(default)
      } else {
        parent_id = deps[list(param_id), on = "id"]$on
        parent_level = deps[list(param_id), cond, on = "id"][[1]]$rhs
        default = default_configs[[parent_level]]
        default[param_id] = level
        args = names(default)
      }
      data_i = data_i[default, on = args]
    }

    data.table(parameter = param_id, level = level, meta_score = data_i$mean_meta_score, delta = data_i$mean_meta_score - final_config$mean_meta_score)
  })
})

ablation = merge(ablation_start[, list(parameter, level, start_delta = delta)], ablation_final[, list(parameter, level, final_delta = delta)], by = c("parameter", "level"), all = TRUE)
param_ids = search_space$ids()
ablation = ablation[param_ids, on = "parameter", nomatch = NULL][, list(parameter, level, start_delta, final_delta)]

ablation[, parameter := map_chr(parameter, function(x) {
  switch(x,
    "input_trafo" = "Input Trafo",
    "output_trafo" = "Output Trafo",
    "init" = "Init",
    "init_size_fraction" = "Init Size",
    "random_interleave_iter" = "Random Interleave",
    "trees" = "Trees",
    "variance_estimator" = "Variance Estimator",
    "acqf" = "Acquisition Function",
    "lambda" = "Lambda",
    "acqopt" = "Optimizer",
    "epsilon_decay" = "Epsilon Decay",
    "lambda_decay" = "Lambda Decay",
    "nugget" = "Nugget",
    "scaling" = "Scaling",
    "extratrees" = "Extra Trees",
    "kernel" = "Kernel",
    "surrogate" = "Surrogate",
    x
  )
})]

ablation[, level := map_chr(level, function(x) {
  switch(x,
    "none" = "None",
    "unitcube" = "Unitcube",
    "standardize" = "Standardize",
    "log" = "Log",
    "random" = "Random",
    "lhs" = "LHS",
    "sobol" = "Sobol",
    "jack" = "Jack",
    "ensemble_standard_deviation" = "ESD",
    "law_of_total_variance" = "LTV",
    "EI" = "EI",
    "CB" = "CB",
    "PI" = "PI",
    "Mean" = "Mean",
    "RS_1000" = "RS 1000",
    "RS" = "RS",
    "LS" = "LS",
    "DIRECT" = "DIRECT",
    "CMAES" = "CMAES",
    "LBFGSB" = "LBFGSB",
    "exp" = "Exp",
    "matern3_2" = "Matern 3/2",
    "matern5_2" = "Matern 5/2",
    "powexp" = "Powexp",
    "gauss" = "Gauss",
    "rbf" = "RBF",
    "true" = "True",
    "false" = "False",
    "rf" = "RF",
    "gp" = "GP",
    x
  )
})]

options(knitr.kable.NA = "--")
knitr::kable(ablation, format = "latex", booktabs = TRUE, linesep = "", digits = 2, col.names = c("Parameter", "Value", "Start Delta", "Final Delta"))

# mixed
data = readRDS("coordinate_descent/results/mixed_archive.rds")
search_space = readRDS("common/mixed_search_space.rds")

data[1, parameter := "start_config"]
start_config = data[1, c("mean_meta_score", search_space$ids()), with = FALSE]
data_1 = data[iteration == 1, c("mean_meta_score", search_space$ids()), with = FALSE]

default_configs = list(
  "EI" = list(epsilon_decay = FALSE),
  "CB" = list(lambda = "1", lambda_decay = FALSE)
)

ablation_start = map_dtr(search_space$subspaces(), function(subspace) {
  param_id = subspace$ids()[1]
  default_level = start_config[[param_id]]

  if (is.na(default_level)) return(NULL)

  levels = subspace$levels[[1]]
  levels = setdiff(levels, default_level)

  map_dtr(levels, function(level) {
    data_i = data_1[list(level), , on = param_id]

    # match default config
    if (nrow(data_i) > 1) {

      if (level %in% names(default_configs)) {
        # parent parameter
        default = default_configs[[level]]
        args = names(default)
      } else {
        # dependent parameter
        # find parent parameter
        parent = deps[list(param_id), cond, on = "id"][[1]]$rhs
        default = default_configs[[parent]]
        default[param_id] = level
        args = names(default)
      }
      data_i = data_i[default, on = args]
    }

    data.table(parameter = param_id, level = level, meta_score = data_i$mean_meta_score, delta = data_i$mean_meta_score - start_config$mean_meta_score)
  })
})

final_config = data[iteration == 7][order(mean_meta_score, decreasing = TRUE)][1, c("mean_meta_score", search_space$ids()), with = FALSE]
data_8 = data[iteration == 8, c("mean_meta_score", search_space$ids()), with = FALSE]

ablation_final = map_dtr(search_space$subspaces(), function(subspace) {
  param_id = subspace$ids()[1]
  final_level = final_config[[param_id]]

  if (is.na(final_level)) return(NULL)

  default_level = start_config[[param_id]]
  levels = subspace$levels[[1]]
  levels = setdiff(levels, final_level)

  print(param_id)

  map_dtr(levels, function(level) {
    data_i = data_8[list(level), , on = param_id]

    if (nrow(data_i) > 1) {
      if (level %in% names(default_configs)) {
        default = default_configs[[level]]
        args = names(default)
      } else {
        parent_id = deps[list(param_id), on = "id"]$on
        parent_level = deps[list(param_id), cond, on = "id"][[1]]$rhs
        default = default_configs[[parent_level]]
        default[param_id] = level
        args = names(default)
      }
      data_i = data_i[default, on = args]
    }

    data.table(parameter = param_id, level = level, meta_score = data_i$mean_meta_score, delta = data_i$mean_meta_score - final_config$mean_meta_score)
  })
})


ablation = merge(ablation_start[, list(parameter, level, start_delta = delta)], ablation_final[, list(parameter, level, final_delta = delta)], by = c("parameter", "level"), all = TRUE)
param_ids = search_space$ids()
ablation = ablation[param_ids, on = "parameter", nomatch = NULL][, list(parameter, level, start_delta, final_delta)]

ablation[, parameter := map_chr(parameter, function(x) {
  switch(x,
    "input_trafo" = "Input Trafo",
    "output_trafo" = "Output Trafo",
    "init" = "Init",
    "init_size_fraction" = "Init Size",
    "random_interleave_iter" = "Random Interleave",
    "trees" = "Trees",
    "variance_estimator" = "Variance Estimator",
    "acqf" = "Acquisition Function",
    "lambda" = "Lambda",
    "acqopt" = "Optimizer",
    "epsilon_decay" = "Epsilon Decay",
    "lambda_decay" = "Lambda Decay",
    "nugget" = "Nugget",
    "scaling" = "Scaling",
    "extratrees" = "Extra Trees",
    "kernel" = "Kernel",
    "surrogate" = "Surrogate",
    x
  )
})]

ablation[, level := map_chr(level, function(x) {
  switch(x,
    "none" = "None",
    "unitcube" = "Unitcube",
    "standardize" = "Standardize",
    "log" = "Log",
    "random" = "Random",
    "lhs" = "LHS",
    "sobol" = "Sobol",
    "jack" = "Jack",
    "ensemble_standard_deviation" = "ESD",
    "law_of_total_variance" = "LTV",
    "EI" = "EI",
    "CB" = "CB",
    "PI" = "PI",
    "Mean" = "Mean",
    "RS_1000" = "RS 1000",
    "RS" = "RS",
    "LS" = "LS",
    "DIRECT" = "DIRECT",
    "CMAES" = "CMAES",
    "LBFGSB" = "LBFGSB",
    "exp" = "Exp",
    "matern3_2" = "Matern 3/2",
    "matern5_2" = "Matern 5/2",
    "powexp" = "Powexp",
    "gauss" = "Gauss",
    "rbf" = "RBF",
    "true" = "True",
    "false" = "False",
    "rf" = "RF",
    "gp" = "GP",
    x
  )
})]

options(knitr.kable.NA = "--")
knitr::kable(ablation, format = "latex", booktabs = TRUE, linesep = "", digits = 2, col.names = c("Parameter", "Value", "Start Delta", "Final Delta"))


# final configuration
best = data[iteration == 6][order(mean_meta_score, decreasing = TRUE)][1, c("raw_meta_score", search_space$ids()), with = FALSE]
best = cbind(best[rep(1, nrow(best)), -c("raw_meta_score")], best[, list(meta_score = unlist(raw_meta_score))])

# input trafo
data_input_trafo = data[iteration == 7 & parameter == "input_trafo" & input_trafo != "none", .(mean_meta_score, input_trafo, raw_meta_score)]
data_input_trafo = data_input_trafo[, list(meta_score = unlist(raw_meta_score), parameter = "input_trafo"), by = input_trafo]
setnames(data_input_trafo, "input_trafo", "value")
data_input_trafo = rbind(data_input_trafo, best[, list(meta_score, value = input_trafo, parameter = "input_trafo")])

# output trafo
data_output_trafo = data[iteration == 7 & parameter == "output_trafo" & output_trafo != "log", .(mean_meta_score, output_trafo, raw_meta_score)]
data_output_trafo = data_output_trafo[, list(meta_score = unlist(raw_meta_score), parameter = "output_trafo"), by = output_trafo]
setnames(data_output_trafo, "output_trafo", "value")
data_output_trafo = rbind(data_output_trafo, best[, list(meta_score, value = output_trafo, parameter = "output_trafo")])

# init
data_init = data[iteration == 7 & parameter == "init" & init != "random", .(mean_meta_score, init, raw_meta_score)]
setorder(data_init, -mean_meta_score)
data_init = data_init[, head(.SD, 1), by = "init"][, list(value = init, meta_score = unlist(raw_meta_score), parameter = "init")]
data_init = rbind(data_init, best[, list(meta_score, value = init, parameter = "init")])

# init size fraction
data_init_size_fraction = data[iteration == 7 & parameter == "init_size_fraction" & init_size_fraction != "0.05", .(mean_meta_score, init_size_fraction, raw_meta_score)]
data_init_size_fraction = data_init_size_fraction[, list(meta_score = unlist(raw_meta_score), parameter = "init_size_fraction"), by = init_size_fraction]
setnames(data_init_size_fraction, "init_size_fraction", "value")
data_init_size_fraction = rbind(data_init_size_fraction, best[, list(meta_score, value = init_size_fraction, parameter = "init_size_fraction")])

# random interleave iter
data_random_interleave_iter = data[iteration == 7 & parameter == "random_interleave_iter" & random_interleave_iter != "0", .(mean_meta_score, random_interleave_iter, raw_meta_score)]
data_random_interleave_iter = data_random_interleave_iter[, list(meta_score = unlist(raw_meta_score), parameter = "random_interleave_iter"), by = random_interleave_iter]
setnames(data_random_interleave_iter, "random_interleave_iter", "value")
data_random_interleave_iter = rbind(data_random_interleave_iter, best[, list(meta_score, value = random_interleave_iter, parameter = "random_interleave_iter")])

# surrogate
data_surrogate = data[iteration == 7 & surrogate == "rf", .(mean_meta_score, surrogate, extratrees, trees, variance_estimator, raw_meta_score)]
setorder(data_surrogate, -mean_meta_score)
data_surrogate = data_surrogate[, head(.SD, 1)][, list(meta_score = unlist(raw_meta_score), value = "RF", parameter = "surrogate")]
data_surrogate = rbind(data_surrogate, best[, list(meta_score, value = surrogate, parameter = "surrogate")])

# acqf
data_acqf = data[iteration == 7 & parameter == "acqf" & acqf != "CB", .(mean_meta_score, acqf, raw_meta_score)]
setorder(data_acqf, -mean_meta_score)
data_acqf = data_acqf[, head(.SD, 1), by = "acqf"][, list(value = acqf, meta_score = unlist(raw_meta_score), parameter = "acqf")]
data_acqf = rbind(data_acqf, best[, list(meta_score, value = acqf, parameter = "acqf")])

# acqopt
data_acqopt = data[iteration == 7 & parameter == "acqopt", .(mean_meta_score, acqopt, raw_meta_score)]
data_acqopt = data_acqopt[, list(meta_score = unlist(raw_meta_score), parameter = "acqopt"), by = acqopt]
setnames(data_acqopt, "acqopt", "value")
data_acqopt = rbind(data_acqopt, best[, list(meta_score, value = acqopt, parameter = "acqopt")])

x = rbindlist(list(data_acqopt, data_acqf, data_surrogate, data_init, data_init_size_fraction, data_random_interleave_iter, data_output_trafo, data_input_trafo), use.names = TRUE)

pdf("coordinate_descent/results/numeric_ablation.pdf", width = 10, height = 10)
ggplot(x, aes(x = value, y = meta_score)) +
  geom_boxplot() +
  facet_wrap(~ parameter, scales = "free_x") +
  theme_minimal()
dev.off()

# model

parent_parameters = setdiff(search_space$ids(), search_space$deps$id)

best = data[iteration == 6][order(mean_meta_score, decreasing = TRUE)][1, ]

data_other = data[iteration == 7 & parameter %nin% c("surrogate", "acqf"), ]

data_surrogate = data[iteration == 7 & surrogate == "rf"]
setorder(data_surrogate, -mean_meta_score)
data_surrogate = data_surrogate[, head(.SD, 1)]

data_acqf = data[iteration == 7 & acqf != "CB"]
setorder(data_acqf, -mean_meta_score)
data_acqf = data_acqf[, head(.SD, 1), by = "acqf"]

data_model = rbindlist(list(data_other, data_surrogate, data_acqf, best), use.names = TRUE, fill = TRUE)
data_model = data_model[, list(meta_score = unlist(raw_meta_score)), by = c(search_space$ids())][, instance := rep(seq(8), nrow(data_model))]
data_model = data_model[, c("meta_score", "instance", parent_parameters), with = FALSE]

data_model[, input_trafo := factor(input_trafo, levels = c("none", "unitcube"))]
data_model[, output_trafo := factor(output_trafo, levels = c("log", "none", "standardize"))]
data_model[, init := factor(init, levels = c("random", "lhs", "sobol"))]
data_model[, init_size_fraction := factor(init_size_fraction, levels = c("0.05", "0.10", "0.25"))]
data_model[, random_interleave_iter := factor(random_interleave_iter, levels = c("0", "2", "4"))]
data_model[, surrogate := factor(surrogate, levels = c("gp", "rf"))]
data_model[, acqf := factor(acqf, levels = c("CB", "EI", "PI", "Mean"))]
data_model[, acqopt := factor(acqopt, levels = c("CMAES", "RS_1000", "RS", "LS", "DIRECT", "LBFGSB"))]

fit = lmer(meta_score ~ input_trafo + output_trafo + init + init_size_fraction + random_interleave_iter + surrogate + acqf + acqopt + (1 | instance), data = data_model)


emm_input_trafo = emmeans(fit, ~ input_trafo)
emm_output_trafo = emmeans(fit, ~ output_trafo)
emm_init = emmeans(fit, ~ init)
emm_init_size_fraction = emmeans(fit, ~ init_size_fraction)
emm_random_interleave_iter = emmeans(fit, ~ random_interleave_iter)
emm_surrogate = emmeans(fit, ~ surrogate)
emm_acqf = emmeans(fit, ~ acqf)
emm_acqopt = emmeans(fit, ~ acqopt)

contrast(emm_input_trafo, "pairwise")
contrast(emm_output_trafo, "pairwise")
contrast(emm_init, "pairwise")
contrast(emm_init_size_fraction, "pairwise")
contrast(emm_random_interleave_iter, "pairwise")
contrast(emm_surrogate, "pairwise")
contrast(emm_acqf, "pairwise")
contrast(emm_acqopt, "pairwise")


fix_parameters = function(x) {
  switch(x,
    "input_trafo" = "Input Trafo",
    "output_trafo" = "Output Trafo",
    "init" = "Init",
    "init_size_fraction" = "Init Size",
    "random_interleave_iter" = "Random Interleave",
    "trees" = "Trees",
    "variance_estimator" = "Variance Estimator",
    "acqf" = "Acquisition Function",
    "lambda" = "Lambda",
    "acqopt" = "Optimizer",
    "epsilon_decay" = "Epsilon Decay",
    "lambda_decay" = "Lambda Decay",
    "nugget" = "Nugget",
    "scaling" = "Scaling",
    "extratrees" = "Extra Trees",
    "kernel" = "Kernel",
    "surrogate" = "Surrogate",
    x
  )
}

fix_values = function(x) {
  switch(x,
    "none" = "None",
    "unitcube" = "Unitcube",
    "standardize" = "Standardize",
    "log" = "Log",
    "random" = "Random",
    "lhs" = "LHS",
    "sobol" = "Sobol",
    "jack" = "Jack",
    "ensemble_standard_deviation" = "ESD",
    "law_of_total_variance" = "LTV",
    "EI" = "EI",
    "CB" = "CB",
    "PI" = "PI",
    "Mean" = "Mean",
    "RS_1000" = "RS 1000",
    "RS" = "RS",
    "LS" = "LS",
    "DIRECT" = "DIRECT",
    "CMAES" = "CMAES",
    "LBFGSB" = "LBFGSB",
    "exp" = "Exp",
    "matern3_2" = "Matern 3/2",
    "matern5_2" = "Matern 5/2",
    "powexp" = "Powexp",
    "gauss" = "Gauss",
    "rbf" = "RBF",
    "true" = "True",
    "false" = "False",
    "rf" = "RF",
    "gp" = "GP",
    x
  )
}
