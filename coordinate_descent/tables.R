search_space = readRDS("common/mixed_search_space.rds")
best_scores = fread("coordinate_descent/results/mixed_best_scores.csv")

best_scores[, parameter := map_chr(parameter, function(x) {
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
    "lambda_decay" = "Lambda Decay"
  )
})]

best_scores[, value := map_chr(value, function(x) {
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
    x
  )
})]

best_scores[, parameter := fifelse(parameter == shift(parameter), "", parameter)]

knitr::kable(best_scores, format = "latex", booktabs = TRUE, digits = 2, linesep = "", col.names = c(
  "Parameter", "Value", "Best Mean RSNS"
))
