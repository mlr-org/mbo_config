library(paradox)

search_space = ps(
  input_trafo            = p_fct(c("none", "unitcube")),
  output_trafo           = p_fct(c("none", "standardize", "log")),
  init                   = p_fct(c("random", "lhs", "sobol")),
  init_size_fraction     = p_fct(c("0.05", "0.10", "0.25"), trafo = as.numeric),
  random_interleave_iter = p_fct(c("0", "2", "4"), trafo = as.integer),
  # surrogate
  trees                  = p_fct(c("10", "500"), trafo = as.integer),
  variance_estimator     = p_fct(c("jack", "ensemble_standard_deviation", "law_of_total_variance")),
  # acqf
  acqf                   = p_fct(c("EI", "CB", "PI", "Mean")),
  lambda                 = p_fct(c("1", "3", "10"), depends = acqf == "CB", trafo = as.integer),
  epsilon_decay          = p_lgl(depends = acqf == "EI"),
  lambda_decay           = p_lgl(depends = acqf == "CB"),
  # acqopt
  acqopt                 = p_fct(c("RS_1000", "RS", "LS"))
)

saveRDS(search_space, "common/mixed_deps_search_space.rds")