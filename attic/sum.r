archive[, config_hash := pmap_chr(list(input_trafo, output_trafo, init, init_size_fraction, random_interleave_iter, surrogate, acqf, lambda, acqopt, epsilon_decay, lambda_decay), mlr3misc::calculate_hash)]

archive[, sd_meta_score := sd(mean_meta_score), by = config_hash]
archive[, n := .N, by = config_hash]

x = archive[order(batch_nr, mean_meta_score, decreasing = TRUE)][, head(.SD, 1), by = batch_nr][order(batch_nr)][,2:14]
setcolorder(x, c("parameter"))

x = archive[, 1:14]
setnames(x, "batch_nr", "iteration")
x[, iteration := iteration - 1]

write.xlsx(x, file = "supplement_1_coordinate_descent_numeric.xlsx", sheetName = "coordinate_descent_numeric",
  col.names = TRUE, row.names = TRUE, append = FALSE)
