library(batchtools)
library(paradox)
library(mlr3misc)
library(data.table)
conda_dir = "/glade/work/marcbecker/conda-envs"

YAHPO_BENCHMARK = "pure_numeric"  # "pure_numeric", "mixed_deps"

packages = c("data.table", "paradox")

if (YAHPO_BENCHMARK == "pure_numeric") {
  setup = readRDS("common/pure_numeric_instances.rds")
} else if (YAHPO_BENCHMARK == "mixed_deps") {
  setup = readRDS("common/mixed_deps_instances.rds")
}

set(setup, j = "budget", value = 20L)
setup = setup[rep(seq_row(setup), each = 30L), ]
setup[, repl := seq_len(30L), by = id]
setup[, seed := sample(seq_len(1e6), .N)]

set(setup, j = "budget", value = 20L)
setup = setup[rep(seq_row(setup), each = 30L), ]
setup[, repl := seq_len(30L), by = id]
setup[, seed := sample(seq_len(1e6), .N)]

hq = "/home/marc/repositories/hyperqueue/target/release/hq"
conda_envs = "/home/marc/miniconda/envs"
results_dir = "/home/marc/repositories/mbo_config/competitors/results"

pmap(setup[1], function(benchmark, scenario, instance, target_variable, direction, budget, id, seed, repl) {
  id = paste0(benchmark, "_", scenario, "_", instance)
  system(sprintf("%s submit --name ax_%s --stdout logs/ax_%s.stdout --stderr logs/ax_%s.stderr  -- %s/ax/bin/python competitors/ax_wrapper.py --benchmark %s --scenario %s --instance %s --target_variable %s --direction %s --budget %s --seed %s --output_path %s/%s_ax_result_%s.csv",
    hq,
    id,
    id,
    id,
    conda_envs,
    benchmark,
    scenario,
    instance,
    target_variable,
    direction,
    budget,
    seed,
    results_dir,
    id,
    repl))
})

pmap(setup[1], function(benchmark, scenario, instance, target_variable, direction, budget, id, seed, repl) {
  id = paste0(benchmark, "_", scenario, "_", instance)
  system(sprintf("%s submit --name hebo_%s --stdout logs/hebo_%s.stdout --stderr logs/hebo_%s.stderr  -- %s/hebo/bin/python competitors/hebo_wrapper.py --benchmark %s --scenario %s --instance %s --target_variable %s --direction %s --budget %s --seed %s --output_path %s/%s_hebo_result_%s.csv",
    hq,
    id,
    id,
    id,
    conda_envs,
    benchmark,
    scenario,
    instance,
    target_variable,
    direction,
    budget,
    seed,
    results_dir,
    id,
    repl))
})

pmap(setup[1], function(benchmark, scenario, instance, target_variable, direction, budget, id, seed, repl) {
  id = paste0(benchmark, "_", scenario, "_", instance)
  system(sprintf("%s submit --name smac_bb_%s --stdout logs/smac_bb_%s.stdout --stderr logs/smac_bb_%s.stderr  -- %s/smac/bin/python competitors/smac_wrapper.py --benchmark %s --scenario %s --instance %s --target_variable %s --direction %s --budget %s --seed %s --output_path %s/%s_smac_bb_result_%s.csv --facade bb",
    hq,
    id,
    id,
    id,
    conda_envs,
    benchmark,
    scenario,
    instance,
    target_variable,
    direction,
    budget,
    seed,
    results_dir,
    id,
    repl))
})

pmap(setup[1], function(benchmark, scenario, instance, target_variable, direction, budget, id, seed, repl) {
  id = paste0(benchmark, "_", scenario, "_", instance)
  system(sprintf("%s submit --name smac_hpo_%s --stdout logs/smac_hpo_%s.stdout --stderr logs/smac_hpo_%s.stderr  -- %s/smac/bin/python competitors/smac_wrapper.py --benchmark %s --scenario %s --instance %s --target_variable %s --direction %s --budget %s --seed %s --output_path %s/%s_smac_hpo_result_%s.csv --facade hpo",
    hq,
    id,
    id,
    id,
    conda_envs,
    benchmark,
    scenario,
    instance,
    target_variable,
    direction,
    budget,
    seed,
    results_dir,
    id,
    repl))
})
