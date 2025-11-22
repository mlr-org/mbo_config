from yahpo_gym import benchmark_set
from ConfigSpace import ConfigurationSpace, Constant

cs = ConfigurationSpace(seed=0)


cs.add_hyperparameters([
    Constant("booster", "dart"),
    Constant("nrounds", 58),
    Constant("lambda", 3.390559),
    Constant("alpha", 3.715534),
    Constant("subsample", 0.4960434),
    Constant("rate_drop", 0.2035218),
    Constant("skip_drop", 0.001236176),
    Constant("num.impute.selected.cpo", "impute.mean"),
])


cs_2 = bench.get_opt_space().sample_configuration(1).get_dictionary()


bench = benchmark_set.BenchmarkSet("rbv2_xgboost", instance="12", multithread=False)

bench.objective_function(cs_2)


bench.objective_function(cs)

cfg = bench.config_space.get_default_configuration().get_dictionary()


# Overwrite with your values
cfg.update({
    "booster": "dart",
    "nrounds": 58,
    "lambda": 3.390559,
    "alpha": 3.715534,
    "subsample": 0.4960434,
    "rate_drop": 0.2035218,
    "skip_drop": 0.001236176,
    "num.impute.selected.cpo": "impute.mean",
    # required extras for rbv2_*:
    "trainsize": 1.0,
    "repl": 10,
    bench.config.instance_names: "12",  # usually "task_id" for rbv2_*
})

bench.objective_function(cfg)




X = {
    "booster": "dart",
    "nrounds": 58,
    "lambda": 3.390559,
    "alpha": 3.715534,
    "subsample": 0.4960434,
    "rate_drop": 0.2035218,
    "skip_drop": 0.001236176,
    "num.impute.selected.cpo": "impute.mean",
    # required extras (mirror what your runner does)
    "trainsize": 1.0,          # fidelity
    "repl": 10,                # rbv2_* quirk
    bench.config.instance_names: "12",  # usually "task_id" for rbv2_*
}

y = bench.objective_function(X, seed=1234, logging=False, multithread=False)[0]



