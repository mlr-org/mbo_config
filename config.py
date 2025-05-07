SCENARIO_META_DATA = {
    "lcbench": {
        "fidelity_param_id": "epoch",
        "on_integer_scale": True,
        "max_fidelity": 52,
        "params_to_remove": ["epoch", "OpenML_task_id"],
        "pure_numeric": {
            "must_be_rounded_to_integer": ["num_layers"],
            "params_to_remove": [],
            "params_to_constant": {},
        },
    },
    "nb301": {
        "fidelity_param_id": "epoch",
        "on_integer_scale": True,
        "max_fidelity": 98,
        "params_to_remove": ["epoch"],
        "pure_numeric": {},
    },
    "rbv2_glmnet": {
        "fidelity_param_id": "trainsize",
        "on_integer_scale": False,
        "max_fidelity": 1.0,
        "params_to_remove": ["trainsize", "repl", "task_id"],
        "pure_numeric": {
            "must_be_rounded_to_integer": [],
            "params_to_remove": [],
            "params_to_constant": {"num.impute.selected.cpo": "impute.mean"},
        },
    },
    "rbv2_rpart": {
        "fidelity_param_id": "trainsize",
        "on_integer_scale": False,
        "max_fidelity": 1.0,
        "params_to_remove": ["trainsize", "repl", "task_id"],
        "pure_numeric": {
            "must_be_rounded_to_integer": ["maxdepth", "minbucket", "minsplit"],
            "params_to_remove": [],
            "params_to_constant": {"num.impute.selected.cpo": "impute.mean"},
        },
    },
    "rbv2_ranger": {
        "fidelity_param_id": "trainsize",
        "on_integer_scale": False,
        "max_fidelity": 1.0,
        "params_to_remove": ["trainsize", "repl", "task_id"],
        "pure_numeric": {
            "must_be_rounded_to_integer": ["min.node.size", "num.trees"],
            "params_to_remove": ["num.random.splits"],
            "params_to_constant": {
                "num.impute.selected.cpo": "impute.mean",
                "respect.unordered.factors": "ignore",
                "splitrule": "gini",
            },
        },
    },
    "rbv2_xgboost": {
        "fidelity_param_id": "trainsize",
        "on_integer_scale": False,
        "max_fidelity": 1.0,
        "params_to_remove": ["trainsize", "repl", "task_id"],
        "pure_numeric": {
            "must_be_rounded_to_integer": ["max_depth"],
            "params_to_remove": ["rate_drop", "skip_drop"],
            "params_to_constant": {
                "booster": "gbtree",
                "num.impute.selected.cpo": "impute.mean",
            },
        },
    },
    "rbv2_super": {
        "fidelity_param_id": "trainsize",
        "on_integer_scale": False,
        "max_fidelity": 1.0,
        "params_to_remove": ["trainsize", "repl", "task_id"],
        "pure_numeric": {},
    },
}

YAHPO_DATA_PATH = "/glade/derecho/scratch/lschneider/yahpo_data/"
YAHPO_SCRIPT_PATH = "/glade/u/home/lschneider/mbo_config/subprocess_yahpo.py"
YAHPO_VENV_PATH = "/glade/u/home/lschneider/mbo_config/yahpo_venv/bin/python"
