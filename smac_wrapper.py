import json
import os
import random
import re
import shutil
import subprocess
import tempfile
from functools import partial

import numpy as np
import pandas as pd
from ConfigSpace import ConfigurationSpace
from ConfigSpace.conditions import (
    AndConjunction,
    EqualsCondition,
    GreaterThanCondition,
    InCondition,
    LessThanCondition,
    NotEqualsCondition,
    OrConjunction,
)
from smac import BlackBoxFacade, HyperparameterOptimizationFacade, Scenario
from smac.intensifier import Intensifier

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


def get_children(condition):
    if isinstance(condition, (AndConjunction, OrConjunction)):
        children = condition.get_children()
    elif isinstance(
        condition,
        (
            EqualsCondition,
            NotEqualsCondition,
            InCondition,
            GreaterThanCondition,
            LessThanCondition,
        ),
    ):
        children = [condition.child]
    else:
        raise ValueError(f"Condition of class {condition.__class__} not supported.")
    return [child.name for child in children]


def get_parents(condition):
    if isinstance(condition, (AndConjunction, OrConjunction)):
        parents = condition.get_parents()
    elif isinstance(
        condition,
        (
            EqualsCondition,
            NotEqualsCondition,
            InCondition,
            GreaterThanCondition,
            LessThanCondition,
        ),
    ):
        parents = [condition.parent]
    else:
        raise ValueError(f"Condition of class {condition.__class__} not supported.")
    return [parent.name for parent in parents]


def remove_hyperparameters(cs_original, params_to_remove, seed):
    if len(params_to_remove) == 0:
        cs_original.seed(seed)
        return cs_original
    if len(cs_original.forbidden_clauses) > 0:
        raise ValueError("Forbidden clasuses are not supported.")
    cs_new = ConfigurationSpace(seed=seed)

    for hp in cs_original.values():
        if hp.name not in params_to_remove:
            cs_new.add(hp)

    for condition in cs_original.conditions:
        children = get_children(condition)
        parents = get_parents(condition)

        if not set(children).intersection(params_to_remove) and not set(
            parents
        ).intersection(params_to_remove):
            cs_new.add(condition)

    return cs_new


def fix_config(
    config, benchmark, scenario, fidelity_param_id, on_integer_scale, max_fidelity
):
    X = dict(config)
    if "rbv2_" in scenario:
        X.update({"repl": 10})  # manual fix required for rbv2_
    X.update(
        {
            fidelity_param_id: (
                int(round(max_fidelity)) if on_integer_scale else max_fidelity
            )
        }
    )
    if benchmark == "pure_numeric":
        for param_id in SCENARIO_META_DATA[scenario]["pure_numeric"][
            "must_be_rounded_to_integer"
        ]:
            X.update({param_id: int(round(X.get(param_id)))})
        for param_id in SCENARIO_META_DATA[scenario]["pure_numeric"][
            "params_to_constant"
        ].keys():
            X.update(
                {
                    param_id: SCENARIO_META_DATA[scenario]["pure_numeric"][
                        "params_to_constant"
                    ].get(param_id)
                }
            )
    # FIXME: mixed
    return X


def target_function(
    config,
    seed,
    benchmark,
    scenario,
    instance,
    target_variable,
    direction,
    fidelity_param_id,
    on_integer_scale,
    max_fidelity,
):
    X = fix_config(
        config,
        benchmark=benchmark,
        scenario=scenario,
        fidelity_param_id=fidelity_param_id,
        on_integer_scale=on_integer_scale,
        max_fidelity=max_fidelity,
    )
    input_data = {"X": X}
    with tempfile.NamedTemporaryFile(mode="w+", delete=False) as f:
        json.dump(input_data, f)
        input_path = f.name

    script_path = "/glade/u/home/lschneider/mbo_config/subprocess_yahpo.py"
    venv_path = "/glade/u/home/lschneider/mbo_config/yahpo_venv/bin/python"

    cmd = [
        venv_path,
        script_path,
        "--input_path",
        input_path,
        "--scenario",
        scenario,
        "--instance",
        instance,
        "--target_variable",
        target_variable,
        "--seed",
        str(seed),
    ]
    result = subprocess.run(
        cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True
    )
    os.unlink(input_path)

    if result.returncode != 0:
        raise RuntimeError(
            f"Subprocess failed:\nSTDOUT: {result.stdout}\nSTDERR: {result.stderr}"
        )

    match = re.search(r"({.*})\s*$", result.stdout.strip(), re.DOTALL)
    if not match:
        raise RuntimeError(
            f"Could not extract JSON from subprocess output:\n{result.stdout}"
        )

    try:
        output = json.loads(match.group(1))
    except json.JSONDecodeError as e:
        raise RuntimeError(
            f"Failed to parse JSON from subprocess output:\nMatched String: {match.group(1)}\nError: {e}"
        )

    if direction == "maximize":
        factor = -1.0
    elif direction == "minimize":
        factor = 1.0

    return factor * output[target_variable]


def run_smac(
    benchmark,
    scenario,
    instance,
    target_variable,
    direction,
    budget,
    seed,
    facade="hpo",
):
    random.seed(seed)
    np.random.seed(seed)

    if benchmark == "pure_numeric":
        config_space = ConfigurationSpace().from_json(
            os.path.join(YAHPO_DATA_PATH, scenario, "config_space_pure_numeric.json")
        )
    elif benchmark == "mixed":
        raise ValueError("TBD")
    elif benchmark == "":
        config_space = ConfigurationSpace().from_json(
            os.path.join(YAHPO_DATA_PATH, scenario, "config_space.json")
        )
    opt_space = remove_hyperparameters(
        config_space,
        params_to_remove=SCENARIO_META_DATA[scenario]["params_to_remove"],
        seed=seed,
    )
    if benchmark == "pure_numeric":
        opt_space = remove_hyperparameters(
            opt_space,
            params_to_remove=SCENARIO_META_DATA[scenario]["pure_numeric"][
                "params_to_remove"
            ],
            seed=seed,
        )
        opt_space = remove_hyperparameters(
            opt_space,
            params_to_remove=SCENARIO_META_DATA[scenario]["pure_numeric"][
                "params_to_constant"
            ].keys(),
            seed=seed,
        )
    # FIXME: mixed
    opt_space.seed(seed)
    fidelity_param_id = SCENARIO_META_DATA[scenario]["fidelity_param_id"]
    on_integer_scale = SCENARIO_META_DATA[scenario]["on_integer_scale"]
    max_fidelity = SCENARIO_META_DATA[scenario]["max_fidelity"]
    output_directory = (
        "smac4hpo_tmp_" + str(seed) + "_" + str(random.randrange(49152, 65535 + 1))
    )

    smac_scenario = Scenario(
        configspace=opt_space,
        output_directory=output_directory,
        deterministic=True,
        n_trials=budget,
        seed=seed,
    )

    # for unclear reasons, SMAC might still try to evaluate a configuration multiple times with different seeds although the scenario was specified to be deterministic
    # we therefore set max_config_calls explicitly to 1
    # moreover, to make sure that SMAC always evaluates enough configurations we set retries to budget
    smac_intensifier = Intensifier(
        scenario=smac_scenario, max_config_calls=1, retries=budget
    )

    if facade == "hpo":
        smac = HyperparameterOptimizationFacade(
            scenario=smac_scenario,
            target_function=partial(
                target_function,
                benchmark=benchmark,
                scenario=scenario,
                instance=instance,
                target_variable=target_variable,
                direction=direction,
                fidelity_param_id=fidelity_param_id,
                on_integer_scale=on_integer_scale,
                max_fidelity=max_fidelity,
            ),
            intensifier=smac_intensifier,
            overwrite=True,
        )
    elif facade == "bb":
        smac = BlackBoxFacade(
            scenario=smac_scenario,
            target_function=partial(
                target_function,
                benchmark=benchmark,
                scenario=scenario,
                instance=instance,
                target_variable=target_variable,
                direction=direction,
                fidelity_param_id=fidelity_param_id,
                on_integer_scale=on_integer_scale,
                max_fidelity=max_fidelity,
            ),
            intensifier=smac_intensifier,
            overwrite=True,
        )

    smac.optimize()

    trial_data = []
    for trial_info, trial_value in smac.runhistory.items():
        trial_entry = {
            "config_id": trial_info.config_id,
            "cost": trial_value.cost,
        }
        config = smac.runhistory.get_config(trial_info.config_id)
        config_entry = fix_config(
            config,
            benchmark=benchmark,
            scenario=scenario,
            fidelity_param_id=fidelity_param_id,
            on_integer_scale=on_integer_scale,
            max_fidelity=max_fidelity,
        )
        trial_data.append(trial_entry | config_entry)
    data = pd.DataFrame(trial_data)
    data.rename(columns={"cost": target_variable}, inplace=True)

    shutil.rmtree(output_directory)
    return data
