import json
import os
import random
import re
import subprocess
import tempfile

import numpy as np
import optuna
import pandas as pd
from ConfigSpace import (
    CategoricalHyperparameter,
    ConfigurationSpace,
    Configuration,
    OrdinalHyperparameter,
    UniformFloatHyperparameter,
    UniformIntegerHyperparameter,
)
from ConfigSpace.conditions import AndConjunction, OrConjunction
from optuna.samplers import TPESampler
from optuna.trial import Trial

from config import (
    SCENARIO_META_DATA,
    YAHPO_DATA_PATH,
    YAHPO_SCRIPT_PATH,
    YAHPO_VENV_PATH,
)
from configspace_utils import clip_to_bounds, fix_config, remove_hyperparameters


def get_value(hp_name, config_space, trial):
    hp = config_space.get_hyperparameter(hp_name)

    if isinstance(hp, UniformFloatHyperparameter):
        value = float(
            trial.suggest_float(name=hp_name, low=hp.lower, high=hp.upper, log=hp.log)
        )

    elif isinstance(hp, UniformIntegerHyperparameter):
        value = int(
            trial.suggest_int(name=hp_name, low=hp.lower, high=hp.upper, log=hp.log)
        )

    elif isinstance(hp, CategoricalHyperparameter):
        hp_type = type(hp.default_value)
        value = hp_type(trial.suggest_categorical(name=hp_name, choices=hp.choices))

    elif isinstance(hp, OrdinalHyperparameter):
        num_vars = len(hp.sequence)
        index = trial.suggest_int(hp_name, low=0, high=num_vars - 1, log=False)
        hp_type = type(hp.default_value)
        value = hp.sequence[index]
        value = hp_type(value)

    elif isinstance(hp, Constant):
        value = hp.value

    else:
        raise ValueError(f"Please implement the support for hps of type {type(hp)}")

    return value


def sample_config_from_optuna(trial, config_space):
    config = {}
    for hp_name in config_space.get_all_unconditional_hyperparameters():
        value = get_value(hp_name, config_space, trial)
        config.update({hp_name: value})

    conditions = config_space.get_conditions()
    conditional_hps = list(config_space.get_all_conditional_hyperparameters())
    n_conditions = dict(
        zip(
            conditional_hps,
            [len(config_space.get_parent_conditions_of(hp)) for hp in conditional_hps],
        )
    )
    conditional_hps_sorted = sorted(n_conditions, key=n_conditions.get)
    for hp_name in conditional_hps_sorted:
        conditions_to_check = np.where(
            [
                (
                    hp_name in [child.name for child in condition.get_children()]
                    if (
                        isinstance(condition, AndConjunction)
                        | isinstance(condition, OrConjunction)
                    )
                    else hp_name == condition.child.name
                )
                for condition in conditions
            ]
        )[0]
        checks = [
            conditions[to_check].satisfied_by_value(
                dict(
                    zip(
                        [parent.name for parent in conditions[to_check].get_parents()],
                        [
                            config.get(parent.name)
                            for parent in conditions[to_check].get_parents()
                        ],
                    )
                )
                if (
                    isinstance(conditions[to_check], AndConjunction)
                    | isinstance(conditions[to_check], OrConjunction)
                )
                else {
                    conditions[to_check].parent.name: config.get(
                        conditions[to_check].parent.name
                    )
                }
            )
            for to_check in conditions_to_check
        ]

        if sum(checks) == len(checks):
            value = get_value(hp_name, config_space, trial)
            config.update({hp_name: value})

    Configuration(config_space, config).check_valid_configuration()

    return config


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

    script_path = YAHPO_SCRIPT_PATH
    venv_path = YAHPO_VENV_PATH

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


def run_optuna(
    benchmark,
    scenario,
    instance,
    target_variable,
    direction,
    budget,
    seed,
):
    random.seed(seed)
    np.random.seed(seed)

    if benchmark == "pure_numeric":
        config_space = ConfigurationSpace().from_json(
            os.path.join(YAHPO_DATA_PATH, scenario, "config_space_pure_numeric.json")
        )
    elif benchmark == "mixed":
        raise ValueError("TBD")
    elif benchmark == "mixed_deps":
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

    study = optuna.create_study(
        direction="minimize", sampler=TPESampler(seed=seed)
    )  # target_function corrects sign

    configs = []
    targets = []

    for i in range(budget):
        trial = study.ask()
        config = sample_config_from_optuna(trial, config_space=opt_space)
        config = clip_to_bounds(config, config_space=opt_space)
        target = target_function(
            config,
            seed=seed,
            benchmark=benchmark,
            scenario=scenario,
            instance=instance,
            target_variable=target_variable,
            direction=direction,
            fidelity_param_id=fidelity_param_id,
            on_integer_scale=on_integer_scale,
            max_fidelity=max_fidelity,
        )
        study.tell(trial, values=target)
        configs.append(config)
        targets.append(target)

    trial_data = []
    for i in range(budget):
        trial_entry = {
            "config_id": i,
            "target": targets[i],
        }
        config = configs[i]
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
    data.rename(columns={"target": target_variable}, inplace=True)

    return data
