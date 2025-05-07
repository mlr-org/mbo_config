import json
import os
import random
import re
import subprocess
import tempfile

import numpy as np
import pandas as pd
from ax.service.ax_client import AxClient, ObjectiveProperties
from ConfigSpace import (
    CategoricalHyperparameter,
    ConfigurationSpace,
    UniformFloatHyperparameter,
    UniformIntegerHyperparameter,
)

from config import (
    SCENARIO_META_DATA,
    YAHPO_DATA_PATH,
    YAHPO_SCRIPT_PATH,
    YAHPO_VENV_PATH,
)
from configspace_utils import clip_to_bounds, fix_config, remove_hyperparameters


def configspace_to_ax_parameters(cs):
    ax_params = []
    for hp in list(cs.values()):
        param = {"name": hp.name}
        if hasattr(hp, "lower") and hasattr(hp, "upper"):
            param["type"] = "range"
            param["bounds"] = [hp.lower, hp.upper]
            if isinstance(hp, (UniformIntegerHyperparameter,)):
                param["value_type"] = "int"
            elif isinstance(hp, (UniformFloatHyperparameter,)):
                param["value_type"] = "float"
            else:
                raise NotImplementedError(f"Unsupported parameter type: {type(hp)}")
            if getattr(hp, "log", False):
                param["log_scale"] = True
        elif isinstance(hp, CategoricalHyperparameter):
            param["type"] = "choice"
            param["values"] = hp.choices
            if set(hp.choices) == {True, False} or set(hp.choices) == {False, True}:
                param["value_type"] = "bool"
            else:
                param["value_type"] = "str"
        else:
            raise NotImplementedError(f"Unsupported parameter type: {type(hp)}")
        ax_params.append(param)
    return ax_params


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


def run_ax(
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
    ax_parameters = configspace_to_ax_parameters(opt_space)
    fidelity_param_id = SCENARIO_META_DATA[scenario]["fidelity_param_id"]
    on_integer_scale = SCENARIO_META_DATA[scenario]["on_integer_scale"]
    max_fidelity = SCENARIO_META_DATA[scenario]["max_fidelity"]

    ax_client = AxClient(
        enforce_sequential_optimization=True, random_seed=seed
    )  # will automatically be BO
    ax_client.create_experiment(
        name=f"ax_{benchmark}_{scenario}_{instance}_{target_variable}_{seed}",
        parameters=ax_parameters,
        objectives={target_variable: ObjectiveProperties(minimize=True)},
        overwrite_existing_experiment=True,
    )
    if ax_client.generation_strategy.name != "Sobol+BoTorch":
        raise ValueError("AX did not automatically select a BO strategy.")

    configs = []
    targets = []

    for i in range(budget):
        config, trial_index = ax_client.get_next_trial()
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
        ax_client.complete_trial(trial_index=trial_index, raw_data=target)
        configs.append(config)
        targets.append(target)

    trial_data = []
    for i in range(budget):
        trial_entry = {"config_id": i, "target": targets[i]}
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
