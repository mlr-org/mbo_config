import json
import os
import random
import re
import subprocess
import tempfile

import numpy as np
import pandas as pd
from ConfigSpace import (
    CategoricalHyperparameter,
    ConfigurationSpace,
    UniformFloatHyperparameter,
    UniformIntegerHyperparameter,
)
from hebo.design_space.design_space import DesignSpace
from hebo.optimizers.hebo import HEBO

from config import (
    SCENARIO_META_DATA,
    YAHPO_DATA_PATH,
    YAHPO_SCRIPT_PATH,
    YAHPO_VENV_PATH,
)
from configspace_utils import fix_config, remove_hyperparameters


def configspace_to_hebo_designspace(cs):
    hebo_params = []

    for hp in cs.get_hyperparameters():
        name = hp.name

        if isinstance(hp, UniformFloatHyperparameter):
            if hp.log:
                hebo_params.append(
                    {
                        "name": name,
                        "type": "pow",
                        "lb": hp.lower,
                        "ub": hp.upper,
                        "base": np.exp(1),
                    }
                )
            else:
                hebo_params.append(
                    {"name": name, "type": "num", "lb": hp.lower, "ub": hp.upper}
                )
        elif isinstance(hp, UniformIntegerHyperparameter):
            if hp.log:
                hebo_params.append(
                    {
                        "name": name,
                        "type": "pow_int",
                        "lb": hp.lower,
                        "ub": hp.upper,
                        "base": np.exp(1),
                    }
                )
            else:
                hebo_params.append(
                    {"name": name, "type": "int", "lb": hp.lower, "ub": hp.upper}
                )

        elif isinstance(hp, CategoricalHyperparameter):
            choices = hp.choices
            if set(choices) == {True, False} or set(choices) == {False, True}:
                hebo_params.append({"name": name, "type": "bool"})
            else:
                hebo_params.append({"name": name, "type": "cat", "categories": choices})
        else:
            raise NotImplementedError(
                f"Unsupported hyperparameter type: {type(hp)} for {name}"
            )

    return DesignSpace().parse(hebo_params)


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


def run_hebo(
    benchmark,
    scenario,
    instance,
    target_variable,
    direction,
    budget,
    seed,
):
    random.seed(seed)
    np.random.seed(seed)  # also seeds HEBO's design space

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
    hebo_designspace = configspace_to_hebo_designspace(opt_space)
    fidelity_param_id = SCENARIO_META_DATA[scenario]["fidelity_param_id"]
    on_integer_scale = SCENARIO_META_DATA[scenario]["on_integer_scale"]
    max_fidelity = SCENARIO_META_DATA[scenario]["max_fidelity"]
    hebo = HEBO(hebo_designspace)

    configs = []
    targets = []
    hebo_fallbacks = []

    for i in range(budget):
        try:
            config_raw = hebo.suggest(n_suggestions=1)
            hebo_fallback = False
        except:
            config_raw = hebo_designspace.sample(num_samples=1)
            hebo_fallback = True
        config = config_raw.to_dict(orient="records")[0]
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
        hebo.observe(config_raw, y=np.array([[target]]))
        configs.append(config)
        targets.append(target)
        hebo_fallbacks.append(hebo_fallback)

    trial_data = []
    for i in range(budget):
        trial_entry = {
            "config_id": i,
            "target": targets[i],
            "hebo_fallback": hebo_fallbacks[i],
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
    data.rename(columns={"target_variable": target_variable}, inplace=True)

    return data
