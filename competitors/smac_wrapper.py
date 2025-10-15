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
from smac import BlackBoxFacade, HyperparameterOptimizationFacade, Scenario
from smac.intensifier import Intensifier

from config import (
    SCENARIO_META_DATA,
    YAHPO_DATA_PATH,
    YAHPO_SCRIPT_PATH,
    YAHPO_VENV_PATH,
)
from configspace_utils import fix_config, remove_hyperparameters


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
    output_directory = (
        f"smac4{facade}_tmp_"
        + str(seed)
        + "_"
        + str(random.randrange(49152, 65535 + 1))
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

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--benchmark", required=True, choices=["pure_numeric", "mixed", "mixed_deps"])
    parser.add_argument("--scenario", required=True)
    parser.add_argument("--instance", required=True)
    parser.add_argument("--target_variable", required=True)
    parser.add_argument("--direction", required=True, choices=["minimize", "maximize"])
    parser.add_argument("--budget", required=True, type=int)
    parser.add_argument("--seed", required=True, type=int)
    parser.add_argument("--output_path", required=True)
    parser.add_argument("--facade", required=True, choices=["hpo", "bb"])
    args = parser.parse_args()

    data = run_smac(
        benchmark=args.benchmark,
        scenario=args.scenario,
        instance=args.instance,
        target_variable=args.target_variable,
        direction=args.direction,
        budget=args.budget,
        seed=args.seed,
        facade=args.facade,
    )

    data.to_csv(args.output_path, index=False)

