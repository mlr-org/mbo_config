import subprocess
import tempfile
import json
import os
import re

from functools import partial
import copy
import random
import pandas as pd
import numpy as np
import shutil

from ConfigSpace import ConfigurationSpace
from smac import HyperparameterOptimizationFacade, Scenario

SCENARIO_META_DATA = {
    "lcbench": {"fidelity_param_id": "epoch", "on_integer_scale": True, "max_fidelity": 52, "params_to_remove": ["epoch", "OpenML_task_id"]},
    "nb301": {"fidelity_param_id": "epoch", "on_integer_scale": True, "max_fidelity": 98, "params_to_remove": ["epoch"]},
    "rbv2_glmnet": {"fidelity_param_id": "trainsize", "on_integer_scale": False, "max_fidelity": 1.0, "params_to_remove": ["trainsize", "repl", "task_id"]},
    "rbv2_rpart": {"fidelity_param_id": "trainsize", "on_integer_scale": False, "max_fidelity": 1.0, "params_to_remove": ["trainsize", "repl", "task_id"]},
    "rbv2_ranger": {"fidelity_param_id": "trainsize", "on_integer_scale": False, "max_fidelity": 1.0, "params_to_remove": ["trainsize", "repl", "task_id"]},
    "rbv2_xgboost": {"fidelity_param_id": "trainsize", "on_integer_scale": False, "max_fidelity": 1.0, "params_to_remove": ["trainsize", "repl", "task_id"]},
    "rbv2_super": {"fidelity_param_id": "trainsize", "on_integer_scale": False, "max_fidelity": 1.0, "params_to_remove": ["trainsize", "repl", "task_id"]},
}

YAHPO_DATA_PATH = "/glade/derecho/scratch/lschneider/yahpo_data/"


def remove_hyperparameters(cs_original, params_to_remove, seed):
    # only works for fidelity parameters or instance parameters without conditions and/or forbidden clasuses
    cs = copy.deepcopy(cs_original)
    hyperparameters = list(cs.values())

    params_to_remove_idx = [
        list(cs.keys()).index(param)
        for param in params_to_remove
    ]
    params_to_remove_idx.sort()
    params_to_remove_idx.reverse()
    for idx in params_to_remove_idx:
        del hyperparameters[idx]

    conditions = cs.conditions
    forbidden_clauses = cs.forbidden_clauses
    cs = ConfigurationSpace(seed=seed)
    cs.add(hyperparameters)
    cs.add(conditions)
    cs.add(forbidden_clauses)
    return cs


def target_function(config, seed, scenario, instance, target_variable, factor, fidelity_param_id, on_integer_scale, max_fidelity):
    X = dict(config)
    if "rbv2_" in scenario:
        X.update({"repl": 10})  # manual fix required for rbv2_
    X.update({fidelity_param_id: int(round(max_fidelity)) if on_integer_scale else max_fidelity})
    input_data = {
        "X": X
    }
    with tempfile.NamedTemporaryFile(mode="w+", delete=False) as f:
        json.dump(input_data, f)
        input_path = f.name

    script_path = "/glade/u/home/lschneider/mbo_config/subprocess_yahpo.py"
    venv_path = "/glade/u/home/lschneider/mbo_config/yahpo_venv/bin/python"

    cmd = [
        venv_path, script_path,
        "--input_path", input_path,
        "--scenario", scenario,
        "--instance", instance,
        "--target_variable", target_variable,
        "--seed", str(seed)
    ]
    result = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    os.unlink(input_path)

    if result.returncode != 0:
        raise RuntimeError(f"Subprocess failed:\nSTDOUT: {result.stdout}\nSTDERR: {result.stderr}")

    match = re.search(r'({.*})\s*$', result.stdout.strip(), re.DOTALL)
    if not match:
        raise RuntimeError(f"Could not extract JSON from subprocess output:\n{result.stdout}")

    try:
        output = json.loads(match.group(1))
    except json.JSONDecodeError as e:
        raise RuntimeError(f"Failed to parse JSON from subprocess output:\nMatched String: {match.group(1)}\nError: {e}")

    return factor * output[target_variable]

def run_smac4hpo(scenario, instance, target_variable, direction, budget, seed):
    random.seed(seed)
    np.random.seed(seed)

    config_space = ConfigurationSpace().from_json(os.path.join(YAHPO_DATA_PATH, scenario, "config_space_smac.json"))
    opt_space = remove_hyperparameters(config_space, params_to_remove=SCENARIO_META_DATA[scenario]["params_to_remove"], seed=seed)
    opt_space.seed(seed)
    factor = 1 if direction == "minimize" else -1
    fidelity_param_id = SCENARIO_META_DATA[scenario]["fidelity_param_id"]
    on_integer_scale = SCENARIO_META_DATA[scenario]["on_integer_scale"]
    max_fidelity = SCENARIO_META_DATA[scenario]["max_fidelity"]
    output_directory = "smac4hpo_tmp_" + str(seed) + "_" + str(random.randrange(49152, 65535 + 1))

    smac_scenario = Scenario(configspace=opt_space, output_directory=output_directory, deterministic=True, n_trials=budget, seed=seed)

    smac4hpo = HyperparameterOptimizationFacade(
        scenario=smac_scenario,
        target_function=partial(target_function, scenario=scenario, instance=instance, target_variable=target_variable, factor=factor, fidelity_param_id=fidelity_param_id, on_integer_scale=on_integer_scale, max_fidelity=max_fidelity),
        overwrite=True
    )

    smac4hpo.optimize()

    trial_data = []
    for trial_info, trial_value in smac4hpo.runhistory.items():
        trial_entry = {
            "config_id": trial_info.config_id,
            "cost": trial_value.cost,
        }
        config_entry = dict(smac4hpo.runhistory.get_config(trial_info.config_id))
        if "rbv2_" in scenario:  # manual fix required for rbv2_
            config_entry.update({"repl": 10})
            config_entry.update({"trainsize": 1.0})
        else:
            config_entry.update({fidelity_param_id: max_fidelity})
        trial_data.append(trial_entry | config_entry)
    data = pd.DataFrame(trial_data)
    data.rename(columns={"cost": target_variable}, inplace=True)

    shutil.rmtree(output_directory)
    return data
