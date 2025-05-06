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

from config import SCENARIO_META_DATA


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
