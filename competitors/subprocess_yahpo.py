import argparse
import json
from yahpo_gym import benchmark_set

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--input_path", required=True, help="Path to JSON file with input configuration")
    parser.add_argument("--scenario", required=True, help="YAHPO Gym scenario")
    parser.add_argument("--instance", required=True, help="YAHPO Gym instance")
    parser.add_argument("--target_variable", required=True, help="Target variable")
    parser.add_argument("--seed", required=True, help="Random seed")
    args = parser.parse_args()

    bench = benchmark_set.BenchmarkSet(args.scenario, instance=args.instance, multithread=False)
    bench.config_space.seed(int(args.seed))
    with open(args.input_path, "r") as f:
        input_data = json.load(f)
    X = input_data["X"]
    if args.scenario != "nb301":
        X.update({bench.config.instance_names: args.instance})

    y = bench.objective_function(X, seed=int(args.seed), logging=False, multithread=False)[0]
    print(json.dumps({args.target_variable: float(y.get(args.target_variable))}))

if __name__ == "__main__":
    main()

