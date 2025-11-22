/home/marc/miniconda/envs/smac/bin/python smac3/wrapper_smac.py \
  --benchmark  mixed_deps \
  --scenario rbv2_xgboost \
  --instance 12 \
  --target_variable acc \
  --direction maximize \
  --budget 170 \
  --seed 1234 \
  --output_path smac3/results/rbv2_xgboost_mfeat-factors_12_accuracy.csv \
  --facade hpo \
  --keep_output \
  > smac3/output.log 2>&1



dart,4.06814327277243,,,1.22099471325055,1.31252249592217,0.496043365219889,,,,,0.203521782508555,0.00123617572762984,impute.mean,1

[[1]]
[[1]]$booster
[1] "dart"

[[1]]$nrounds
[1] 58

[[1]]$lambda
[1] 3.390559

[[1]]$alpha
[1] 3.715534

[[1]]$subsample
[1] 0.4960434

[[1]]$rate_drop
[1] 0.2035218

[[1]]$skip_drop
[1] 0.001236176

[[1]]$num.impute.selected.cpo
[1] "impute.mean"


