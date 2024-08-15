#!/bin/bash
#SBATCH --account=eap-larsko
#SBATCH --job-name=aggregate_random_search
#SBATCH --output=aggregate_random_search.log
#SBATCH --error=aggregate_random_search.log
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=96
#SBATCH --mem=700G
#SBATCH --time=72:00:00
#SBATCH --partition=mb

source /home/mbecke16/mbo_config/modules

Rscript /home/mbecke16/mbo_config/random_search/aggregate_random_search.R
