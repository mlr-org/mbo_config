#!/bin/bash
#SBATCH --account=eap-larsko
#SBATCH --job-name=focus_search_results
#SBATCH --output=focus_search_results.log
#SBATCH --error=focus_search_results.log
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=96
#SBATCH --mem=700G
#SBATCH --time=72:00:00
#SBATCH --partition=mb

source modules

Rscript focus_search_results.R