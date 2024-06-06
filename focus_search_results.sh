#!/bin/bash
#SBATCH --account=eap-larsko
#SBATCH --job-name=focus_search_results
#SBATCH --output=output_%j.txt
#SBATCH --error=output_%j.txt
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=96
#SBATCH --mem=30G
#SBATCH --time=24:00:00
#SBATCH --partition=mb

source modules

Rscript focus_search_results.R