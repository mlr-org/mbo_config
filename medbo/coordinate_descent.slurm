#!/bin/bash
#SBATCH --account=eap-larsko
#SBATCH --job-name=coordinate_descent
#SBATCH --output=coordinate_descent.log
#SBATCH --error=coordinate_descent.log
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=96
#SBATCH --mem=700G
#SBATCH --time=72:00:00
#SBATCH --partition=mb

source /home/mbecke16/mbo_config/medbo/modules

Rscript /home/mbecke16/mbo_config/coordinate_descent.R