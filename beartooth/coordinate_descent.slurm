#!/bin/bash
#SBATCH --account=mallet
#SBATCH --job-name=coordinate_descent
#SBATCH --output=coordinate_descent.log
#SBATCH --error=coordinate_descent.log
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=72:00:00
#SBATCH --partition=teton

source /home/mbecke16/mbo_config/beartooth/modules

Rscript /home/mbecke16/mbo_config/coordinate_descent.R
