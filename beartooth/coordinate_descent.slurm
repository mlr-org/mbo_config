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

source beartooth/modules

Rscript beartooth/coordinate_descent.R
