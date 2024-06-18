#!/bin/bash
#SBATCH --job-name=TestNextJob
#SBATCH --output=test_next_job_output.txt
#SBATCH --time=00:10:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH -A eap-larsko
#SBATCH -p mb

echo "This is the test next job."

# Perform tasks in the next job
sleep 20

echo "Test next job finished."