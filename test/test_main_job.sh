#!/bin/bash
#SBATCH --job-name=TestMainJob
#SBATCH --output=test_main_job_output.txt
#SBATCH --time=00:10:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH -A eap-larsko
#SBATCH -p mb

echo "Starting the test main job..."

# Perform some initial tasks
sleep 10

echo "Submitting the test next job..."

# Submit the next job
sbatch test_next_job.sh

echo "Test main job finished."