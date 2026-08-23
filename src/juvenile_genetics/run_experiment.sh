#!/bin/bash
#SBATCH --partition=lotterhos
#SBATCH --exclude=d4048
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --mem=180G
#SBATCH --time=36:00:00
#SBATCH --job-name=experiment
#SBATCH --output=experiment_%j.log
#SBATCH --error=experiment_%j.err
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=eppley.m@northeastern.edu

cd /projects/gatins/2025_Mobulid/hetfst
/home/eppley.m/anaconda3/envs/hierfstat/bin/Rscript run_experiment.R
