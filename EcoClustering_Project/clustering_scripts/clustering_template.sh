#!/bin/bash

####################
# SBATCH OPTIONS
####################
#SBATCH --job-name=EcoClustering_cc
#SBATCH --account=co_biostat
#SBATCH --partition=savio3
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=32
#SBATCH --time=30:00:00
#SBATCH --output=../slurm_output/clustering_cc.out
#SBATCH --error=../slurm_output/clustering_cc.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=___@berkeley.edu

####################
# What to run (replace cc with correct country code [above too])
####################

module load r
module load r-packages
module load r-spatial
R CMD BATCH --no-save ./R/clustering_cc.R ./R_output/clustering_cc.Rout