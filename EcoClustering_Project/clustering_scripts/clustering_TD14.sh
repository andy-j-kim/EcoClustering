#!/bin/bash

####################
# SBATCH OPTIONS
####################
#SBATCH --job-name=EcoClustering_TD14
#SBATCH --account=co_biostat
#SBATCH --partition=savio3
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=32
#SBATCH --time=30:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=a_kim@berkeley.edu

####################
# What to run (replace cc with correct country code [above too])
####################

module load r
module load r-packages
module load r-spatial
R CMD BATCH --no-save ../EcoClustering/R/clustering_TD14.R ../EcoClustering/R_output/clustering_TD14.Rout