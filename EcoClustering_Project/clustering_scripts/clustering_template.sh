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
#SBATCH --time=20:00:00
#SBATCH --mail-type=ALL
#SBATCH --mail-user=___@berkeley.edu

####################
# What to run (replace cc with correct country code [above too])
####################

module load r
module load r-packages
module load r-spatial
R CMD BATCH --no-save ../EcoClustering/R/clustering_cc.R ../EcoClustering/R_output/clustering_cc.Rout