##########################################################################################
# 2. Run clustering procedure
# INPUT: Cleaned .rds file from "_cleandata.R"
# OUTPUT: .csv file containing ASW values for each clustering result: "ASW_cc_#cluster.csv"
# This script is designed to be used on a clustering node for faster parallel computing, 
# but can be run on a local device as well; be sure to comment/uncomment the relevant code
##########################################################################################

library(parallel)
library(doParallel)
library(parallelDist)

# Change if clustering node directory is different
source("./input_files/EC_user_functions.R")

# Country-code
cc <- "GN18"

# Set the number of asset variables to cluster on (default is 4)
num_cluster <- 4

# If running this script on an external cluster:
nCores <- as.numeric(Sys.getenv('SLURM_CPUS_ON_NODE'))
doParallel::registerDoParallel(nCores)

# If running this script on your local device:
# nCores <- as.numeric(parallel::detectCores())
# doParallel::registerDoParallel(nCores)

# read cleaned data from correct directory (different if on cluster)
dataclean <- readRDS(paste0("./data/cleaned/", cc, "_clean.rds"))

output <- calcASW(dataclean, num.in.cluster = num_cluster, nCores)

# If running on an external cluster, change output directory accordingly
saveRDS(output[[2]], paste0("./data/asw/ASW_", cc, "_", num_cluster, "cluster.rds"))
print(output[[3]]) # Print runtime
print("DONE")
