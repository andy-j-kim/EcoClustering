##########################################################################################
# 1. Import and clean data
# INPUT: Raw DHS Household .dta file
# OUTPUT: Cleaned .rds file (includes weighting variable, 
# 20-40 binary asset variables and multicategory asset variables)
##########################################################################################

library(haven)
library(tidyverse)
source("./input_files/EC_user_functions.R")

# Read in the raw DHS .dta file
dataset <- haven::read_dta("~/Box/Project 1/DHS Data/MADAGASCAR (2021) STANDARD DHS DATASET/MADAGASCAR (2021) STANDARD DHS DATASET/MD_2021_DHS_03242024_75_171933/MDHR81DT/MDHR81FL.DTA")

# Country-code (e.g. "CM18" for Cameroon 2018)
cc <- "MD21"

# Set threshold for maximum allowable missingness (default is 10%)
max_prop_NA <- 0.1

# Set threshold for maximum allowable imbalance (default is 10%)
max_imbalance <- 0.1

# variable selection
dataclean <- dataset %>% 
  #1
  dplyr::mutate(wt = ifelse(hv012==0, hv013, hv012)*hv005/1e+06) %>% 
  #2
  dplyr::select(-contains('_')) %>% 
  #3
  dplyr::select(wt, hv000, dplyr::starts_with(c('hv2', 'sh1'))) %>%
  #4
  preprocess_dhsfactors() %>% 
  #5
  dplyr::select(where(~sum(is.na(.x))/length(.x) < max_prop_NA)) %>%
  dplyr::filter(hv201a != 8, hv237 != 8) %>% 
  #6
  dplyr::select(-which(purrr::map_lgl(., detect_imbalance, max_imbalance)), wt) %>% 
  
  #7
  dplyr::select(-c(hv237a, hv246b, hv246d, hv246e, hv246i, hv252)) %>% 

  # move weights to front of the dataset
  dplyr::select(wt, everything())

saveRDS(dataclean, file = paste0("./data/cleaned/", cc, "_clean.rds"))
