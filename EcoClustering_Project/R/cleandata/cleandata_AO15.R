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
dataset <- haven::read_dta("~/Box/Project 1/DHS Data/ANGOLA 2015 - 2016/AOHR71DT/AOHR71FL.DTA")

# Country-code (e.g. "CM18" for Cameroon 2018)
cc <- "AO15"

# Set threshold for maximum allowable missingness (default is 10%)
max_prop_NA <- 0.1

# Set threshold for maximum allowable imbalance (default is 10%)
max_imbalance <- 0.07

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
  dplyr::filter(hv237 != 8,
                hv253 != 8) %>% # loses about 1.56% of dataset (filtered before detecting imbalance)
  #6
  dplyr::select(-which(purrr::map_lgl(., detect_imbalance, max_imbalance)), wt) %>% 
  #7
  dplyr::select(-c(hv237b, hv246g, hv246h, hv252)) %>% 

  # move weights to front of the dataset
  dplyr::select(wt, everything())

saveRDS(dataclean, file = paste0("./data/cleaned/", cc, "_clean.rds"))
