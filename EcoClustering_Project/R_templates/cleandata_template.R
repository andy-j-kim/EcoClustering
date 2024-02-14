##########################################################################################
# 1. Import and clean data
# INPUT: Raw DHS Household .dta file
# OUTPUT: Cleaned .rds file (includes weighting variable, 
# 20-40 binary asset variables and multicategory asset variables)
##########################################################################################

library(haven)
library(tidyverse)
source("./input_files/EC_user_functions.R")

##########################################################################################
# IMPORTANT: Check "Standard_Recode_Manual_for_DHS7_Recode7_DHS_10Sep2018_DHSG4.pdf"
# for DHS-7 variable coding details and discrepancies
#
# Note on cleaning DHS data:
# This script aims to provide as systematized of an approach for variable selection
# across all DHS country files as possible. The aim of this script is to end up with ~20-40 
# asset variables for the clustering algorithm to run within a reasonable amount of time on 
# a standard computer with 8 or 16 cores. All selected variables should either be binary or 
# one of the processed multicategorical variables. Nevertheless, each country will require 
# its own cleaning script due to unique and important discrepancies in the DHS data. 
#
# For instance, for some countries, inclusion of certain non-binary data (e.g. variables under 
# "hv246_": number of livestock owned) may result in unstable, or uninterpretable clustering 
# results. We suggest using the relevant base binary asset variable (as in variable "hv246") 
# in this case. 
#
# Due to the country-specific nature of DHS data, other discrepancies will have to be accounted 
# for ad-hoc. This current presents a significant drawback in our aim to standardize a procedure 
# to handle DHS data, and thus we recommend reporting all variables included in the clustering 
# procedure in order to be maximally transparent.
##########################################################################################


# Read in the raw DHS .dta file
dataset <- haven::read_dta("./data/")

# Country-code (e.g. "CM18" for Cameroon 2018)
cc <- ""

# Set threshold for maximum allowable missingness (default is 10%)
max_prop_NA <- 0.1

# Set threshold for maximum allowable imbalance (default is 10%). For a variable to be meaningful
# as a potential survey candidate, at least #% households in the specific country data should have
# or not have the relevant asset.

max_imbalance <- 0.1

# Step-by-step guide for variable selection

# 1. Generate weighting variable
# 2. Filter out variables containing "_" (avoids hierarchical data)
# 3. Select variables starting with "hv2" or "sh1" 
# 4. Recategorize the multicategorical asset variables (e.g. fuel/water source, roof material)
# 5. Select variables that meet missingness criteria
# 6. Select variables that meet balance criteria
# 7. (Optional) Filter out any additional variables by hand 

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
  #6
  dplyr::select(-which(purrr::map_lgl(., detect_imbalance, max_imbalance)), wt) %>% 
  #7
  dplyr::select(-c(hv246a, hv246b, hv246e, hv246g))

saveRDS(dataclean, file = paste0("./data/", cc, "_clean.rds"))
