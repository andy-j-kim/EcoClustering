library(devtools)
library(EconomicClusters) #devtools::install_github("Lauren-Eyler/EconomicClusters")
library(haven)
library(nloptr)
#library(weights)
library(parallelDist) #install.packages('parallelDist')
library(foreach)
library(doParallel)
#library(survey) #install.packages('survey')
library(Hmisc)
library(descr) #install.packages('descr')
library(tidyverse)
library(car) #install.packages('car')
library(stats)
library(future) #install.packages('future')

source("./EC_fxns.R")

path <- "~/Box/Project 1/DHS Data/CAMEROON (CM) 2018/CM_2018_DHS_01312023_2251_171933.zip"
dataset <- read_dta(unzip(zipfile = path , files = "CMHR71DT/CMHR71FL.DTA"))

#nCores <- as.numeric(Sys.getenv('SLURM_CPUS_ON_NODE'))
#registerDoParallel(nCores)

# clean data
dataclean <- dataset %>% 
  select(-contains('_')) %>% 
  select(hv000, hv012, hv013, hv005, 
         starts_with('hv2'), matches('sh1')) %>% 
  select(where(~sum(is.na(.x))/length(.x) < 0.1)) %>%  # Less than 10% of any column can contain NAs
  preprocess_dhsfactors()

wt <- ifelse(dataclean$hv012 == 0, dataclean$hv013, dataclean$hv012)*dataclean$hv005/1e+06

dataclean <- dataclean %>% 
  select(-c(hv012, hv013, hv005)) %>% 
  select(-c(hv271,  hv220, hv216, hv217, hv218,
            hv246a, hv246b, hv246e, hv246g
            ))

vars_to_remove <- which(apply(dataclean, 2, function(i){
  mean(as.numeric(i), na.rm = T)
}) < 0.1 | apply(dataclean, 2, function(i){
  mean(as.numeric(i), na.rm = T)
}) > 0.9 )

dataclean <- dataclean[-vars_to_remove]
dataclean2 <- cbind(wt, dataclean)

rm(dataset)

saveRDS(dataclean2, file = "./cleanedDHSdata/CM18_clean.rds")
