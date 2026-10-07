# EcoClustering
Economic Clustering Project - part of D-SINE

## Data

This repository does not include DHS data. The DHS Program's terms of use do
not allow micro-level data to be redistributed, so each user must register at
https://dhsprogram.com/data/ and download the Household Recode (HR) and
Individual Recode (IR) Stata files for each country. Place them under
`EcoClustering_Project/data/dhs/<Country_Year>/`, for example
`data/dhs/Cameroon_2018/CMHR71DT/CMHR71FL.DTA`. The cleaning scripts in
`EcoClustering_Project/R/cleandata/` then rebuild the cleaned files in
`data/cleaned/`, which are also not tracked because they are household-level.
