## This is a convenience script that runs the data preparation workflow in order 
## to ensure that any changes made to the code do not break the workflow

## Code from Francisco J. Guerrero and Peter Regier
## Contact: peter.regier@pnnl.gov

## No outputs to check
source("scripts/0_setup.R")

## This script makes two datasets, neither of which is generated in the current
## code because bootstrapping changes them slightly everytime. The two outputs
## are called below to make sure they exist
source("scripts/1_analysis_cumulative_properties.R")
scaling_analysis_dat
scaling_analysis_accm_dat

## This script makes one dataset, which also isn't generated in the current code
## for the same bootstrapping reason. It's also output below
source("scripts/2_analysis_blocked_bootstrap_scaling.R")
as_tibble(reordered_results)

## This script makes one dataset, which also isn't generated in the current code
## for the same bootstrapping reason. It's also output below
source("scripts/3_analysis_cross_validation_blocked_bootstrap.R")
combined_results

## If all these scripts run and all datasets are generated correctly, you're good
## to go making figures!
