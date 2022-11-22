#' This script runs the different scripts to 
#' impute the missing values multiple times
#' compute the sampling weights and the calibrated weights for the different rounds 
#' following different scenarii


rm(list = ls())

###
# Packages
###

if (!require("pacman")) install.packages("pacman")
library(pacman)

p_load(Amelia)
p_load(descr)
p_load(reshape2)
p_load(sampling)
p_load(lubridate)
p_load(tidyverse)
p_load(data.table)
p_load(ggplot2)
p_load(ggpattern)
p_load(gg.gap)
p_load(cowplot)
p_load(pdftools)
p_load(ggrepel)
p_load(scales)
p_load(car)
p_load(openxlsx)
p_load(naniar)
p_load(mice)


###
# Scripts
###

here_koco_scripts = function (...) here::here("Scripts", ...)

###
# Multiple imputation for NR mechanism and risk factor analysis
###

source(here_koco_scripts("NR_mechanism_MI.R"))


###
# Study NR mechanism with dummy encoding for missing values of income
###

source(here_koco_scripts("NR_mechanism_NA.R"))



###
# Multiple imputation for vaccination status and serological status
###

# Number of iterations
seeds <- 1:100

# Create multiple imputed datasets
source(here_koco_scripts("Imputation.R"))

for(i in seeds){
  seed = i
  impute(seed)
}

###
# Unweighted Results
###

### Seroprevalence

source(here_koco_scripts("Unweighted_seroprevalence.R"))

run_sp_boot(boot = 5000)

### Seroincidence, seroincidence vax no vax, seroprevalence vax no vax

source(here_koco_scripts("Unweighted_seroestimates.R"))

# Number of iterations
seeds <- 1:100

for(i in seeds){
  seed = i
  run_boot(seed, boot = 5000)
}

# Aggregate results
source(here_koco_scripts("Aggregate_results_uw.R"))
aggregate_res_uw(seeds)


###
# Sampling weights
###

source(here_koco_scripts("Sampling_weights.R"))


###
# Round 1
###

source(here_koco_scripts("R1_Seroprevalence.R"))

###
# Round 2
###

# Calibrated weights
source(here_koco_scripts("R2_Cumulative_Seroprevalence.R"))


###
# Round 3
###

### Calibrated weights including calibration on vaccination
source(here_koco_scripts("R3_Cumulative_Seroprevalence.R"))

# Number of iterations
seeds <- 1:100

for(i in seeds){
  seed = i
  run(seed)
}

# Aggregate results
source(here_koco_scripts("Aggregate_results_w.R"))
aggregate_res_w(seeds = seeds, r = 3, cal_vax = TRUE)

### Calibrated weights without calibration on vaccination
source(here_koco_scripts("R3_Cumulative_Seroprevalence_no_cal.R"))

# Number of iterations
seeds <- 1:100

for(i in seeds){
  seed = i
  run(seed)
}

# Aggregate results
aggregate_res_w(seeds = seeds, r = 3, cal_vax = FALSE)

###
# Round 4
###

### Calibrated weights including calibration on vaccination
source(here_koco_scripts("R4_Cumulative_Seroprevalence.R"))

# Number of iterations
seeds <- 1:100

for(i in seeds){
  seed = i
  run(seed)
}

# Aggregate results
aggregate_res_w(seeds = seeds, r = 4, cal_vax = TRUE)


### Calibrated weights without calibration on vaccination
source(here_koco_scripts("R4_Cumulative_Seroprevalence_no_cal.R"))

# Number of iterations
seeds <- 1:100

for(i in seeds){
  seed = i
  run(seed)
}

# Aggregate results
aggregate_res_w(seeds = seeds, r = 4, cal_vax = FALSE)

###
# Round 5
###

### Calibrated weights including calibration on vaccination
source(here_koco_scripts("R5_Cumulative_Seroprevalence.R"))

# Number of iterations
seeds <- 1:100

for(i in seeds){
  seed = i
  run(seed)
}

# Aggregate results
aggregate_res_w(seeds = seeds, r = 5, cal_vax = TRUE)


### Calibrated weights without calibration on vaccination
source(here_koco_scripts("R5_Cumulative_Seroprevalence_no_cal.R"))

# Number of iterations
seeds <- 1:100

for(i in seeds){
  seed = i
  run(seed)
}

# Aggregate results
aggregate_res_w(seeds = seeds, r = 5, cal_vax = FALSE)


###
# Plots
###

source(here_koco_scripts("Plots_seroprevalence.R"))

