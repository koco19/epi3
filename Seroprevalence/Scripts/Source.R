#' This script runs the different scripts to 
#' impute the missing values for the vaccination status multiple times
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

here_koco_data = function (...) here::here("Data", ...)
here_koco_prev = function (...) here::here("Seroprevalence", ...)
here_prev_figures = function (...) here_koco_prev("Figures", ...)
here_prev_results = function (...) here_koco_prev("Results", ...)
here_prev_scripts = function (...) here_koco_prev("Scripts", ...)


###
# Multiple imputation for vaccination status and serological status
###

# Number of iterations
seeds <- 1:100

# Create multiple imputed datasets
source(here_prev_scripts("Imputation.R"))

for(i in seeds){
  seed = i
  impute(seed)
}

###
# Unweighted Results
###

### Seroprevalence

rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

source(here_prev_scripts("Unweighted_seroprevalence.R"))

run_sp_boot(boot = 5000)

### Seroincidence, seroincidence vax no vax, seroprevalence vax no vax

rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

source(here_prev_scripts("Unweighted_seroestimates.R"))

# Number of iterations
seeds <- 1:100

for(i in seeds){
  seed = i
  run_boot(seed, boot = 5000)
}

# Aggregate results
rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

# Number of iterations
seeds <- 1:100

source(here_prev_scripts("Aggregate_results_uw.R"))
aggregate_res_uw(seeds)


###
# Sampling weights
###

rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

source(here_prev_scripts("Sampling_weights.R"))


###
# Round 1
###

rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

source(here_prev_scripts("R1_Seroprevalence.R"))

###
# Round 2
###

# Calibrated weights
rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

source(here_prev_scripts("R2_Cumulative_Seroprevalence.R"))


###
# Round 3
###

### Calibrated weights including calibration on vaccination
rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

source(here_prev_scripts("R3_Cumulative_Seroprevalence.R"))

# Number of iterations
seeds <- 1:100

for(i in seeds){
  seed = i
  run(seed)
}

# Aggregate results
rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

# Number of iterations
seeds <- 1:100

source(here_prev_scripts("Aggregate_results_w.R"))
aggregate_res_w(seeds = seeds, r = 3, cal_vax = TRUE)

### Calibrated weights without calibration on vaccination
rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

source(here_prev_scripts("R3_Cumulative_Seroprevalence_no_cal.R"))

# Number of iterations
seeds <- 1:100

for(i in seeds){
  seed = i
  run(seed)
}

# Aggregate results
rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

# Number of iterations
seeds <- 1:100

source(here_prev_scripts("Aggregate_results_w.R"))
aggregate_res_w(seeds = seeds, r = 3, cal_vax = FALSE)

###
# Round 4
###

### Calibrated weights including calibration on vaccination
rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

source(here_prev_scripts("R4_Cumulative_Seroprevalence.R"))

# Number of iterations
seeds <- 1:100

for(i in seeds){
  seed = i
  run(seed)
}

# Aggregate results
rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

# Number of iterations
seeds <- 1:100

source(here_prev_scripts("Aggregate_results_w.R"))
aggregate_res_w(seeds = seeds, r = 4, cal_vax = TRUE)


### Calibrated weights without calibration on vaccination
rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

source(here_prev_scripts("R4_Cumulative_Seroprevalence_no_cal.R"))

# Number of iterations
seeds <- 1:100

for(i in seeds){
  seed = i
  run(seed)
}

# Aggregate results
rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

# Number of iterations
seeds <- 1:100

source(here_prev_scripts("Aggregate_results_w.R"))
aggregate_res_w(seeds = seeds, r = 4, cal_vax = FALSE)

###
# Round 5
###

### Calibrated weights including calibration on vaccination
rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

source(here_prev_scripts("R5_Cumulative_Seroprevalence.R"))

# Number of iterations
seeds <- 1:100

for(i in seeds){
  seed = i
  run(seed)
}

# Aggregate results
rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

# Number of iterations
seeds <- 1:100

source(here_prev_scripts("Aggregate_results_w.R"))
aggregate_res_w(seeds = seeds, r = 5, cal_vax = TRUE)


### Calibrated weights without calibration on vaccination
rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

source(here_prev_scripts("R5_Cumulative_Seroprevalence_no_cal.R"))

# Number of iterations
seeds <- 1:100

for(i in seeds){
  seed = i
  run(seed)
}

# Aggregate results
rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

# Number of iterations
seeds <- 1:100

source(here_prev_scripts("Aggregate_results_w.R"))
aggregate_res_w(seeds = seeds, r = 5, cal_vax = FALSE)


###
# Plots
###
rm(list = setdiff(ls(), c("here_koco_data", "here_prev_figures",
                          "here_koco_prev", "here_prev_results",
                          "here_prev_scripts")))

source(here_prev_scripts("Plots_seroprevalence.R"))

