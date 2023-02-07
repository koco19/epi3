# Seroprevalence analyses

Codes for the analyses to compute the seroprevalence estimates. 

## Structure

The code is is organized as follow:

* `Source.R`: Source all scripts listed below (in this order).
* `Imputation.R`: Imputation of vaccination status and serological status. This script produces the RDS files `Koco_R5latest_data_seedx.RDS` (not available) stored in the folder `/Data/Imputed_Data`.
* `Unweighted_seroprevalence.R`: Computes unweighted seroprevalence estimates. This script prduces the RDS file `sp_uw.RDS` in the folder `/Seroprevalence/Results`.
* `Unweighted_seroestimates.R`: Computes unweighted sero-estimates (sero-incidence, seroprevalence depending on the vaccination status) using different imputed datasets produced by `Imputation.R`. This script prduces the RDS files `sp_vax_uw_seedx.RDS`, `si_uw_seedx.RDS` and `si_vax_uw_seedx.RDS` stored in the folder `/Seroprevalence/Results/Unweighted` (not available).
* `Aggregate_results_uw.R`: Computes the final unweighted sero-estimates (sero-incidence, seroprevalence depending on the vaccination status) for the different rounds using the RDS files `sp_vax_uw_seedx.RDS`, `si_uw_seedx.RDS` and `si_vax_uw_seedx.RDS` stored in the folder `/Seroprevalence/Results/Unweighted` (not available). This script produces the RDS files `sp_vax_uw.RDS`, `si_uw.RDS` and `si_vax_uw.RDS` in the folder `/Seroprevalence/Results`.
* `Sampling_weights.R`: Computes the sampling weights using information at baseline. This script produced the RDS file `SamplingWeights.RDS` stored in the folder `/Data/R1` (not available).
* `R1_Seroprevalence.R`: Computes the weighted seroprevalence estimates for round 1. This script produces the CSV file `r1_sp_w.csv` stored in the folder `/Seroprevalence/Results`.
* `R2_Cumulative_Seroprevalence.R`: Computes the weighted seroprevalence estimates for round 2. This script produces the CSV file `r2_cum_sp_w.csv` stored in the folder `/Seroprevalence/Results`.
* `R3_Cumulative_Seroprevalence.R`: Computes the weighted seroprevalence estimates for round 3 calibrated on the vaccintation status using different imputed datasets produced by `Imputation.R`. This script produces the CSV files `r3_cum_sp_w_seedx.csv` stored in the folder `/Seroprevalence/Results/R3` (not available).
* `R3_Cumulative_Seroprevalence_no_cal.R`: Computes the weighted seroprevalence estimates for round 3 not calibrated on the vaccination status using different imputed datasets produced by `Imputation.R`. This script produces the CSV files `r3_cum_sp_w_no_cal_seedx.csv` stored in the folder `/Seroprevalence/Results/R3` (not available).
* `R4_Cumulative_Seroprevalence.R`: Computes the weighted seroprevalence estimates for round 4 calibrated on the vaccintation status using different imputed datasets produced by `Imputation.R`. This script produces the CSV files `r4_cum_sp_w_seedx.csv` stored in the folder `/Seroprevalence/Results/R4` (not available).
* `R4_Cumulative_Seroprevalence_no_cal.R`: Computes the weighted seroprevalence estimates for round 4 not calibrated on the vaccination status using different imputed datasets produced by `Imputation.R`. This script produces the CSV files `r4_cum_sp_w_no_cal_seedx.csv` stored in the folder `/Seroprevalence/Results/R4` (not available).
* `R5_Cumulative_Seroprevalence.R`: Computes the weighted seroprevalence estimates for round 5 calibrated on the vaccintation status using different imputed datasets produced by `Imputation.R`. This script produces the CSV files `r5_cum_sp_w_seedx.csv` stored in the folder `/Seroprevalence/Results/R5` (not available).
* `R5_Cumulative_Seroprevalence_no_cal.R`: Computes the weighted seroprevalence estimates for round 5 not calibrated on the vaccination status using different imputed datasets produced by `Imputation.R`. This script produces the CSV files `r5_cum_sp_w_no_cal_seedx.csv` stored in the folder `/Seroprevalence/Results/R5` (not available).
* `Aggregate_results_w.R`: Computes the final weighted sero-estimates (sero-incidence, seroprevalence depending on the vaccination status) for the different rounds R using the CSV files `rR_cum_sp_w_seedx.csv` and `rR_cum_sp_w_no_cal_seedx.csv` stored in the folders `/Seroprevalence/Results/RR` (not available). This script produces the CSV files `rR_cum_sp_w.csv` and `rR_cum_sp_w_no_cal.csv` for the different rounds R in the folder `/Seroprevalence/Results`.
* `Plots_seroprevalence.R`: Plots the seroprevalence estimates (weighted and unweighted). This script produces the files `Figure_3.pdf` `Figure_3.png` in the folder `/Seroprevalence/Figures`.
* `my_gg.gap.R`: Modified version of the function gg.gap() to show % on the y-axis (used in `Plots_seroprevalence.R`).

## Contact

Ronan Le Gleut
