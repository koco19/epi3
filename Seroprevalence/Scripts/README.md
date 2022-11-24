# Seroprevalence analyses

Codes for the analyses to compute the seroprevalence estimates. 

## Structure

The code is is organized as follow:

* `Source.R`: Source all scripts listed below (in this order).
* `Imputation.R`: Imputation of vaccination status and serological status. This script produces the RDS files `Koco_R5latest_data_seedx.RDS` stored in the folder `/Data/Imputed_Data`.
* `Unweighted_seroprevalence.R`: Computes unweighted seroprevalence estimates. This script prduces the RDS file `sp_uw.RDS` in the folder `Seroprevalence/Results`.
* `Unweighted_seroestimates.R`: Computes unweighted sero-estimates (sero-incidence, seroprevalence depending on the vaccination status) using different imputed datasets produced by `Imputation.R`. This script prduces the RDS files `sp_vax_uw_seedx.RDS`, `si_uw_seedx.RDS` and `si_vax_uw_seedx.RDS` stored in the folder `Seroprevalence/Results/Unweighted`.
* `Aggregate_results_uw.R`: Computes the final unweighted sero-estimates (sero-incidence, seroprevalence depending on the vaccination status). This script prduces the RDS files `sp_vax_uw.RDS`, `si_uw.RDS` and `si_vax_uw.RDS` in the folder `Seroprevalence/Results`.
* `Sampling_weights.R`: Computes the sampling weights using information at baseline. This script produced the RDS file `SamplingWeights.RDS` stored in the folder `/Data/R1`.
* `R1_Seroprevalence.R`: Computes the weighted seroprevalence estimates for round 1. This script produces the CSV file `r1_sp_w.csv` stored in the folder `Seroprevalence/Results`.
* `R2_Cumulative_Seroprevalence.R`: Computes the weighted seroprevalence estimates for round 2. This script produces the CSV file `r2_cum_sp_w.csv` stored in the folder `Seroprevalence/Results`.
* `R3_Cumulative_Seroprevalence.R`: Computes the weighted seroprevalence estimates for round 3 calibrated on the vaccintation status using different imputed datasets produced by `Imputation.R`. This script produces the CSV files `r3_cum_sp_w_seedx.csv` stored in the folder `Seroprevalence/Results/R3`.
* `R3_Cumulative_Seroprevalence_no_cal.R`: Computes the weighted seroprevalence estimates for round 3 not calibrated on the vaccination status using different imputed datasets produced by `Imputation.R`. This script produces the CSV files `r3_cum_sp_w_no_cal_seedx.csv` stored in the folder `Seroprevalence/Results/R3`.

## Contact

Ronan Le Gleut
