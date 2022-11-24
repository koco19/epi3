# Seroprevalence analyses

Codes for the analyses to compute the seroprevalence estimates. 

## Structure

The folder `/Results` contains the following folders and files:

* `/R3`: Weighted seroprevalence estimates for round 3 calibrated (`r3_cum_sp_w_seedx.csv`) and not calibrated (`r3_cum_sp_w_no_cal_seedx.csv`) on the vaccintation status using different imputed datasets produced by `/Seroprevalence/Scripts/Imputation.R`. These CSV files, obtained from `R3_Cumulative_Seroprevalence.R` and `R3_Cumulative_Seroprevalence_no_cal.R`, are not available. 
* `/R4`: Weighted seroprevalence estimates for round 4 calibrated (`r4_cum_sp_w_seedx.csv`) and not calibrated (`r4_cum_sp_w_no_cal_seedx.csv`) on the vaccintation status using different imputed datasets produced by `/Seroprevalence/Scripts/Imputation.R`. These CSV files, obtained from `R4_Cumulative_Seroprevalence.R` and `R4_Cumulative_Seroprevalence_no_cal.R`, are not available. 
* `/R5`: Weighted seroprevalence estimates for round 5 calibrated (`r5_cum_sp_w_seedx.csv`) and not calibrated (`r5_cum_sp_w_no_cal_seedx.csv`) on the vaccintation status using different imputed datasets produced by `/Seroprevalence/Scripts/Imputation.R`. These CSV files, obtained from `R5_Cumulative_Seroprevalence.R` and `R5_Cumulative_Seroprevalence_no_cal.R`, are not available. 
* `/Unweighted`: Unweighted sero-estimates (sero-incidence, seroprevalence depending on the vaccination status) for all rounds using different imputed datasets produced by `/Seroprevalence/Scripts/Imputation.R`. These RDS files are not available. 
* `r1_sp_w.csv`: Weighted seroprevalence estimates for round 1. This file is obtained from `/Seroprevalence/Scripts/R1_Seroprevalence.R`. 
* `r2_cum_sp_w.csv`: Weighted seroprevalence estimates for round 2. This file is obtained from `/Seroprevalence/Scripts/R2_Cumulative_Seroprevalence.R`. 
* `r3_cum_sp_w.csv`: Weighted seroprevalence estimates for round 3 calibrated on the vaccination status. This file is obtained from `/Seroprevalence/Scripts/Aggregate_results_w.R` and the CVS files (`/Seroprevalence/Results/R3/r3_cum_sp_w_seedx.csv`). 
* `r3_cum_sp_w_no_cal.csv`: Weighted seroprevalence estimates for round 3 not calibrated on the vaccination status. This file is obtained from `/Seroprevalence/Scripts/Aggregate_results_w.R` and the CVS files (`/Seroprevalence/Results/R3/r3_cum_sp_w_no_cal_seedx.csv`). 
* `r4_cum_sp_w.csv`: Weighted seroprevalence estimates for round 4 calibrated on the vaccination status. This file is obtained from `/Seroprevalence/Scripts/Aggregate_results_w.R` and the CVS files (`/Seroprevalence/Results/R4/r4_cum_sp_w_seedx.csv`). 
* `r4_cum_sp_w_no_cal.csv`: Weighted seroprevalence estimates for round 4 not calibrated on the vaccination status. This file is obtained from `/Seroprevalence/Scripts/Aggregate_results_w.R` and the CVS files (`/Seroprevalence/Results/R4/r4_cum_sp_w_no_cal_seedx.csv`). 
* `r5_cum_sp_w.csv`: Weighted seroprevalence estimates for round 5 calibrated on the vaccination status. This file is obtained from `/Seroprevalence/Scripts/Aggregate_results_w.R` and the CVS files (`/Seroprevalence/Results/R5/r5_cum_sp_w_seedx.csv`). 
* `r5_cum_sp_w_no_cal.csv`: Weighted seroprevalence estimates for round 5 not calibrated on the vaccination status. This file is obtained from `/Seroprevalence/Scripts/Aggregate_results_w.R` and the CVS files (`/Seroprevalence/Results/R5/r5_cum_sp_w_no_cal_seedx.csv`). 
* `si_uw.RDS`: Unweighted sero-incidence for the different rounds. This file is obtained from `/Seroprevalence/Scripts/Aggregate_results_uw.R` and the RDS files (`/Seroprevalence/Results/Unweighted/si_uw_seedx.RDS`). 
* `si_vax_uw.RDS`: Unweighted sero-incidence split according to the vaccination status for the different rounds. This file is obtained from `/Seroprevalence/Scripts/Aggregate_results_uw.R` and the RDS files (`/Seroprevalence/Results/Unweighted/si_vax_uw_seedx.RDS`). 
* `sp_uw.RDS`: Unweighted seroprevalence for the different rounds. This file is obtained from `/Seroprevalence/Scripts/Unweighted_seroprevalence.R`. 
* `sp_vax_uw.RDS`: Unweighted seroprevalence split according to the vaccination status for the different rounds. This file is obtained from `/Seroprevalence/Scripts/Aggregate_results_uw.R` and the RDS files (`/Seroprevalence/Results/Unweighted/sp_vax_uw_seedx.RDS`). 


## Contact

Ronan Le Gleut
