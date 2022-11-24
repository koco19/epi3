# Seroprevalence analyses

Codes for the analyses to compute the seroprevalence estimates. 

## Structure

The code is is organized as follow:

* `Source.R`: Source all scripts listed below (in this order).
* `Imputation.R`: Imputation of vaccination status and serological status. This script produces the RDS files `Koco_R5latest_data_seedx.RDS` stored in the folder `/Data/Imputed_Data`.
* `Unweighted_seroprevalence.R`: Computes unweighted seroprevalence estimates. This script prduces the RDS file `sp_uw.RDS` in the folder `Seroprevalence/Results`.
* `Unweighted_seroestimates.R`: Computes unweighted sero-estimates (sero-incidence, seroprevalence depending on the vaccination status). This script prduces the RDS files `sp_vax_uw.RDS`, `si_uw.RDS` and `si_vax_uw.RDS` in the folder `Seroprevalence/Results`.

## Contact

Ronan Le Gleut
