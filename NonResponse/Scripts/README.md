# Non-response analyses

Codes for the non-response analyses. 

## Structure

The folder `/Scripts` contains the follwing R scripts:

* `NR_mechanism_MI.R`: Studies the NR mechanism at the different rounds using multiple imputation of missing values in the covariates. This script produces the XLSX file `NonResponse/Results/NR_mechanism_MI.xlsx` as well as the results of multiple imputation used in the risk factor analyses (RDS file `/Data/Multiple_Imputation/Results_mi.RDS`). It also produces a plot `NonResponse/Figures/NR.png` showing the missing pattern in the data.
* `NR_mechanism_NA.R`: Studies the NR mechanism at the different rounds using complete cases for the covariates and an indicator variable for missingness for the income. This script produces the XLSX file `NonResponse/Results/NR_mechanism_NA.xlsx`.

## Contact

Ronan Le Gleut

