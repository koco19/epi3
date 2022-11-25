# Data

Data used for the statistical epidemiological analysis of the prospective Covid-19 cohort study in Munich KoCo19. For data protection reasons, the data are not publicly available.

## Structure

The folder `/R5` contains the following files:

* `Create_tables.R`: Takes as input the file `KoCo_19_2021_11_30.xlsx` to create the file `muc_age_structure_KoCo_study_pop.csv`.
* `data_for_imputation_NC.CSV`: Data used to identify which risk factors (variables) should be considered in the multiple imputation procedure (`/NonResponse/Scripts/NR_mechanism_MI.R`).
* `KoCo_19_Blatt_31_2021_08_31.xlsx`: Information on migration background of people linving in Munich for round 5. Numbers are used in `/Seroprevalence/Scripts/R5_Cumulative_Seroprevalence.R` and `/Seroprevalence/Scripts/R5_Cumulative_Seroprevalence_no_cal.R` to calibrate the weights.
* `KoCo_19_2021_11_30.xlsx`: Information on sex, age, household size, household with/without children distribution in Munich at round 5. This file is used in the R script `Create_tables.R` and numbers are used in `/Seroprevalence/Scripts/R5_Cumulative_Seroprevalence.R` and `/Seroprevalence/Scripts/R5_Cumulative_Seroprevalence_no_cal.R` to calibrate the weights.
* `muc_age_structure_KoCo_study_pop.csv`: Age and sex structure in Munich for round 4. These data are used in the script `/Seroprevalence/Scripts/R4_Cumulative_Seroprevalence.R` and `/Seroprevalence/Scripts/R4_Cumulative_Seroprevalence_no_cal.R`.
* `R5_CompleteData_NC.RDS`: Serological results of the KoCo19 cohort from round 1 to round 5. These data contain some information on the participants such as sex, age, migration background, etc. These data are used in almost all scripts (all scripts related to the study of the non-response mechanism, all scripts related to estimation of the seroprevalence in Munich)
* `Vaccination.csv`: Data on vaccination for the city of Munich (https://www.muenchen.de/rathaus/Stadtinfos/Coronavirus-Fallzahlen.html#Impfungen). These data are used in the script `/Seroprevalence/Scripts/Plots_seroprevalence.R`.

## Contact

Ronan Le Gleut
