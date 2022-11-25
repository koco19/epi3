# Data

Data used for the statistical epidemiological analysis of the prospective Covid-19 cohort study in Munich KoCo19. For data protection reasons, the data are not publicly available.

## Structure

The folder `/R1` contains the following files:

* `200821_KoCo19.xlsx`: Data on the Munich structure (number of private hauseholds at 31.03.20 per constituency). These data are used in the script `/Data/R1/Create_tables.R`.
* `contiguity_matrices-munich-constituencies_200914.RData`: Contiguity matrices of the constituencies. These data are used in the script `/Seroprevalence/Scripts/Sampling_weights.R`.
* `Create_tables.R`: Takes as input the files `200821_KoCo19.xlsx` and `KoCo_2020_05_31.xlsx` to create the files `muc_age_structure_KoCo_study_pop.csv` and `muc_hh_structure_KoCo_study_pop.csv`.
* `hh_characteristics_new.csv`: Information on households in the cohort at baseline. These data are used to correct for the attrition observed in the cohort from round 2 up to round 5 in the calculation of the weights (`/Seroprevalence/Scripts/RR_Cumulative_Seroprevalence.R` and `/Seroprevalence/Scripts/RR_Cumulative_Seroprevalence_no_cal.R`) and to study the NR mechanism (`/NonResponse/Scripts/NR_mechanism_MI.R` and `/NonResponse/Scripts/NR_mechanism_NA.R`).
* `ind_characteristics_new.csv`: Information on individuals in the cohort at baseline. These data are used to study the NR mechanism (`/NonResponse/Scripts/NR_mechanism_MI.R` and `/NonResponse/Scripts/NR_mechanism_NA.R`).
* `ind_lab_baseline_new.csv`: Information on individuals in the cohort at baseline. These data are used to compute the sampling weights (`/Seroprevalence/Scripts/SamplingWeights.R`) and to identify households with children and to correct for the attrition observed in the cohort from round 2 up to round 5 in the calculation of the weights (`/Seroprevalence/Scripts/RR_Cumulative_Seroprevalence.R` and `/Seroprevalence/Scripts/RR_Cumulative_Seroprevalence_no_cal.R`).
* `KoCo_19_Blatt_31_2020_05_31.xlsx`: Munich structure on migration background for round 1. The numbers in this files are used in the script `/Seroprevalence/Scripts/R1_Seroprevalence.R`.
* `KoCo_2020_05_31.xlsx`: Munich structure regarding age, sex, household size, etc. distribution for the calibration of the weights. These data are used in the script `Create_tables.R`.
* `KoCo19_Haushalte4Modeler_wRRstartConstituency_20200910.csv`:  For each random route, starting and ending constituencies. These data are used in all scripts to compute the sampling weights (`/Seroprevalence/Scripts/SamplingWeights.R`) and calibrated weights at each round R (`/Seroprevalence/Scripts/R1_Seroprevalence.R`, `/Seroprevalence/Scripts/RR_Cumulative_Seroprevalence.R` and `/Seroprevalence/Scripts/RR_Cumulative_Seroprevalence_no_cal.R`).
* `muc_age_structure_KoCo_study_pop.csv`: Age and sex structure in Munich for round 1. These data are used in the script `/Seroprevalence/Scripts/R1_Seroprevalence.R`.
* `muc_hh_structure_KoCo_study_pop.csv`: Number of households per constituencies at baseline. These data are used in all scripts to compute the sampling weights (`/Seroprevalence/Scripts/SamplingWeights.R`) and calibrated weights at each round R (`/Seroprevalence/Scripts/R1_Seroprevalence.R`, `/Seroprevalence/Scripts/RR_Cumulative_Seroprevalence.R` and `/Seroprevalence/Scripts/RR_Cumulative_Seroprevalence_no_cal.R`).
* `SamplingWeights.RDS`: Sampling weights of each participant and each household. These data are obtained from the script `/Seroprevalence/ScriptsSampling_weights.R` and are used in all scripts to compute the calibrated weights at each round R (`/Seroprevalence/Scripts/R1_Seroprevalence.R`, `/Seroprevalence/Scripts/RR_Cumulative_Seroprevalence.R` and `/Seroprevalence/Scripts/RR_Cumulative_Seroprevalence_no_cal.R`).
* `specificity_sensitivity_classifier.RData`: Sensitivity and specificity for Roche test. These data are used in all scripts to compute the calibrated weights at each round R (`/Seroprevalence/Scripts/R1_Seroprevalence.R`, `/Seroprevalence/Scripts/RR_Cumulative_Seroprevalence.R` and `/Seroprevalence/Scripts/RR_Cumulative_Seroprevalence_no_cal.R`).


## Contact

Ronan Le Gleut
