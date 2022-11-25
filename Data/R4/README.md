# Data

Data used for the statistical epidemiological analysis of the prospective Covid-19 cohort study in Munich KoCo19. For data protection reasons, the data are not publicly available.

## Structure

The folder `/R4` contains the following files:

* `Create_tables.R`: Takes as input the file `KoCo_2021_08_31.xlsx` to create the file `muc_age_structure_KoCo_study_pop.csv`.
* `KoCo_19_Blatt_31_2021_08_31.xlsx`: Information on migration background of people linving in Munich for round 4. Numbers are used in `/Seroprevalence/Scripts/R4_Cumulative_Seroprevalence.R` and `/Seroprevalence/Scripts/R4_Cumulative_Seroprevalence_no_cal.R` to calibrate the weights.
* `KoCo_2021_08_31.xlsx`: Information on sex, age, household size, household with/without children distribution in Munich at round 4. This file is used in the R script `Create_tables.R` and numbers are used in `/Seroprevalence/Scripts/R4_Cumulative_Seroprevalence.R` and `/Seroprevalence/Scripts/R4_Cumulative_Seroprevalence_no_cal.R` to calibrate the weights.
* `muc_age_structure_KoCo_study_pop.csv`: Age and sex structure in Munich for round 4. These data are used in the script `/Seroprevalence/Scripts/R4_Cumulative_Seroprevalence.R` and `/Seroprevalence/Scripts/R4_Cumulative_Seroprevalence_no_cal.R`.


## Contact

Ronan Le Gleut

