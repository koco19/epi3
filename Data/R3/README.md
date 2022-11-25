# Data

Data used for the statistical epidemiological analysis of the prospective Covid-19 cohort study in Munich KoCo19. For data protection reasons, the data are not publicly available.

## Structure

The folder `/R3` contains the following files:

* `Create_tables.R`: Takes as input the file `KoCo19_202103-3.xlsx` to create the file `muc_age_structure_KoCo_study_pop.csv`.
* `KoCo19_202003_202012_202103_Blatt31.xlsx`: Information on migration background of people linving in Munich for rounds 2. Numbers are used in `/Seroprevalence/Scripts/RR_Cumulative_Seroprevalence.R` and `/Seroprevalence/Scripts/RR_Cumulative_Seroprevalence_no_cal.R` for rounds 2 and 3 to calibrate the weights.
* `KoCo19_202103-3.xlsx`: Information on sex, age, household size, household with/without children distribution in Munich at round 3. This file is used in the R script `Create_tables.R` and numbers are used in `/Seroprevalence/Scripts/R3_Cumulative_Seroprevalence.R` and `/Seroprevalence/Scripts/R3_Cumulative_Seroprevalence_no_cal.R` to calibrate the weights.
* `muc_age_structure_KoCo_study_pop.csv`: Age and sex structure in Munich for round 3. These data are used in the script `/Seroprevalence/Scripts/R3_Cumulative_Seroprevalence.R` and `/Seroprevalence/Scripts/R3_Cumulative_Seroprevalence_no_cal.R`.


## Contact

Ronan Le Gleut
