# Data

Data used for the statistical epidemiological analysis of the prospective Covid-19 cohort study in Munich KoCo19. For data protection reasons, the data are not publicly available.

## Structure

The folder `/R2` contains the following files:

* `Create_tables.R`: Takes as input the file `Geschlecht_Alter_neueAltersgr_20201231.xlsx` to create the file `muc_age_structure_KoCo_study_pop.csv`.
* `Geschlecht_Alter_neueAltersgr_20201231.xlsx`: Information on sex and age distribution in Munich at round 2. This file is used in the R script `Create_tables.R`.
* `KoCo19_202012.xlsx`: Information on household distribution in Munich (size, children) at round 2. Numbers in this file are used in `/Seroprevalence/Scripts/R2_Cumulative_Seroprevalence.R` to calibrate the weights.
* `muc_age_structure_KoCo_study_pop.csv`: Age and sex structure in Munich for round 2. These data are used in the script `/Seroprevalence/Scripts/R1_Seroprevalence.R`.


## Contact

Ronan Le Gleut
