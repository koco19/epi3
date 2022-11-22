########################################################
# Program to create the tables muc_age_structure_KoCo_study_pop.csv 
# 
# We use the table Geschlecht_Alter_neueAltersgr_20201231.xlsx as entry
########################################################


library(openxlsx)

here_koco_data = function (...) here::here("Data", ...)


###
# Age and Sex distribution at 31.12.20
###

data_age_sex <- read.xlsx(here_koco_data("R2/Geschlecht_Alter_neueAltersgr_20201231.xlsx"),
                     colNames = FALSE, rows = 761, cols = 8:39)


names_age_sex <- read.xlsx(here_koco_data("R2/Geschlecht_Alter_neueAltersgr_20201231.xlsx"), 
                           colNames = FALSE, rows = 4:5, cols = 8:39,
                           fillMergedCells = TRUE)

data_age_sex <- data.frame(x = as.numeric(paste(data_age_sex[1, ])), Age = paste(names_age_sex[1, ]), 
                           Sex = paste(names_age_sex[2, ]))

# Define the correct age classes
data_age_sex$Group.1 <- factor(c(rep("14-19", 2), 
                                 rep(c("20-34", "35-49", "50-64", "65-79", ">=80"), each = 6)),
                               levels = c("14-19", "20-34", "35-49", "50-64", "65-79", ">=80"))
                                 
# Rename male and female
data_age_sex$Group.2 <- factor(rep(c("male", "female"), 16), levels = c("male", "female"))

# Aggregate results by Age and Sex
data_age_sex <- aggregate(data_age_sex$x, data_age_sex[, c("Group.1", "Group.2")], sum)

# Export
write.csv(data_age_sex, here_koco_data("R2/muc_age_structure_KoCo_study_pop.csv"), row.names = FALSE)

