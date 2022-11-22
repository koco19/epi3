########################################################
# Program to create the tables 
#  - muc_age_structure_KoCo_study_pop.csv 
#  - muc_hh_structure_KoCo_study_pop.csv
#
# For this purpose, we use the following tables as entry
#  - 200821_KoCo19.xlsx
#  - KoCo_2020_05_31.xlsx
########################################################


library(openxlsx)

here_koco_data = function (...) here::here("Data", ...)


###
# Number of private hauseholds at 31.03.20 per constituency
###

data_hh <- read.xlsx(here_koco_data("R1/200821_KoCo19.xlsx"), sheet = "Blatt 4 - HH Personen",
                     colNames = FALSE, startRow = 3, cols = c(1, 9, 23))

# Principal residences only (HWS) and 
# principal residences + secondary residences at the same time (HWS-NWS)
colnames(data_hh) <- c("Const_ID", "HWS", "HWS_NWS")

# Sum them up
data_hh$Nb_hh <- data_hh$HWS + data_hh$HWS_NWS

# Remove last row (total of all constituencies) and keep only Nb_hh
data_hh <- data_hh[-nrow(data_hh), c("Const_ID", "Nb_hh")]

# Export
write.csv(data_hh, here_koco_data("R1/muc_hh_structure_KoCo_study_pop.csv"), row.names = FALSE)

###
# Age and Sex distribution at 31.05.20
###

data_age_sex <- read.xlsx(here_koco_data("R1/KoCo_2020_05_31.xlsx"), sheet = "Blatt 12 - Geschlecht Alter",
                     colNames = FALSE, rows = 762, cols = 8:39)


names_age_sex <- read.xlsx(here_koco_data("R1/KoCo_2020_05_31.xlsx"), sheet = "Blatt 12 - Geschlecht Alter",
                           colNames = FALSE, rows = 5:6, cols = 8:39,
                           fillMergedCells = TRUE)

data_age_sex <- data.frame(x = as.numeric(paste(data_age_sex[1, ])), Age = paste(names_age_sex[1, ]), 
                           Sex = paste(names_age_sex[2, ]))

# Add the number of inhabitants aged 14 to the class 15-19
data_age_sex$x[data_age_sex$Age == "15-19 Jahre" & 
                 data_age_sex$Sex %in% c("m?nnlich", "weiblich")] <- data_age_sex$x[data_age_sex$Age == "15-19 Jahre" & 
                                                                     data_age_sex$Sex %in% c("m?nnlich", "weiblich")] + c(5867, 5635)

# Define the correct age classes
data_age_sex$Group.1 <- factor(c(rep("14-19", 2), 
                                 rep(c("20-34", "35-49", "50-64", "65-79", ">=80"), each = 6)),
                               levels = c("14-19", "20-34", "35-49", "50-64", "65-79", ">=80"))
                                 
# Rename male and female
data_age_sex$Group.2 <- factor(rep(c("male", "female"), 16), levels = c("male", "female"))

# Aggregate results by Age and Sex
data_age_sex <- aggregate(data_age_sex$x, data_age_sex[, c("Group.1", "Group.2")], sum)

# Export
write.csv(data_age_sex, here_koco_data("R1/muc_age_structure_KoCo_study_pop.csv"), row.names = FALSE)

