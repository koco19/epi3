########################################################
# Program to create the table muc_age_structure_KoCo_study_pop.csv 
#
# We use the table KoCo_19_2021_11_30.xlsx as entry
########################################################


library(openxlsx)

here_koco_data = function (...) here::here("Data", ...)


###
# Age and Sex distribution at 31.08.21
###

data_age_sex <- read.xlsx(here_koco_data("R5/KoCo_19_2021_11_30.xlsx"), sheet = "Blatt 12 - Geschlecht Alter",
                     colNames = FALSE, rows = 762, cols = 8:39)


names_age_sex <- read.xlsx(here_koco_data("R5/KoCo_19_2021_11_30.xlsx"), sheet = "Blatt 12 - Geschlecht Alter",
                           colNames = FALSE, rows = 5:6, cols = 8:39,
                           fillMergedCells = TRUE)

data_age_sex <- data.frame(x = as.numeric(paste(data_age_sex[1, ])), Age = paste(names_age_sex[1, ]), 
                           Sex = paste(names_age_sex[2, ]))

# Add the number of inhabitants aged 14 to the class 15-19
data_age_sex$x[data_age_sex$Age == "15-19 Jahre" & 
                 data_age_sex$Sex %in% c("m?nnlich", "weiblich")] <- data_age_sex$x[data_age_sex$Age == "15-19 Jahre" & 
                                                                     data_age_sex$Sex %in% c("m?nnlich", "weiblich")] + c(6151, 5819)

# Define the correct age classes
data_age_sex$Group.1 <- factor(c(rep("14-19", 2), 
                                 rep(c("20-34", "35-49", "50-64", "65-79", ">=80"), each = 6)),
                               levels = c("14-19", "20-34", "35-49", "50-64", "65-79", ">=80"))
                                 
# Rename male and female
data_age_sex$Group.2 <- factor(rep(c("male", "female"), 16), levels = c("male", "female"))

# Aggregate results by Age and Sex
data_age_sex <- aggregate(data_age_sex$x, data_age_sex[, c("Group.1", "Group.2")], sum)

sum(data_age_sex$x)

# Export
write.csv(data_age_sex, here_koco_data("R5/muc_age_structure_KoCo_study_pop.csv"), row.names = FALSE)

