#' This script calibrates the sampling weights (from the script Sampling_weights.R)
#' for the first round of visit and calculates the variance associated with the estimation of the proportion of ppl.
#' infected by the virus (positive tested).

here_koco_data = function (...) here::here("Data", ...)
here_koco_prev = function (...) here::here("Seroprevalence", ...)
here_prev_figures = function (...) here_koco_prev("Figures", ...)
here_prev_results = function (...) here_koco_prev("Results", ...)
here_prev_scripts = function (...) here_koco_prev("Scripts", ...)

#############################
# Load data sets
#############################

###
# Sampling Weights
###


KoCo_BLab <- readRDS(here_koco_data("R1/SamplingWeights.RDS"))



###
# Complete sample of KoCo19 (with missing values)
###

# We need this data set to identify households with or without children
KoCo19 <- read.csv(here_koco_data("R1/ind_lab_baseline_new.csv"), stringsAsFactors = F)


# We have 134 missing ages in this data set. We consider that:
# - in households with 2 members, the missing age is another adult
# - in households with 3 or more members, the missing age(s) is/are a child(ren)


# In households with missing ages and with more than 2 members, we impute the missing ages with
# the value 10. It does not matter here, we will not use directly this value. We just want to say
# that these missing values are children.
KoCo19$Age[is.na(KoCo19$Age) & KoCo19$obs_hh_members > 2] <- 10


###
# Data set with the constituencies
###

# reading the data set with with the consituencies info for each household
Const <- read.csv(here_koco_data("R1/KoCo19_Haushalte4Modeler_wRRstartConstituency_20200910.csv"), stringsAsFactors = TRUE)
# 3007 hh

# Remove some hh that are not in the final study population
Const <- Const[Const$hht_ID %in% KoCo_BLab$hh_id, ]
# 2994 hh

# Recoding of the IDs of the starting constituencies
Const$const_start <- paste0(substr(Const$constituency_RRstart, 1, 2), "0",
                            substr(Const$constituency_RRstart, 3, 4))

Const$const_start[Const$constituency_RRstart < 1000] <- 
  paste0("0", substr(Const$constituency_RRstart[Const$constituency_RRstart < 1000], 1, 1),
         "0", substr(Const$constituency_RRstart[Const$constituency_RRstart < 1000], 2, 3))

# Keep only the variables of interest
Const <- Const[, c("hht_ID", "const_start")]


###
# Data sets from the Statistisches Amt
###


### Sex/Age distribution

margins_age_sex <- read.csv(here_koco_data("R1/muc_age_structure_KoCo_study_pop.csv"), stringsAsFactors = F)

# Rename variables to match the included information 
names(margins_age_sex)[names(margins_age_sex)=="Group.1"] <- "age_cat"
names(margins_age_sex)[names(margins_age_sex)=="Group.2"] <- "Sex"
names(margins_age_sex)[names(margins_age_sex)=="x"] <- "Freq"

# Changing the first letters of sex to match the information in the study data set
margins_age_sex$Sex[margins_age_sex$Sex=="male"] <- "Male"
margins_age_sex$Sex[margins_age_sex$Sex=="female"] <- "Female"

# Creating a variable including the information about the sex/age categories
margins_age_sex$sex_age <- paste(margins_age_sex$Sex, margins_age_sex$age_cat,
                                 sep = "_")

### Number of households per constituency

# Information about number of hh in Munich depending on constituency based on stat.Amt
Munich_hh <- read.csv(here_koco_data("R1/muc_hh_structure_KoCo_study_pop.csv"), stringsAsFactors = TRUE)

# Recoding of the IDs of the constituencies
Munich_hh$const <- as.character(Munich_hh$Const_ID)

Munich_hh$const[Munich_hh$Const_ID < 10000] <- paste0("0", Munich_hh$const[Munich_hh$Const_ID < 10000])

# Keep only the variables of interest
Munich_hh <- Munich_hh[, c("const", "Nb_hh")]

###
# Add individual information
###

info_ind <- readRDS(here_koco_data("R5/R5_CompleteData_NC.RDS"))

KoCo_BLab <- merge(KoCo_BLab, info_ind[, c("ind_id", "Q6_f1_Geburtsdatum_DDMONYYYY", "date_R1",
                                           "Sex", "Age", "Birth_Country", "R1_Result")])

KoCo_BLab$Age2 <- trunc((as.Date(KoCo_BLab$Q6_f1_Geburtsdatum_DDMONYYYY) %--% as.Date(KoCo_BLab$date_R1))/years(1))
KoCo_BLab$Age2[is.na(KoCo_BLab$Age2)] <- KoCo_BLab$Age[is.na(KoCo_BLab$Age2)]

anyNA(KoCo_BLab$Age2)

KoCo_BLab$Age <- KoCo_BLab$Age2

KoCo_BLab <- KoCo_BLab[setdiff(names(KoCo_BLab), c("Q6_f1_Geburtsdatum_DDMONYYYY", "date_R1", "Age2"))]


#############################
# Collecting auxiliary data for calibration (definition of the margins)
#############################

###
# Age/Sex structure and country of birth in Munich (based on KoCo19, stat. Amt)
###

# Selecting the margins for sex/age categories 
totals <- margins_age_sex$Freq
names(totals) <- margins_age_sex$sex_age

# Selecting the margins for country of origin 
# Numbers can be found in Data/R1/KoCo_19_Blatt_31_2020_05_31.xlsx
country_other <- 428705
country_germany_unknow <- sum(totals) - country_other

###
# Nb of single/multi-ppl hh and hh with/without children in Munich (based on KoCo19, stat. Amt)
###

# Selecting the margins for household size (1 vs. 2 vs. multi) 
# Main residences + Main and secondary residences
# Numbers can be found in Data/R1/KoCo_2020_05_31.xlsx
n_1_house <- 451254
n_2_house <- 203243+2924
n_multi_house <- 828480+5796 - n_1_house - n_2_house

# Selecting the margins for household with or without children in Munich
# Numbers can be found in Data/R1/KoCo_2020_05_31.xlsx
n_house_0_child <- 683160+4476
n_house_1plus_child <- 828480+5796 - n_house_0_child


###
# Summarizing the information about Munich
###
totals <- c(totals, country_germany_unknow = country_germany_unknow, country_other = country_other,
            n_1_house = n_1_house, n_2_house = n_2_house, n_multi_house = n_multi_house,
            n_house_0_child = n_house_0_child, n_house_1plus_child = n_house_1plus_child)

# Removing temporary data
rm(margins_age_sex, country_germany_unknow, country_other, n_1_house, n_2_house, n_multi_house, n_house_0_child, n_house_1plus_child)


#############################
# Create in the sample the variables needed for the calibration
#############################

###
# Age/Sex structure, country of origin 
###

# Creating age categories matching the information about Munich 
KoCo_BLab$age_cat <- cut(KoCo_BLab$Age, c(0, 19, 34, 49, 64, 79, 150),
                            include.lowest = TRUE,
                            right = TRUE,
                            labels = c("<=19", "20-34", "35-49", "50-64",
                                       "65-79", ">=80"))

# Creating a variable including the information about the sex/age categories
KoCo_BLab$sex_age <- paste(KoCo_BLab$Sex, KoCo_BLab$age_cat, sep = "_")

# Checking the nb of ppl in the sex/age categories
table(KoCo_BLab$sex_age, useNA = "always")

# Creating a variable including the information about hh_id and sex/age categories
KoCo_BLab$hh_sex_age <- paste(KoCo_BLab$hh_id, KoCo_BLab$sex_age, sep = "_")

table(KoCo_BLab$Birth_Country, useNA = "always")

# Creating a variable including the information about the country of origin
KoCo_BLab$Birth_Country[is.na(KoCo_BLab$Birth_Country)] <- "Germany"

# Checking the nb of ppl by country of origin
table(KoCo_BLab$Birth_Country, useNA = "always")

# Creating a variable including the information about hh_id and country categories
KoCo_BLab$hh_country <- paste(KoCo_BLab$hh_id, KoCo_BLab$Birth_Country, sep = "_")


###
# Roche
###

### Sex/Age

# Calculating the number of males, females in each age group in each household
n_sex_age <- freq(KoCo_BLab$hh_sex_age, w = KoCo_BLab$w_ind, plot=F)

# Removing the line for totals
n_sex_age <- n_sex_age[-nrow(n_sex_age), ]

# Splitting the information saved in the variable about hh_id and sex/age categories
n_sex_age <- data.frame(hh_id = substr(rownames(n_sex_age), 1, 7),
                              sex_age = substr(rownames(n_sex_age), 9, 30),
                              freq = n_sex_age[, "Frequency"])

# Reshaping the dataframe to have for each hh the number of ppl based on sex and age
data_house <- reshape(n_sex_age, v.names = "freq",
                      timevar = "sex_age", idvar = "hh_id", direction = "wide")


### Country of origin

# Calculating the number Germans/other in each household
n_country <- freq(KoCo_BLab$hh_country, w = KoCo_BLab$w_ind, plot=F)

# Removing the line for totals
n_country <- n_country[-nrow(n_country), ]

# Splitting the information saved in the variable about hh_id and country of origin
n_country <- data.frame(hh_id = substr(rownames(n_country), 1, 7),
                              country = substr(rownames(n_country), 9, 15),
                              freq = n_country[, "Frequency"])


# Reshaping the dataframe to have for each hh the number of ppl based on contry of origin
data_house2 <- reshape(n_country, v.names = "freq",
                       timevar = "country", idvar = "hh_id", direction = "wide")

# Merging of the two information
data_house <- merge(data_house, data_house2, by = "hh_id")

# Setting empty cells to 0
data_house[is.na(data_house)] <- 0

# Renaming columns and rows 
colnames(data_house) <- gsub(x = colnames(data_house), pattern = "freq.", replacement = "")
rownames(data_house) <- NULL


# Reordering the columns
data_house <- data_house[, c("hh_id", "Male_<=19", "Male_20-34", "Male_35-49", "Male_50-64", "Male_65-79", "Male_>=80",
                             "Female_<=19", "Female_20-34", "Female_35-49", "Female_50-64", "Female_65-79", "Female_>=80",
                             "Germany", "Other")]



###
# Nb of members in the hh, children/no children in the hh 
###

# Adding information about number of observed individuals in hh
data_house <- merge(data_house, unique(KoCo19[, c("hh_id", "obs_hh_members")]), by="hh_id", all.x = T)


# Adding information about type of housing (1 vs. 2 vs. multi)
data_house$house <- cut(data_house$obs_hh_members, breaks = c(0, 1, 2, 100),
                        labels = c("n_1", "n_2", "multi"), ordered_result = TRUE)

# Creating dummy variable(s) for type of housing
data_house <- cbind(data_house, model.matrix(~ house - 1, data = data_house))


# Adding information about children in the household (children vs. no children)
# Nb of children in each household
nb_child <- tapply(KoCo19$Age < 18, KoCo19$hh_id, function(x) sum(x, na.rm = T))
nb_child <- data.frame(hh_id = names(nb_child), nb_child = nb_child)

# Dummy variable: Child vs. no child
nb_child$child <- ifelse(nb_child$nb_child == 0, "no", "yes")

data_house <- merge(data_house, nb_child, by="hh_id", all.x = T)

# Creating dummy variable(s) for Children
data_house <- cbind(data_house, model.matrix(~ child - 1, data = data_house))


# Adding rownames
rownames(data_house) <- data_house$hh_id

# Removing unneccessary variables
data_house[, c("hh_id", "house", "obs_hh_members", "nb_child", "child")] <- NULL


# removing temporary data
rm(n_sex_age, n_country, KoCo19, data_house2, nb_child)


#############################
# Calibration Roche
#############################

# Checking if the order of age/sex categories are the same
colnames(data_house)
names(totals)

# Sampling weights at the household level
d_house <- KoCo_BLab$w_constituency * KoCo_BLab$w_household
names(d_house) <- KoCo_BLab$hh_id
d_house <- d_house[!duplicated(names(d_house))]

# Ordering weights in the same order than the household data
d_house <- d_house[rownames(data_house)]

###
# Calculating the calibrated weights by means of different calibration methods.
###



### method "logit"
g_logit <- calib(data_house, d = d_house, totals, method="logit",
           bounds=c(0.3,2.5), max_iter = 2000)  
w_hh_cal_logit <- g_logit * d_house

# Checking if calibrated weights sum up to auxiliary data
sum(t(w_hh_cal_logit) %*% as.matrix(data_house) - totals[])
summary(w_hh_cal_logit)
plot(density(g_logit))
# Ok

# Save calibrated weights at the household level
calib_weights <- data.frame(hh_id = names(d_house), w_hh_cal = w_hh_cal_logit,
                            g = g_logit)

KoCo_BLab <- merge(KoCo_BLab, calib_weights, by = "hh_id")

# Compute calibrated weights at the individual level
KoCo_BLab$w_ind_cal <- KoCo_BLab$w_hh_cal * KoCo_BLab$w_ind



#############################
# Calibrated estimators
#############################

# Estimation of the proportion with/without the sampling/calibrated weights

freq(x = KoCo_BLab$R1_Result, plot=F)

freq(x = KoCo_BLab$R1_Result, w = KoCo_BLab$w_ind_cal, plot=F)


### Removing temporary data
rm(list = setdiff(ls(), c("KoCo_BLab", "data_house", "Const", "Munich_hh", "d_house",
                          "here_koco_data", "here_koco_prev", "here_prev_results", "here_prev_scripts")))


#############################
# Variance of the calibrated estimator
#############################


###
# calculate the linearized variable for a proportion
###

### Linearization at the individual level

KoCo_BLab$results <- ifelse(KoCo_BLab$R1_Result == "Positive", 1, 0)


# Weighted proportion
p <- sum(KoCo_BLab$results * KoCo_BLab$w_ind_cal)/sum(KoCo_BLab$w_ind_cal)

# Linearized variables
KoCo_BLab$u_k_0 <- (KoCo_BLab$results - p)/sum(KoCo_BLab$w_ind_cal)


### Linearization at the household level

split_hh <- split(KoCo_BLab[, c("hh_id", "w_ind", "u_k_0")], KoCo_BLab$hh_id)

hh_u_k <- sapply(split_hh, function(z){
  sum(z[, "u_k_0"] * z[, "w_ind"])
})


hh_u_k <- data.frame(hh_id = names(hh_u_k), hh_u_k = hh_u_k)


###
# Create a data frame at the hh level with the linearized variable,
# the auxiliary information, the calibrated weights and the ratio
# of the calibrated weights and the sampling weights g
###

data_house$hh_id <- rownames(data_house)

data_house <- merge(data_house, KoCo_BLab[!duplicated(KoCo_BLab$hh_id),
                                          c("hh_id", "w_hh_noshare", "w_household", "w_hh_cal", "g")],
                    by = "hh_id")


data_house <- merge(data_house, hh_u_k, by = "hh_id")

identical(data_house$hh_id, names(d_house))

###
# Run a linear model with the linearized variable as response variable and the
# auxiliary information as covariates including the sampling weights
###

res.lm <- lm(as.formula(paste("hh_u_k", paste(paste0("`", names(data_house[, 2:20]), "`"), collapse=" + "), sep=" ~ ")), 
             weights = d_house,
             data = data_house)

###############
# Variance estimation
###############

# For the variance estimation, we will use the residuals e_k


data_res <- cbind(data_house, res = res.lm$residuals)


# Add the constituency ID
data_res <- merge(data_res, Const, by.x = "hh_id", by.y = "hht_ID")

########
### First term of the variance estimation
########

# Total number of constituencies
N_const <- length(unique(Munich_hh$const))

# number of constituencies surveyed
n_const <- length(unique(Const$const_start))


# Calculate the estimated totals of ge at the constituency level
split_const <- split(data_res, data_res$const_start)

tot_e_const <- sapply(split_const, function(z){
  sum(z[, "res"] * z[, "w_household"])
})


# Calculate the dispersion of res
s_e <- var(tot_e_const)


# First term of the variance estimation
V1 <- N_const^2 * (1/n_const - 1/N_const) * s_e


########
### Second term of the variance estimation
########

# In each constituency, variance of the error terms ge
s_e_const <- tapply(data_res$res, data_res$const_start, var)

# Add the number of households surveyed and in the population for each constituency
s_e_const <- data.frame(hh_id = names(s_e_const), s_e = s_e_const)

# For constituencies with freq==1, tapply(, , var) is giving NA, therefore we replace NA by 0
s_e_const[is.na(s_e_const$s_e)==T, "s_e"] <- 0

# Nb hh surveyed
nb_hh_s <- as.data.frame(table(Const$const_start))

# Merge all the information
s_e_const <- merge(s_e_const, nb_hh_s, by.x = "hh_id", by.y = "Var1")
s_e_const <- merge(s_e_const, Munich_hh, by.x = "hh_id", by.y = "const", all.x = TRUE)

# Second term of the variance estimation
V2 <- N_const / n_const * sum(s_e_const$Nb_hh^2 * (1/s_e_const$Freq - 1/s_e_const$Nb_hh) * s_e_const$s_e)


###
# Variance estimation and confidence intervals
###


w_p <- freq(KoCo_BLab$R1_Result, w = KoCo_BLab$w_ind_cal, plot = F)[2, 2]
  

V <- V1 + V2

# Upper bound CI
w_ub <- w_p + qnorm(0.975) * sqrt(V) * 100

# Lower bound CI
w_lb <- w_p - qnorm(0.975) * sqrt(V) * 100

res_w <- c(estimate = w_p, lb_ci = w_lb, ub_ci = w_ub)


###
# Adjust for specificity and sensitivity
###

spec_sens_classifier <- readRDS(file = here_koco_data("R1/specificity_sensitivity_classifier.RData"))

res_adj <- (res_w/100 + spec_sens_classifier["Roche N pan-Ig optimized cut-off", "Specificity"] - 1)/(spec_sens_classifier["Roche N pan-Ig optimized cut-off", "Sensitivity"] + spec_sens_classifier["Roche N pan-Ig optimized cut-off", "Specificity"] - 1)*100


###
# Save results
###

res_all <- res_all <- data.frame(Adjust = c("unadjusted", "adjusted"),
                                 Calculation = "All",
                                 rbind(res_w, res_adj))


write.csv(res_all, here_prev_results("r1_sp_w.csv"), row.names = FALSE)
