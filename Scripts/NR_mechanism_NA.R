#' This script studies the NR mechanism at the different rounds using 
#' keeping the missing values for the variable income as NAs.

rm(list = ls())


here_koco_data = function (...) here::here("Data", ...)
here_koco_results = function (...) here::here("Results", ...)


########
# Data #
########


# Collect information on respondents and non-respondents at the household level
info_hh <- read.csv(here_koco_data("R1/hh_characteristics_new.csv"), stringsAsFactors = TRUE)

# Collect information on respondents and non-respondents at the individual level
info_ind <- read.csv(here_koco_data("R1/ind_characteristics_new.csv"), stringsAsFactors = TRUE)


# Serological results
KoCo19 <- readRDS(here_koco_data("R5/R5_CompleteData_NC.RDS"))

# Additional variables for the risk factor analysis
var_risk <- read.csv(here_koco_data("R5/data_for_imputation_NC.CSV"))


# Merge information at the household level
KoCo19 <- merge(KoCo19, info_hh[, c("hh_id", "HousingType", "HouseholdSize", "livingarea_sqm",
                                    "NetIncome_month_hh", "livingstyle")],
                by = "hh_id", all.x = TRUE)

# Merge information at the individual level
KoCo19 <- merge(KoCo19, info_ind[, c("ind_id", "Education_ind", "Employment_ind", "HealthStatus_Self_Overall",
                                     "HealthStatus_Self_Allergies_Respiratory", "HealthStatus_Self_Diabetes",
                                     "HealthStatus_Self_CardiovascularDisease", "HealthStatus_Self_Obesity", 
                                     "HealthStatus_Self_Cancer", "HealthStatus_Self_LungDisease", 
                                     "HealthStatus_Self_Allergies_Skin", "HealthStatus_Self_Autoimmune",
                                     "Medication_Immunosuppressive", "Medication_Cortisone",
                                     "Medication_Bloodpressure")],
                by = "ind_id", all.x = TRUE)

# Add variables for risk factor analysis
KoCo19 <- merge(KoCo19, var_risk)

#########################
# Recode some variables #
#########################

### Sex
KoCo19$Sex <- as.factor(KoCo19$Sex)

### Birth country
KoCo19$Birth_Country <- as.factor(KoCo19$Birth_Country)

### Employment
KoCo19$employment <- ifelse(KoCo19$Employment_ind %in% c("Type1", "Type2", "Type8"),"Unemployed",
                            ifelse(KoCo19$Employment_ind %in% c("Type3","Type4","Type5","Type9"), "Employed",
                                   ifelse(KoCo19$Employment_ind %in% c("Type6","Type7"),"Self Employed",
                                          ifelse(KoCo19$Employment_ind %in% c("Type10","Type11","Type12","Type13"),"Others", NA))))
KoCo19$employment <- factor(KoCo19$employment, levels = c("Employed", "Self Employed", "Unemployed", "Others"))


### Any risk
KoCo19$any_risk_emp <- as.factor(KoCo19$any_risk_emp)

### General health
KoCo19$health <- as.character(KoCo19$HealthStatus_Self_Overall)
KoCo19$health[KoCo19$health %in% c("bad", "less good")] <- "not good"
KoCo19$health <- factor(KoCo19$health, 
                        levels = c("not good", "good", "very good", "excellent"))

### Respiratory allergies
KoCo19$resp_all <- as.character(KoCo19$HealthStatus_Self_Allergies_Respiratory)
KoCo19$resp_all[KoCo19$resp_all == "No"] <- "No"
KoCo19$resp_all[KoCo19$resp_all == "Not sure"] <- NA
KoCo19$resp_all <- factor(KoCo19$resp_all)


### Diabetes
KoCo19$diabetes <- as.character(KoCo19$HealthStatus_Self_Diabetes)
KoCo19$diabetes[KoCo19$diabetes == "No"] <- "No"
KoCo19$diabetes[KoCo19$diabetes == "Not sure"] <- NA
KoCo19$diabetes <- factor(KoCo19$diabetes)


### CVD
KoCo19$cvd <- as.character(KoCo19$HealthStatus_Self_CardiovascularDisease)
KoCo19$cvd[KoCo19$cvd == "No"] <- "No"
KoCo19$cvd[KoCo19$cvd == "Not sure"] <- NA
KoCo19$cvd <- factor(KoCo19$cvd)


### Obesity
KoCo19$obesity <- as.character(KoCo19$HealthStatus_Self_Obesity)
KoCo19$obesity[KoCo19$obesity == "No"] <- "No"
KoCo19$obesity[KoCo19$obesity == "Not sure"] <- NA
KoCo19$obesity <- factor(KoCo19$obesity)

### Cancer
KoCo19$cancer <- as.character(KoCo19$HealthStatus_Self_Cancer)
KoCo19$cancer[KoCo19$cancer == "No"] <- "No"
KoCo19$cancer[KoCo19$cancer == "Not sure"] <- NA
KoCo19$cancer <- factor(KoCo19$cancer)

### Lung disease
KoCo19$lung <- as.character(KoCo19$HealthStatus_Self_LungDisease)
KoCo19$lung[KoCo19$lung == "No"] <- "No"
KoCo19$lung[KoCo19$lung == "Not sure"] <- NA
KoCo19$lung <- factor(KoCo19$lung)

### Skin allergies
KoCo19$skin_all <- as.character(KoCo19$HealthStatus_Self_Allergies_Skin)
KoCo19$skin_all[KoCo19$skin_all == "No"] <- "No"
KoCo19$skin_all[KoCo19$skin_all == "Not sure"] <- NA
KoCo19$skin_all <- factor(KoCo19$skin_all)

### Autoimmune disease
KoCo19$autoimmune <- as.character(KoCo19$HealthStatus_Self_Autoimmune)
KoCo19$autoimmune[KoCo19$autoimmune == "No"] <- "No"
KoCo19$autoimmune[KoCo19$autoimmune == "Not sure"] <- NA
KoCo19$autoimmune <- factor(KoCo19$autoimmune)

### Medication immunosuppressive
KoCo19$med_immunosupp <- as.character(KoCo19$Medication_Immunosuppressive)
KoCo19$med_immunosupp[KoCo19$med_immunosupp == "No"] <- "No"
KoCo19$med_immunosupp[KoCo19$med_immunosupp == "Not sure"] <- NA
KoCo19$med_immunosupp <- factor(KoCo19$med_immunosupp)

### Medication cortisone
KoCo19$med_cortisone <- as.character(KoCo19$Medication_Cortisone)
KoCo19$med_cortisone[KoCo19$med_cortisone == "No"] <- "No"
KoCo19$med_cortisone[KoCo19$med_cortisone == "Not sure"] <- NA
KoCo19$med_cortisone <- factor(KoCo19$med_cortisone)

### Medication blood pressure
KoCo19$med_blood <- as.character(KoCo19$Medication_Bloodpressure)
KoCo19$med_blood[KoCo19$med_blood == "No"] <- "No"
KoCo19$med_blood[KoCo19$med_blood == "Not sure"] <- NA
KoCo19$med_blood <- factor(KoCo19$med_blood)


### Age

# Round 1
KoCo19$Age_R1 <- trunc((as.Date(KoCo19$Q6_f1_Geburtsdatum_DDMONYYYY) %--% as.Date(KoCo19$date_R1))/years(1))
KoCo19$Age_R1[is.na(KoCo19$Age_R1)] <- KoCo19$Age[is.na(KoCo19$Age_R1)]
KoCo19$age_class_R1 <- cut(KoCo19$Age_R1, breaks = c(0, 19, 34, 49, 64, 79, Inf),
                           labels = c("0-19", "20-34", "35-49", "50-64", "65-79", "80+"))

# Round 2
KoCo19$Age_R2 <- trunc((as.Date(KoCo19$Q6_f1_Geburtsdatum_DDMONYYYY) %--% as.Date(KoCo19$date_R2))/years(1))
KoCo19$Age_R2[is.na(KoCo19$Age_R2)] <- KoCo19$Age[is.na(KoCo19$Age_R2)]
KoCo19$age_class_R2 <- cut(KoCo19$Age_R2, breaks = c(0, 19, 34, 49, 64, 79, Inf),
                           labels = c("0-19", "20-34", "35-49", "50-64", "65-79", "80+"))

# Round 3
KoCo19$Age_R3 <- trunc((as.Date(KoCo19$Q6_f1_Geburtsdatum_DDMONYYYY) %--% as.Date(KoCo19$date_R3))/years(1))
KoCo19$Age_R3[is.na(KoCo19$Age_R3)] <- KoCo19$Age[is.na(KoCo19$Age_R3)] + 1
KoCo19$age_class_R3 <- cut(KoCo19$Age_R3, breaks = c(0, 19, 34, 49, 64, 79, Inf),
                           labels = c("0-19", "20-34", "35-49", "50-64", "65-79", "80+"))

# Round 4
KoCo19$Age_R4 <- trunc((as.Date(KoCo19$Q6_f1_Geburtsdatum_DDMONYYYY) %--% as.Date(KoCo19$date_R4))/years(1))
KoCo19$Age_R4[is.na(KoCo19$Age_R4)] <- KoCo19$Age[is.na(KoCo19$Age_R4)] + 1
KoCo19$age_class_R4 <- cut(KoCo19$Age_R4, breaks = c(0, 19, 34, 49, 64, 79, Inf),
                           labels = c("0-19", "20-34", "35-49", "50-64", "65-79", "80+"))

# Round 5
KoCo19$Age_R5 <- trunc((as.Date(KoCo19$Q6_f1_Geburtsdatum_DDMONYYYY) %--% as.Date(KoCo19$date_R5))/years(1))
KoCo19$Age_R5[is.na(KoCo19$Age_R5)] <- KoCo19$Age[is.na(KoCo19$Age_R5)] + 1
KoCo19$age_class_R5 <- cut(KoCo19$Age_R5, breaks = c(0, 19, 34, 49, 64, 79, Inf),
                           labels = c("0-19", "20-34", "35-49", "50-64", "65-79", "80+"))



### Household size

# Size is missing for one household. We checked and household size = 1
KoCo19[is.na(KoCo19$HouseholdSize), "HouseholdSize"] <- 1

KoCo19$hh_members <- cut(KoCo19$HouseholdSize, breaks = c(0, 1, 2, 4, Inf),
                         labels = c("1", "2", "3-4", "5+"))

### Living area/inhabitant
KoCo19$living <- cut(KoCo19$livingarea_sqm/KoCo19$HouseholdSize, breaks = c(0, 30, 40, 55, Inf),
                     labels = c("0-30", "31-40", "41-55", "56+"))


### Income
KoCo19$income <- NA
KoCo19$income[KoCo19$NetIncome_month_hh %in% c("<500", "500-750", "750-1000", "1000-1500", "1500-2000", "2000-2500")] <- "<=2500"
KoCo19$income[KoCo19$NetIncome_month_hh %in% c("2500-3000", "3000-4000")] <- "2501-4000"
KoCo19$income[KoCo19$NetIncome_month_hh %in% c("4000-5000", "5000-6000")] <- "4001-6000"
KoCo19$income[KoCo19$NetIncome_month_hh == ">6000"] <- "6001+"
KoCo19$income <- factor(KoCo19$income, levels = c("<=2500", "2501-4000", "4001-6000", "6001+"))

### Smoking status
KoCo19$smoke <- factor(KoCo19$smokestatus, levels = c("Non smoker", "Past smoker", "Current smoker"))

### House type
KoCo19$house_type <- "5+ apt"
KoCo19$house_type[KoCo19$HousingType %in% c("Type1", "Type2")] <- "1-2 apt"
KoCo19$house_type[KoCo19$HousingType == "Type3"] <- "3-4 apt"
KoCo19$house_type <- factor(KoCo19$house_type)

### Household type
KoCo19$household_type <- ifelse(KoCo19$livingstyle=="Type1" & KoCo19$hh_members == "1","Single",
                                ifelse(KoCo19$livingstyle=="Type1" & KoCo19$hh_members == "2","Couple",
                                       ifelse(KoCo19$livingstyle=="Type2","Family","Others")))
KoCo19$household_type <- factor(KoCo19$household_type, levels = c("Single", "Couple", "Family", "Others"))

### Education
KoCo19$education <- ifelse(KoCo19$Education_ind %in% c("Type1",  "Type2", "Type7","Type8"),"<12 years schooling",
                           ifelse(KoCo19$Education_ind=="Type10",NA,
                                  ifelse(KoCo19$Education_ind=="Type9","In school",">=12 years schooling")))
KoCo19$education <- factor(KoCo19$education, levels = c("In school", "<12 years schooling", ">=12 years schooling"))

### Positive in the past
KoCo19$R2_Result_imp <- KoCo19$R2_Result
KoCo19$R3_Result_imp <- KoCo19$R3_Result
KoCo19$R4_Result_imp <- KoCo19$R4_Result
KoCo19$R5_Result_imp <- KoCo19$R5_Result

KoCo19$R2_Result_imp[KoCo19$R1_Result == "Positive"] <- "Positive"
KoCo19$R3_Result_imp[KoCo19$R2_Result_imp == "Positive"] <- "Positive"
KoCo19$R4_Result_imp[KoCo19$R3_Result_imp == "Positive"] <- "Positive"
KoCo19$R5_Result_imp[KoCo19$R4_Result_imp == "Positive"] <- "Positive"

KoCo19$R2_Result_imp[is.na(KoCo19$R2_Result_imp)] <- "NA"
KoCo19$R3_Result_imp[is.na(KoCo19$R3_Result_imp)] <- "NA"
KoCo19$R4_Result_imp[is.na(KoCo19$R4_Result_imp)] <- "NA"

KoCo19$R2_Result_imp <- factor(KoCo19$R2_Result_imp, levels = c("Negative", "Positive", "NA"))
KoCo19$R3_Result_imp <- factor(KoCo19$R3_Result_imp, levels = c("Negative", "Positive", "NA"))
KoCo19$R4_Result_imp <- factor(KoCo19$R4_Result_imp, levels = c("Negative", "Positive", "NA"))


### No response
KoCo19$ind_nr_r2 <- as.factor(ifelse(is.na(KoCo19$R2_Result), 0, 1))
KoCo19$ind_nr_r3 <- as.factor(ifelse(is.na(KoCo19$R3_Result), 0, 1))
KoCo19$ind_nr_r4 <- as.factor(ifelse(is.na(KoCo19$R4_Result), 0, 1))
KoCo19$ind_nr_r5 <- as.factor(ifelse(is.na(KoCo19$R5_Result), 0, 1))


##########################
# Sum to zero constraint #
##########################

contrasts(KoCo19$employment) <- contr.sum(4, contrasts=TRUE)

contrasts(KoCo19$health) <- contr.sum(4, contrasts=TRUE)

contrasts(KoCo19$age_class_R2) <- contr.sum(6, contrasts=TRUE)
contrasts(KoCo19$age_class_R3) <- contr.sum(6, contrasts=TRUE)
contrasts(KoCo19$age_class_R4) <- contr.sum(6, contrasts=TRUE)
contrasts(KoCo19$age_class_R5) <- contr.sum(6, contrasts=TRUE)

contrasts(KoCo19$income) <- contr.sum(4, contrasts=TRUE)

contrasts(KoCo19$smoke) <- contr.sum(3, contrasts=TRUE)

contrasts(KoCo19$house_type) <- contr.sum(3, contrasts=TRUE)

contrasts(KoCo19$household_type) <- contr.sum(4, contrasts=TRUE)

contrasts(KoCo19$living) <- contr.sum(4, contrasts=TRUE)

contrasts(KoCo19$hh_members) <- contr.sum(4, contrasts=TRUE)

contrasts(KoCo19$education) <- contr.sum(3, contrasts=TRUE)

contrasts(KoCo19$R2_Result_imp) <- contr.sum(3, contrasts=TRUE)
contrasts(KoCo19$R3_Result_imp) <- contr.sum(3, contrasts=TRUE)
contrasts(KoCo19$R4_Result_imp) <- contr.sum(3, contrasts=TRUE)


###################
# Missing pattern #
###################


### Recode income with an extra category "missing"

levels(KoCo19$income) <- c("<=2500", "2501-4000", "4001-6000", "6001+", "NA")
KoCo19$income[is.na(KoCo19$income)] <- "NA"
contrasts(KoCo19$income) <- contr.sum(n = 5)


#############
# Modelling #
#############



### R2
res.glm <- glm(ind_nr_r2 ~ Sex + age_class_R2 + Birth_Country + education + employment + any_risk_emp +
                 smoke + health + resp_all + diabetes + cvd + obesity + cancer + lung +
                 skin_all + autoimmune + household_type + income + living + house_type +
                 R1_Result, data = KoCo19, family = "binomial")

vif(res.glm)

res_r2 <- cbind(exp(cbind(OR = res.glm$coefficients, confint.default(res.glm))), 
                pval = summary(res.glm)$coefficients[, "Pr(>|z|)"])

### R3
res.glm <- glm(ind_nr_r3 ~ Sex + age_class_R3 + Birth_Country + education + employment + any_risk_emp +
                 smoke + health + resp_all + diabetes + cvd + obesity + cancer + lung +
                 skin_all + autoimmune + household_type + income + living + house_type +
                 R2_Result_imp, data = KoCo19, family = "binomial")

vif(res.glm)

res_r3 <- cbind(exp(cbind(OR = res.glm$coefficients, confint.default(res.glm))), 
                pval = summary(res.glm)$coefficients[, "Pr(>|z|)"])


### R4
res.glm <- glm(ind_nr_r4 ~ Sex + age_class_R4 + Birth_Country + education + employment + any_risk_emp +
                 smoke + health + resp_all + diabetes + cvd + obesity + cancer + lung +
                 skin_all + autoimmune + household_type + income + living + house_type +
                 R3_Result_imp, data = KoCo19, family = "binomial")

vif(res.glm)

res_r4 <- cbind(exp(cbind(OR = res.glm$coefficients, confint.default(res.glm))), 
                pval = summary(res.glm)$coefficients[, "Pr(>|z|)"])


### R5
res.glm <- glm(ind_nr_r5 ~ Sex + age_class_R5 + Birth_Country + education + employment + any_risk_emp +
                 smoke + health + resp_all + diabetes + cvd + obesity + cancer + lung +
                 skin_all + autoimmune + household_type + income + living + house_type +
                 R4_Result_imp, data = KoCo19, family = "binomial")

vif(res.glm)

res_r5 <- cbind(exp(cbind(OR = res.glm$coefficients, confint.default(res.glm))), 
                pval = summary(res.glm)$coefficients[, "Pr(>|z|)"])

#############################
# Change reference category #
#############################


### Change reference category to calculate the coefficient for the missing category

# Age
KoCo19$age_class_R2 <- relevel(KoCo19$age_class_R2, ref = "80+")
contrasts(KoCo19$age_class_R2) <- contr.sum(n = 6)
KoCo19$age_class_R3 <- relevel(KoCo19$age_class_R3, ref = "80+")
contrasts(KoCo19$age_class_R3) <- contr.sum(n = 6)
KoCo19$age_class_R4 <- relevel(KoCo19$age_class_R4, ref = "80+")
contrasts(KoCo19$age_class_R4) <- contr.sum(n = 6)
KoCo19$age_class_R5 <- relevel(KoCo19$age_class_R5, ref = "80+")
contrasts(KoCo19$age_class_R5) <- contr.sum(n = 6)


# Education
KoCo19$education <- relevel(KoCo19$education, ref = ">=12 years schooling")
contrasts(KoCo19$education) <- contr.sum(n = 3)

# Employment
KoCo19$employment <- relevel(KoCo19$employment, ref = "Others")
contrasts(KoCo19$employment) <- contr.sum(n = 4)

# Smoke
KoCo19$smoke <- relevel(KoCo19$smoke, ref = "Current smoker")
contrasts(KoCo19$smoke) <- contr.sum(n = 3)

# Health
KoCo19$health <- relevel(KoCo19$health, ref = "excellent")
contrasts(KoCo19$health) <- contr.sum(n = 4)

# Household type
KoCo19$household_type <- relevel(KoCo19$household_type, ref = "Others")
contrasts(KoCo19$household_type) <- contr.sum(n = 4)

# Income
KoCo19$income <- relevel(KoCo19$income, ref = "NA")
contrasts(KoCo19$income) <- contr.sum(n = 5)

# Living area
KoCo19$living <- relevel(KoCo19$living, ref = "56+")
contrasts(KoCo19$living) <- contr.sum(n = 4)

# House type
KoCo19$house_type <- relevel(KoCo19$house_type, ref = "5+ apt")
contrasts(KoCo19$house_type) <- contr.sum(n = 3)


# Result previous round
KoCo19$R2_Result_imp <- relevel(KoCo19$R2_Result_imp, ref = "NA")
contrasts(KoCo19$R2_Result_imp) <- contr.sum(n = 3)

KoCo19$R3_Result_imp <- relevel(KoCo19$R3_Result_imp, ref = "NA")
contrasts(KoCo19$R3_Result_imp) <- contr.sum(n = 3)

KoCo19$R4_Result_imp <- relevel(KoCo19$R4_Result_imp, ref = "NA")
contrasts(KoCo19$R4_Result_imp) <- contr.sum(n = 3)


############
# Model R2 #
############

res.glm <- glm(ind_nr_r2 ~ Sex + age_class_R2 + Birth_Country + education + employment + any_risk_emp +
                 smoke + health + resp_all + diabetes + cvd + obesity + cancer + lung +
                 skin_all + autoimmune + household_type + income + living + house_type +
                 R1_Result, data = KoCo19, family = "binomial")

res2 <- cbind(exp(cbind(OR = res.glm$coefficients, confint.default(res.glm))), 
               pval = summary(res.glm)$coefficients[, "Pr(>|z|)"])

### Aggregate results

coef_r2 <- rbind(res_r2[2:7, ], res2["age_class_R21", ], 
                 res_r2[8:10, ], res2["education1", ],
                 res_r2[11:13, ], res2["employment1", ], 
                 res_r2[14:16, ], res2["smoke1", ],
                 res_r2[17:19, ], res2["health1", ],
                 res_r2[20:30, ], res2["household_type1", ], 
                 res_r2[31:34, ], res2["income1", ],
                 res_r2[35:37, ], res2["living1", ],
                 res_r2[38:39, ], res2["house_type1", ],
                 res_r2[40, ])

coef_r2 <- as.data.frame(coef_r2)

### Formatting

# CI
coef_r2$CI <- paste0("[", format(round(coef_r2[, "2.5 %"], 2), nsmall = 2), "; ",
                     format(round(coef_r2[, "97.5 %"], 2), nsmall = 2), "]")

coef_r2$OR <- format(round(coef_r2$OR, 2), nsmall = 2)


# Add stars

coef_r2$stars <- cut(coef_r2$pval, breaks = c(0, 0.001, 0.01, 0.05, 1),
                     labels = c("***", "**", "*", ""),
                     right = FALSE)


### Save file
res.coef <- list()
res.coef$R2 <- coef_r2


############
# Model R3 #
############
res.glm <- glm(ind_nr_r3 ~ Sex + age_class_R3 + Birth_Country + education + employment + any_risk_emp +
                 smoke + health + resp_all + diabetes + cvd + obesity + cancer + lung +
                 skin_all + autoimmune + household_type + income + living + house_type +
                 R2_Result_imp, data = KoCo19, family = "binomial")

res2 <- cbind(exp(cbind(OR = res.glm$coefficients, confint.default(res.glm))), 
              pval = summary(res.glm)$coefficients[, "Pr(>|z|)"])

### Aggregate results

coef_r3 <- rbind(res_r3[2:7, ], res2["age_class_R31", ], 
                 res_r3[8:10, ], res2["education1", ],
                 res_r3[11:13, ], res2["employment1", ], 
                 res_r3[14:16, ], res2["smoke1", ],
                 res_r3[17:19, ], res2["health1", ],
                 res_r3[20:30, ], res2["household_type1", ], 
                 res_r3[31:34, ], res2["income1", ],
                 res_r3[35:37, ], res2["living1", ],
                 res_r3[38:39, ], res2["house_type1", ],
                 res_r3[40:41, ], res2["R2_Result_imp1", ])

coef_r3 <- as.data.frame(coef_r3)

### Formatting

# CI
coef_r3$CI <- paste0("[", format(round(coef_r3[, "2.5 %"], 2), nsmall = 2), "; ",
                     format(round(coef_r3[, "97.5 %"], 2), nsmall = 2), "]")

coef_r3$OR <- format(round(coef_r3$OR, 2), nsmall = 2)


# Add stars

coef_r3$stars <- cut(coef_r3$pval, breaks = c(0, 0.001, 0.01, 0.05, 1),
                     labels = c("***", "**", "*", ""),
                     right = FALSE)


### Save file
res.coef$R3 <- coef_r3


############
# Model R4 #
############
res.glm <- glm(ind_nr_r4 ~ Sex + age_class_R4 + Birth_Country + education + employment + any_risk_emp +
                 smoke + health + resp_all + diabetes + cvd + obesity + cancer + lung +
                 skin_all + autoimmune + household_type + income + living + house_type +
                 R3_Result_imp, data = KoCo19, family = "binomial")

res2 <- cbind(exp(cbind(OR = res.glm$coefficients, confint.default(res.glm))), 
              pval = summary(res.glm)$coefficients[, "Pr(>|z|)"])

### Aggregate results

coef_r4 <- rbind(res_r4[2:7, ], res2["age_class_R41", ], 
                 res_r4[8:10, ], res2["education1", ],
                 res_r4[11:13, ], res2["employment1", ], 
                 res_r4[14:16, ], res2["smoke1", ],
                 res_r4[17:19, ], res2["health1", ],
                 res_r4[20:30, ], res2["household_type1", ], 
                 res_r4[31:34, ], res2["income1", ],
                 res_r4[35:37, ], res2["living1", ],
                 res_r4[38:39, ], res2["house_type1", ],
                 res_r4[40:41, ], res2["R3_Result_imp1", ])

coef_r4 <- as.data.frame(coef_r4)

### Formatting

# CI
coef_r4$CI <- paste0("[", format(round(coef_r4[, "2.5 %"], 2), nsmall = 2), "; ",
                     format(round(coef_r4[, "97.5 %"], 2), nsmall = 2), "]")

coef_r4$OR <- format(round(coef_r4$OR, 2), nsmall = 2)


# Add stars

coef_r4$stars <- cut(coef_r4$pval, breaks = c(0, 0.001, 0.01, 0.05, 1),
                     labels = c("***", "**", "*", ""),
                     right = FALSE)


### Save file
res.coef$R4 <- coef_r4


############
# Model R5 #
############
res.glm <- glm(ind_nr_r5 ~ Sex + age_class_R5 + Birth_Country + education + employment + any_risk_emp +
                 smoke + health + resp_all + diabetes + cvd + obesity + cancer + lung +
                 skin_all + autoimmune + household_type + income + living + house_type +
                 R4_Result_imp, data = KoCo19, family = "binomial")

res2 <- cbind(exp(cbind(OR = res.glm$coefficients, confint.default(res.glm))), 
              pval = summary(res.glm)$coefficients[, "Pr(>|z|)"])

### Aggregate results

coef_r5 <- rbind(res_r5[2:7, ], res2["age_class_R51", ], 
                 res_r5[8:10, ], res2["education1", ],
                 res_r5[11:13, ], res2["employment1", ], 
                 res_r5[14:16, ], res2["smoke1", ],
                 res_r5[17:19, ], res2["health1", ],
                 res_r5[20:30, ], res2["household_type1", ], 
                 res_r5[31:34, ], res2["income1", ],
                 res_r5[35:37, ], res2["living1", ],
                 res_r5[38:39, ], res2["house_type1", ],
                 res_r5[40:41, ], res2["R4_Result_imp1", ])

coef_r5 <- as.data.frame(coef_r5)

### Formatting

# CI
coef_r5$CI <- paste0("[", format(round(coef_r5[, "2.5 %"], 2), nsmall = 2), "; ",
                     format(round(coef_r5[, "97.5 %"], 2), nsmall = 2), "]")

coef_r5$OR <- format(round(coef_r5$OR, 2), nsmall = 2)


# Add stars

coef_r5$stars <- cut(coef_r5$pval, breaks = c(0, 0.001, 0.01, 0.05, 1),
                     labels = c("***", "**", "*", ""),
                     right = FALSE)


### Save file
res.coef$R5 <- coef_r5




##############
# Export all #
##############

write.xlsx(res.coef, file = here_koco_results("NR_mechanism_NA.xlsx"), overwrite = TRUE)

