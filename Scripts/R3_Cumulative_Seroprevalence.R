#' This script calibrates the sampling weights (from the script Sampling_weights.R)
#' for the third round of visit and calculates the variance associated with the estimation of the proportion of ppl.
#' infected by the virus (positive tested).
#' We consider that if someone was positive once in the past, this person is positive for the next rounds
#' We calibrate on the number of vaccinated persons in Munich.

run <- function(seed){
  
  rm(list = ls())
  

  here_koco_scripts = function (...) here::here("Scripts", ...)
  here_koco_data = function (...) here::here("Data", ...)
  here_koco_results = function (...) here::here("Results", ...)
  
  #############################
  # Load data sets
  #############################
  
  ###
  # Sampling Weights
  ###
  
  KoCo_BLab <- readRDS(here_koco_results("SamplingWeights.RDS"))
  
  
  
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
  # Necessary dataset for the treatment of non response
  ###
  
  # Collect information on respondents and non-respondents at the household level
  info_hh <- read.csv(here_koco_data("R1/hh_characteristics_new.csv"), stringsAsFactors = TRUE)
  
  
  
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
  margins_age_sex <- read.csv(here_koco_data("R3/muc_age_structure_KoCo_study_pop.csv"), stringsAsFactors = F)
  
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
  
  info_ind <- readRDS(paste0(here_koco_data("Imputed_Data"), "/Koco_R5latest_data_seed", seed, ".RDS"))
  
  KoCo_BLab <- merge(KoCo_BLab, info_ind)
  
  KoCo_BLab$Age2 <- trunc((as.Date(KoCo_BLab$Q6_f1_Geburtsdatum_DDMONYYYY) %--% as.Date(KoCo_BLab$date_R3))/years(1))
  KoCo_BLab$Age2[is.na(KoCo_BLab$Age2)] <- KoCo_BLab$Age[is.na(KoCo_BLab$Age2)] + 1
  
  anyNA(KoCo_BLab$Age2)
  
  KoCo_BLab$Age <- KoCo_BLab$Age2
  
  KoCo_BLab <- KoCo_BLab[setdiff(names(KoCo_BLab), c("Q6_f1_Geburtsdatum_DDMONYYYY", "date_R3", "Age2"))]
  
  
  
  #############################
  # Define sample for the third round
  #############################
  
  
  ### Recalculate the weights within the households (non response treatment, since some participants are missing)
  
  # Creating a variable with the number of people (ppl) in the households that are taking part in the study
  KoCo_BLab <- merge(KoCo_BLab, as.data.frame(table(KoCo_BLab$hh_id[!is.na(KoCo_BLab$R3_Result_imp)])), by.x = "hh_id", by.y = "Var1", all.x=T)
  names(KoCo_BLab)[names(KoCo_BLab)=="Freq"] <- "n_ppl_part_hh"
  
  
  # Calculating the weight for ppl in the household depending on other mm of the hh (not taking part in the study but in the study pop)
  KoCo_BLab$w_ind <- KoCo_BLab$obs_hh_members_study / KoCo_BLab$n_ppl_part_hh 
  
  # Remove unnecessary variable
  KoCo_BLab$n_ppl_part_hh <- NULL
  
  
  # Shrink the sample to the respondents
  KoCo_BLab <- KoCo_BLab[!is.na(KoCo_BLab$R3_Result_imp), ]
  
  table(KoCo_BLab$R3_Result)
  
  
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
  # Numbers can be found in Data/R3/KoCo19_202003_202012_202103_Blatt31.xlsx
  country_germany_unknow <- 890546
  country_other <-  sum(totals) - country_germany_unknow
  
  ###
  # Total number of vaccinated ppl (1 dose)
  ###
  
  # https://www.muenchen.de/rathaus/Stadtinfos/Coronavirus-Fallzahlen.html#Impfungen
  n_pos <- 149536
  n_neg <- sum(totals) - n_pos
  
  ###
  # Nb of single/multi-ppl hh and hh with/without children in Munich (based on KoCo19, stat. Amt)
  ###
  
  # Selecting the margins for household size (1 vs. 2 vs. multi) 
  # Main residences + Main and secondary residences
  # Numbers can be found in Data/R3/KoCo19_202103-3.xlsx
  n_1_house <- 451495
  n_2_house <- 204375+2891
  n_multi_house <- 829831+5799 - n_1_house - n_2_house
  
  # Selecting the margins for household with or without children in Munich
  # Numbers can be found in Data/R3/KoCo19_202103-3.xlsx
  n_house_0_child <- 683488+4451
  n_house_1plus_child <- 829831+5799 - n_house_0_child
  
  
  ###
  # Summarizing the information about Munich
  ###
  totals <- c(totals, country_germany_unknow = country_germany_unknow, country_other = country_other,
              n_pos = n_pos, n_neg = n_neg,
              n_1_house = n_1_house, n_2_house = n_2_house, n_multi_house = n_multi_house,
              n_house_0_child = n_house_0_child, n_house_1plus_child = n_house_1plus_child)
  
  # Removing temporary data
  rm(margins_age_sex, country_germany_unknow, country_other, n_pos, n_neg, n_1_house, n_2_house, n_multi_house, n_house_0_child, n_house_1plus_child)
  
  
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
  
  # Creating a variable including the information about hh_id and vaccination
  KoCo_BLab$hh_vax <- paste(KoCo_BLab$hh_id, KoCo_BLab$R3_Vaccination, sep = "_")
  
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
  
  ### Vaccination
  
  # Calculating the number vaccinated/non vaccinated in each household
  n_vax <- freq(KoCo_BLab$hh_vax, w = KoCo_BLab$w_ind, plot=F)
  
  # Removing the line for totals
  n_vax <- n_vax[-nrow(n_vax), ]
  
  # Splitting the information saved in the variable about hh_id and vaccination
  n_vax <- data.frame(hh_id = substr(rownames(n_vax), 1, 7),
                      vax = substr(rownames(n_vax), 9, 15),
                      freq = n_vax[, "Frequency"])
  
  
  # Reshaping the dataframe to have for each hh the number of ppl based on contry of origin
  data_house2 <- reshape(n_vax, v.names = "freq",
                         timevar = "vax", idvar = "hh_id", direction = "wide")
  
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
                               "Germany", "Other", "Positiv", "Negativ")]
  
  
  
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
  rm(n_sex_age, n_country, n_vax, KoCo19, data_house2, nb_child)
  
  #############################
  # Non response treatment
  #############################
  
  #############
  # Estimated probability of NR
  #############
  
  # Link article Juillard, Chauvet:
  # https://www150.statcan.gc.ca/n1/pub/12-001-x/2018002/article/54952-eng.htm
  
  
  # Dummy variable of response at the household level
  ind_nr <- as.numeric(info_hh$hh_id %in% unique(KoCo_BLab$hh_id))
  
  # Create a data frame data_nr to analyse the NR behaviour
  data_nr <- data.frame(hh_id = info_hh$hh_id, ind_nr = ind_nr)
  
  # Add info at the household level
  # Note: More information was included at the beginning, e.g. smoker in the household, living area, constituency, etc...
  # However, these variables did not have an effect on the NR at the household level. Therefore, we do not include them here.
  data_nr <- merge(data_nr, info_hh[, c("hh_id", "HousingType", "HouseholdSize",
                                        "NetIncome_month_hh", "HighestEducation_hh")], by = "hh_id", all.x = TRUE)
  
  # Aggregate individual information at the household level
  info_ind_nr <- by(info_ind, info_ind$hh_id, function(x){
    # male <- sum(x[, "Sex"] == "Male", na.rm = TRUE)
    foreign <- sum(x[, "Birth_Country"] == "Other", na.rm = TRUE)
    mean_age <- mean(x[, "Age"])
    # income <- sum(x[, "NetIncome_monthly_1"], na.rm = TRUE)
    pos <- sum(x[, "R1_Result"] == "Positive" | x[, "R2_Result"] == "Positive" 
               , na.rm = TRUE)
    return(c(mean_age = mean_age, foreign = foreign, pos = pos))
  })
  
  
  info_ind_nr <- as.data.frame(do.call(rbind, info_ind_nr))
  info_ind_nr$hh_id <- rownames(info_ind_nr)
  
  # Combine information at the individual level and at the household level
  data_nr <- merge(data_nr, info_ind_nr, by = "hh_id")
  
  ###
  # Process information at the individual level
  ###
  
  # indicator if at least 1 member is a foreigner
  data_nr$foreign <- as.factor(ifelse(data_nr$foreign == 0, 0, 1))
  
  # Categorize average age of the household
  data_nr$mean_age <- cut(data_nr$mean_age, breaks = c(0, 34, 49, 64, 79, Inf),
                          labels = c("0-34", "35-49", "50-64", "65-79", "80+"))
  
  # If at least 1 member was positive, household positive
  data_nr$pos <- as.factor(ifelse(data_nr$pos == 0, 0, 1))
  
  ###
  # Process information at the household level
  ###
  
  # Income
  data_nr$income <- "No Answer"
  data_nr$income[data_nr$NetIncome_month_hh %in% c("<500", "500-750", "750-1000", "1000-1500", "1500-2000", "2000-2500")] <- "<=2500"
  data_nr$income[data_nr$NetIncome_month_hh %in% c("2500-3000", "3000-4000")] <- "2501-4000"
  data_nr$income[data_nr$NetIncome_month_hh %in% c("4000-5000", "5000-6000")] <- "4001-6000"
  data_nr$income[data_nr$NetIncome_month_hh == ">6000"] <- "6001+"
  data_nr$income <- factor(data_nr$income, levels = c("No Answer", "<=2500", "2501-4000", "4001-6000", "6001+"))
  
  # House type
  data_nr$house_type <- "5+ apt"
  data_nr$house_type[data_nr$HousingType %in% c("Type1", "Type2")] <- "1-2 apt"
  data_nr$house_type[data_nr$HousingType == "Type3"] <- "3-4 apt"
  # data_nr$house_type[data_nr$HousingType == "Type7"] <- "Others"
  data_nr$house_type <- as.factor(data_nr$house_type)
  
  
  
  # Household size
  data_nr$hh_members <- cut(data_nr$HouseholdSize, breaks = c(0, 1, 2, Inf),
                            labels = c("1", "2", "3+"))
  
  # Size is missing for one household. We checked and household size = 1
  data_nr[is.na(data_nr$hh_members), "hh_members"] <- 1
  
  # Highest education
  data_nr$education <- "Unknow"
  data_nr$education[data_nr$HighestEducation_hh %in% c("Type1", "Type2", "Type3", "Type4", "Type7", "Type8", "Type9")] <- "<= 12 yrs"
  data_nr$education[data_nr$HighestEducation_hh %in% c("Type5", "Type6")] <- "> 12 yrs"
  data_nr$education <- factor(data_nr$education, levels = c("Unknow", "<= 12 yrs", "> 12 yrs"))
  
  # No response
  data_nr$ind_nr <- as.factor(data_nr$ind_nr)
  
  
  ###
  # Model the non response to predict the probability of response
  ###
  
  model <- glm(ind_nr ~ income + mean_age + house_type + hh_members + education + pos, data = data_nr, family = "binomial")
  summary(model)
  
  
  # Predicted probability of response
  phat <- predict(model, type = "response")
  summary(phat)
  
  ###
  # Create homogeneous group of response (HRG)
  ###
  
  # We use here a very simple approach to create these groups by cutting the probabilities of response at each decile
  # A more sophisticated approach was also applied (using k-means to cluster the probabilities), providing similar results.
  
  hrg <- cut(phat, breaks = quantile(phat, probs = seq(0,1, 0.1)), include.lowest = TRUE)
  
  data_nr$hrg <- hrg
  
  # Estimate the probabilities in each HRG by number of respondents / number of participant
  phat <- by(data_nr, data_nr$hrg, function(x){
    prop.table(table(x[, "ind_nr"]))[2]
  })
  
  
  # Add these probabilities to the KoCo_BLab table (participants to the follow-up)
  phat <- do.call(rbind, as.list(phat))
  
  phat <- data.frame(hrg = rownames(phat), phat = phat)
  
  data_nr <- merge(data_nr, phat, by = "hrg")
  
  KoCo_BLab <- merge(KoCo_BLab, data_nr[, c("hh_id", "phat")], by = "hh_id", all.x = TRUE)
  
  
  
  #############################
  # Calibration Roche DBS
  #############################
  
  # Checking if the order of age/sex categories are the same
  colnames(data_house)
  names(totals)
  
  # Sampling weights at the household level
  d_house <- KoCo_BLab$w_constituency * KoCo_BLab$w_household / KoCo_BLab$phat
  names(d_house) <- KoCo_BLab$hh_id
  d_house <- d_house[!duplicated(names(d_house))]
  
  # Ordering weights in the same order than the household data
  d_house <- d_house[rownames(data_house)]
  
  ###
  # Calculating the calibrated weights by means of different calibration methods.
  ###
  
  ### method "logit"
  g_logit <- calib(data_house, d = d_house, totals, method="logit",
                   bounds=c(0.3,2.6), max_iter = 2000)  
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
  
  
  freq(x = KoCo_BLab$R3_Result_imp, plot=F)
  
  
  freq(x = KoCo_BLab$R3_Vaccination, plot=F)
  
  
  freq(x = KoCo_BLab$R3_Result_S1_imp, plot=F)
  
  
  
  freq(x = KoCo_BLab$R3_Result_imp, w = KoCo_BLab$w_ind_cal, plot=F)
  
  
  freq(x = KoCo_BLab$R3_Vaccination, w = KoCo_BLab$w_ind_cal, plot=F)
  
  
  
  freq(x = KoCo_BLab$R3_Result_S1_imp, w = KoCo_BLab$w_ind_cal, plot=F)
  
  
  KoCo_BLab$R3_S1_N <- ifelse(KoCo_BLab$R3_Result_imp == "Positive" | KoCo_BLab$R3_Result_S1_imp == "Positive",
                              "Positive", "Negative")
  
  freq(x = KoCo_BLab$R3_S1_N, plot=F)
  
  freq(x = KoCo_BLab$R3_S1_N, w = KoCo_BLab$w_ind_cal, plot=F)
  
  ### Removing temporary data
  rm(list = setdiff(ls(), c("KoCo_BLab", "data_house", "Const", "Munich_hh", "d_house",
                            "here_koco_data", "here_koco_results", "here_koco_scripts")))
  
  #############################
  # Variance of the calibrated estimator
  #############################
  
  
  ###
  # calculate the linearized variable for a proportion
  ###
  
  ### Linearization at the individual level
  
  KoCo_BLab$DBS_results <- ifelse(KoCo_BLab$R3_Result_imp == "Positive", 1, 0)
  
  # Weighted proportion for DBS
  p_dbs <- sum(KoCo_BLab$DBS_results * KoCo_BLab$w_ind_cal)/sum(KoCo_BLab$w_ind_cal)
  
  
  # Linearized variable
  KoCo_BLab$dbs_u_k_0 <- (KoCo_BLab$DBS_results - p_dbs)/sum(KoCo_BLab$w_ind_cal)
  
  
  ### Linearization for ppl with S1 positive
  KoCo_BLab$S1_results <- ifelse(KoCo_BLab$R3_Result_S1_imp == "Positive", 1, 0)
  
  # Weighted proportion for DBS
  p_s1 <- sum(KoCo_BLab$S1_results * KoCo_BLab$w_ind_cal)/sum(KoCo_BLab$w_ind_cal)
  
  
  # Linearized variable
  KoCo_BLab$dbs_u_k_1 <- (KoCo_BLab$S1_results - p_s1)/sum(KoCo_BLab$w_ind_cal)
  
  
  ### Linearization for ppl with S or N positive
  
  KoCo_BLab$S1_N_results <- ifelse(KoCo_BLab$R3_S1_N == "Positive", 1, 0)
  
  # Weighted proportion for DBS
  p_s1_n <- sum(KoCo_BLab$S1_N_results * KoCo_BLab$w_ind_cal)/sum(KoCo_BLab$w_ind_cal)
  
  
  # Linearized variables
  KoCo_BLab$dbs_u_k_2 <- (KoCo_BLab$S1_N_results - p_s1_n)/sum(KoCo_BLab$w_ind_cal)
  
  
  
  ### Linearization for the two groups: positive in the past and the negative in the past
  group1 <- split(KoCo_BLab, KoCo_BLab$R2_Result_new)
  
  ### Linearization for the two groups: vaccinated or not
  group2 <- split(KoCo_BLab, KoCo_BLab$R3_Vaccination)
  
  ### Linearization for the two groups: S1 positive or not
  group3 <- split(KoCo_BLab, KoCo_BLab$R3_Result_S1_imp)
  
  group <- do.call(c, list(group1, group2, group3))
  
  # Weighted proportions for DBS in each group:
  # - Negative in the past: proportion of positive in the third round
  # - Positive in the past: Proportions of positive AND negative in the third round
  # We just calculate the proportion p of those who remain positive (sero-persistence) 
  # and calculate the sero-remission by 1-p
  # - Non vaccinated: proportion of positive in the third round
  # - Vaccinated: Proportions of positive
  # - S1 negative: proportion of positive in the third round
  # - S1 positive: Proportions of positive
  
  p_dbs <- sapply(group, function(z){
    pos <- sum(z[, "DBS_results"] * z[, "w_ind_cal"])/sum(z[, "w_ind_cal"])
  })
  
  
  p_dbs <- c(Negative_Positive = p_dbs[1], Positive_Positive = p_dbs[2],
             Non_vax = p_dbs[3], Vax = p_dbs[4], S_neg = p_dbs[5], S_pos = p_dbs[6])
  
  
  
  # Linearized variables
  
  KoCo_BLab_group <- lapply(1:length(group), function(x){
    group[[x]][, paste0("dbs_u_k_", x+2)] <- (group[[x]][, "DBS_results"] - p_dbs[x])/sum(group[[x]][, "w_ind_cal"])
    return(group[[x]])
  })
  
  
  # Combine datasets
  KoCo_BLab_group1 <- do.call(dplyr::bind_rows, KoCo_BLab_group[1:2])
  
  KoCo_BLab_group2 <- do.call(dplyr::bind_rows, KoCo_BLab_group[3:4])
  
  KoCo_BLab_group3 <- do.call(dplyr::bind_rows, KoCo_BLab_group[5:6])
  
  KoCo_BLab_group <- merge(KoCo_BLab_group1, KoCo_BLab_group2[, c("ind_id", "dbs_u_k_5", "dbs_u_k_6")])
  
  KoCo_BLab_group <- merge(KoCo_BLab_group, KoCo_BLab_group3[, c("ind_id", "dbs_u_k_7", "dbs_u_k_8")])
  
  # Change NAs to zero
  KoCo_BLab_group[, paste0("dbs_u_k_", 0:(length(group)+2))][is.na(KoCo_BLab_group[, paste0("dbs_u_k_", 0:(length(group)+2))])] <- 0
  
  
  
  
  ### Linearization at the household level
  
  split_hh <- split(KoCo_BLab_group[, c("hh_id", "w_ind", paste0("dbs_u_k_", 0:(length(group)+2)))], KoCo_BLab_group$hh_id)
  
  hh_u_k <- sapply(split_hh, function(z){
    res <- sapply(0:(length(group)+2), function(x){
      sum(z[, paste0("dbs_u_k_", x)] * z[, "w_ind"])
    })
    return(res)
  })
  
  
  hh_u_k <- as.data.frame(t(hh_u_k))
  
  hh_u_k$hh_id <- rownames(hh_u_k)
  
  
  
  ###
  # Create a data frame at the hh level with the linearized variable,
  # the auxiliary information, the calibrated weights and the ratio
  # of the calibrated weights and the sampling weights g
  ###
  
  data_house$hh_id <- rownames(data_house)
  
  data_house <- merge(data_house, KoCo_BLab[!duplicated(KoCo_BLab$hh_id),
                                            c("hh_id", "w_hh_noshare", "w_household", "w_hh_cal", "g", "phat")],
                      by = "hh_id")
  
  
  data_house <- merge(data_house, hh_u_k, by = "hh_id")
  
  identical(data_house$hh_id, names(d_house))
  
  ###
  # Run a linear model with the linearized variable as response variable and the
  # auxiliary information as covariates including the sampling weights
  ###
  
  res.lm <- lapply(0:(length(group)+2), function(x){
    res.lm <- lm(as.formula(paste(paste0("V",x+1), paste(paste0("`", names(data_house[, 2:22]), "`"), collapse=" + "), sep=" ~ ")), 
                 weights = d_house,
                 data = data_house)
  })
  
  ###############
  # Variance estimation
  ###############
  
  # For the variance estimation, we will use the residuals e_k -> ge
  
  data_res <- sapply(res.lm, function(x){
    x$residuals
  })
  
  colnames(data_res) <- paste0("ge_dbs_", 0:(length(group)+2))
  
  # Keep useful variables
  data_res <- cbind(data_house[, c("hh_id", "w_hh_noshare", "w_household" , "phat")], data_res)
  
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
    res <- sapply(0:(length(group)+2), function(x){
      sum(z[, paste0("ge_dbs_", x)] * z[, "w_household"] / z[, "phat"] )
    })
    return(res)
  })
  
  
  # Calculate the dispersion of ge
  s_e_dbs <- apply(tot_e_const, 1, var)
  
  
  # First term of the variance estimation
  V1_dbs <- N_const^2 * (1/n_const - 1/N_const) * s_e_dbs
  
  
  ########
  ### Second term of the variance estimation including the non response term
  ########
  
  
  # Own function for the variance including the probability of response
  # (inspired by varHT from the library sampling)
  varHT_home <- function (y, pikl, p_rep, method = 1) 
  {
    if (any(is.na(pikl))) 
      stop("there are missing values in pikl")
    if (any(is.na(p_rep))) 
      stop("there are missing values in p_rep")
    if (any(is.na(y))) 
      stop("there are missing values in y")
    if (!(is.data.frame(pikl) | is.matrix(pikl))) 
      stop("pikl should be a matrix or a data frame")
    if (!(is.data.frame(p_rep) | is.matrix(p_rep))) 
      stop("p_rep should be a matrix or a data frame")
    if (is.data.frame(pikl) | is.matrix(pikl)) 
      if (nrow(pikl) != ncol(pikl)) 
        stop("pikl is not a square matrix")
    if (is.data.frame(p_rep) | is.matrix(p_rep)) 
      if (nrow(p_rep) != ncol(p_rep)) 
        stop("p_rep is not a square matrix")
    if (length(y) != nrow(pikl)) 
      stop("y and pik have different sizes")
    if (length(y) != nrow(p_rep)) 
      stop("y and p_rep have different sizes")
    if (!missing(method) & !(method %in% c(1, 2))) 
      stop("the method should be 1 or 2")
    if (is.data.frame(pikl)) 
      pikl = as.matrix(pikl)
    if (is.data.frame(p_rep)) 
      p_rep = as.matrix(p_rep)
    pik = diag(pikl)
    pik1 = outer(pik, pik, "*")
    delta = pikl - pik1
    diag(delta) = pik * (1 - pik)
    y1 = outer(y, y, "*")
    if (method == 1) 
      return(sum(y1 * delta/(pik1 * pikl * p_rep)))
    if (method == 2) {
      y2 = outer(y/pik, y/pik, "-")^2
      return(0.5 * sum(y2 * (pik1 - pikl)/(pikl * p_rep)))
    }
  }
  
  
  # Nb hh surveyed
  nb_hh_s <- as.data.frame(table(Const$const_start))
  
  # Add to the data the nb of hh in each constituency and the number of hh surveyed
  data_res <- merge(data_res, nb_hh_s, by.x = "const_start", by.y = "Var1")
  data_res <- merge(data_res, Munich_hh, by.x = "const_start", by.y = "const", all.x = TRUE)
  
  # Split data based on the constituencies (in order to calculate the second order inclusion probabilities)
  data_res_split <- split(data_res, data_res$const_start)
  
  ### Calculate the second term of the variance associated to the sampling design and to the non response
  V2_dbs <- sapply(data_res_split, function(x){
    # Matrix containing the second order inclusion probabilities of each hh in consituency x
    pi2 <- matrix(unique(x[, "Freq"] * (x[, "Freq"] - 1)) / (x[, "Nb_hh"] * (x[, "Nb_hh"] - 1)), nrow = nrow(x), ncol = nrow(x))
    diag(pi2) <- unique(x[, "Freq"] / x[, "Nb_hh"])
    # Matrix containig the single and joint probabilities of response of each household in constituency x
    mat_p_rep <- outer(x[, "phat"], x[, "phat"], "*")
    diag(mat_p_rep) <- x[, "phat"]
    # For each variable of interest, we calculate the second term of the variance in constituency x
    V2_dbs <- sapply(0:(length(group)+2), function(y){
      # Variance associated to the sampling design for variable y
      var_ht <- varHT_home(x[, paste0("ge_dbs_", y)], pi2, mat_p_rep, method = 1)
      # Variance associated to the NR for variable y
      var_nr <- sum((x[, paste0("ge_dbs_", y)] / diag(pi2))^2 * (1 - diag(mat_p_rep))/diag(mat_p_rep)^2)
      return(sum(var_ht, var_nr))
    }
    )
    
    return(V2_dbs)
  })
  
  # Second term of the variance
  
  V2_dbs <- N_const / n_const * rowSums(V2_dbs)
  
  ###
  # Variance estimation and confidence intervals
  ###
  
  w_p <- sapply(group, function(z){
    freq(z[, "R3_Result_imp"], w = z[, "w_ind_cal"], plot = F)[1:2, 2]
  })
  
  w_p <- c(N_pos = freq(x = KoCo_BLab$R3_Result_imp, w = KoCo_BLab$w_ind_cal, plot=F)[2, 2],
           S_pos = freq(x = KoCo_BLab$R3_Result_S1_imp, w = KoCo_BLab$w_ind_cal, plot=F)[2, 2],
           S_N_pos = freq(x = KoCo_BLab$R3_S1_N, w = KoCo_BLab_group$w_ind_cal, plot=F)[2, 2],
           Negative_Positive = w_p[2], Positive_Positive = w_p[4],
           Non_vax = w_p[6], Vax = w_p[8],
           S1_neg = w_p[10], S1_pos = w_p[12])
  
  # N_pos: % of ppl infected by Corona
  # S_pos: % of ppl with S positive
  # S_N_pos: % of ppl with S or N positive
  # Negative_Positive: sero-incidence
  # Positive_Positive: sero-persistence
  # Non_vax: sero-prevalence among the non vaccinated persons
  # Vax: sero-prevalence among the vaccinated persons
  # S1_neg: % of ppl infected by Corona among the S1 neg
  # S1_pos: % of ppl infected by Corona among the S1 pos
  
  
  V <- V1_dbs + V2_dbs
  
  # Upper bound CI
  w_ub <- w_p + qnorm(0.975) * sqrt(V) * 100
  
  # Lower bound CI
  w_lb <- w_p - qnorm(0.975) * sqrt(V) * 100
  
  res_w <- cbind(estimate = w_p, lb_ci = w_lb, ub_ci = w_ub)
  
  
  
  ###
  # Adjust for specificity and sensitivity
  ###
  
  spec_sens_classifier <- readRDS(file = here_koco_data("R1/specificity_sensitivity_classifier.RData"))
  
  res_adj <- (res_w/100 + spec_sens_classifier["Roche N pan-Ig optimized cut-off", "Specificity"] - 1)/(spec_sens_classifier["Roche N pan-Ig optimized cut-off", "Sensitivity"] + spec_sens_classifier["Roche N pan-Ig optimized cut-off", "Specificity"] - 1)*100
  
  
  
  ###
  # Save results
  ###
  
  res_all <- data.frame(Adjust = c(rep("unadjusted", 9), rep("adjusted", 7)),
                        Calculation = c(rownames(res_w), rownames(res_w)[-(2:3)]),
                        rbind(res_w, res_adj[-(2:3), ]))
  
  ###
  # Adjusted % of positive S1 or N
  ###
  
  # Size of the population
  nb_ppl <- sum(KoCo_BLab$w_ind_cal)
  
  # Nb S1 positive
  nb_S_pos <- nb_ppl * res_all["S_pos", c("estimate", "lb_ci", "ub_ci")]/100
  
  # Adj. nb N positive among S1 negative
  adj_nb_N_pos_S_neg <- (nb_ppl - nb_S_pos) * res_all["S1_neg.1", c("estimate", "lb_ci", "ub_ci")]/100
  
  # Adjusted % of positive S1 or N positive
  res_all <- rbind(res_all, c(Adjust = "adjusted", Calculation = "S_N_pos", (nb_S_pos + adj_nb_N_pos_S_neg) / nb_ppl*100))
  
  write.csv(res_all, paste0(here_koco_results("R3"), "/r3_cum_sp_w_seed", seed, ".csv"), row.names = FALSE)
  
  
}

