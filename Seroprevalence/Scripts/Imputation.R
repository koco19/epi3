#' This script imputes missing values for the vaccination status of round R and 
#' the serology results (S1 and N) of round R-1.

impute <- function(seed){
  
  here_koco_data = function (...) here::here("Data", ...)
  here_koco_prev = function (...) here::here("Seroprevalence", ...)
  here_prev_figures = function (...) here_koco_prev("Figures", ...)
  here_prev_results = function (...) here_koco_prev("Results", ...)
  here_prev_scripts = function (...) here_koco_prev("Scripts", ...)
  
  KoCo_BLab <- readRDS(here_koco_data("R5/R5_CompleteData_NC.RDS"))
  
  ###
  # Define variables ever positive
  ###
  
  # If the person was positive in the past, we still consider him/her as positive
  
  # Create new variables for imputation
  KoCo_BLab$R2_Result_imp <- KoCo_BLab$R2_Result
  KoCo_BLab$R3_Result_imp <- KoCo_BLab$R3_Result
  KoCo_BLab$R4_Result_imp <- KoCo_BLab$R4_Result
  KoCo_BLab$R5_Result_imp <- KoCo_BLab$R5_Result
  
  
  # Positive round 1 -> positive round 2
  KoCo_BLab$R2_Result_imp[KoCo_BLab$R1_Result == "Positive"] <- "Positive"
  
  # Positive round 1 or 2 -> positive round 3
  KoCo_BLab$R3_Result_imp[KoCo_BLab$R2_Result_imp == "Positive"] <- "Positive"
  
  # Positive round 1, 2 or 3 -> positive round 4
  KoCo_BLab$R4_Result_imp[KoCo_BLab$R3_Result_imp == "Positive"] <- "Positive"
  
  # Positive round 1, 2, 3 or 4 -> positive round 5
  KoCo_BLab$R5_Result_imp[KoCo_BLab$R4_Result_imp == "Positive"] <- "Positive"
  
  ###
  # Impute S1 and vaccination status using the information of Rounds 3, 4 and 5
  ###
  
  ### S1
  
  # R3
  KoCo_BLab$R3_Result_S1_imp <- ifelse(KoCo_BLab$R3_Result_S1 == "Positive", "Positive", "Negative")
  
  # Impute R4 with R3
  KoCo_BLab$R4_Result_S1_imp <- ifelse(KoCo_BLab$R4_Result_S1 == "Positive", "Positive", "Negative")
  
  KoCo_BLab$R4_Result_S1_imp[is.na(KoCo_BLab$R4_Result_S1_imp) & 
                               KoCo_BLab$R3_Result_S1_imp == "Positive"] <- "Positive"
  
  table(KoCo_BLab$R4_Result_S1_imp, useNA = "always")
  
  # Impute R5 with R4 and R3
  KoCo_BLab$R5_Result_S1_imp <- ifelse(KoCo_BLab$R5_Result_S1 == "Positive", "Positive", "Negative")
  
  KoCo_BLab$R5_Result_S1_imp[is.na(KoCo_BLab$R5_Result_S1_imp) & 
                               (KoCo_BLab$R3_Result_S1_imp == "Positive" |
                                  KoCo_BLab$R4_Result_S1_imp == "Positive")] <- "Positive"
  
  table(KoCo_BLab$R5_Result_S1_imp, useNA = "always")
  
  ### Vaccination
  
  # R3
  KoCo_BLab$R3_Vaccination <- ifelse(KoCo_BLab$R3_Vac == "Vaccinated Atleast Once", "Positive", "Negative")
  
  KoCo_BLab$R3_Vaccination[(is.na(KoCo_BLab$R3_Vac) | KoCo_BLab$R3_Vac == "Unknown")] <- "Unknown"
  
  table(KoCo_BLab$R3_Vaccination, useNA = "always")
  
  
  # Impute R4 with R3
  KoCo_BLab$R4_Vaccination <- ifelse(KoCo_BLab$R4_Vac %in% c("Vaccinated Once", "Vaccinated Twice"), "Positive", "Negative")
  
  KoCo_BLab$R4_Vaccination[(is.na(KoCo_BLab$R4_Vac) | KoCo_BLab$R4_Vac == "Unknown")] <- "Unknown"
  
  
  KoCo_BLab$R4_Vaccination[KoCo_BLab$R4_Vaccination == "Unknown" & 
                             KoCo_BLab$R3_Vaccination == "Positive"] <- "Positive"
  
  table(KoCo_BLab$R4_Vaccination, useNA = "always")
  

  # Impute R5 with R3 and R4
  KoCo_BLab$R5_Vaccination <- ifelse(KoCo_BLab$R5_Vac == "Yes", "Positive", "Negative")
  
  KoCo_BLab$R5_Vaccination[is.na(KoCo_BLab$R5_Vac)] <- "Unknown"
  
  
  KoCo_BLab$R5_Vaccination[KoCo_BLab$R5_Vaccination == "Unknown" & 
                             KoCo_BLab$R3_Vaccination == "Positive" | KoCo_BLab$R4_Vaccination == "Positive"] <- "Positive"
  
  table(KoCo_BLab$R5_Vaccination, useNA = "always")
  
  
  # Impute R3 with R4 and R5
  
  KoCo_BLab$R3_Vaccination[KoCo_BLab$R3_Vaccination == "Unknown" &
                             (KoCo_BLab$R4_Vaccination == "Negative" |
                                KoCo_BLab$R5_Vaccination == "Negative")] <- "Negative"
  
  table(KoCo_BLab$R3_Vaccination, useNA = "always")
  
  # Impute R4 with R5
  
  KoCo_BLab$R4_Vaccination[KoCo_BLab$R4_Vaccination == "Unknown" &
                             KoCo_BLab$R5_Vaccination == "Negative"] <- "Negative"
  
  table(KoCo_BLab$R4_Vaccination, useNA = "always")
  
  ###
  # Impute N for R2 (to define who was positive/negative in the past for sero-incidence R3)
  ###
  
  KoCo_BLab$R2_Result_new <- KoCo_BLab$R2_Result_imp
  
  res <- table(KoCo_BLab$R2_Result_imp, KoCo_BLab$R3_Result_imp,
               KoCo_BLab$R1_Result, 
               useNA = "always")
  
  # Participants with N/NA/N
  set.seed(seed)
  imp <- rbinom(res[3], 1, res[2]/(res[2]+res[1]))
  
  KoCo_BLab$R2_Result_new[KoCo_BLab$R1_Result == "Negative" &
                            KoCo_BLab$R3_Result_imp == "Negative" &
                            !is.na(KoCo_BLab$R3_Result_imp) &
                            is.na(KoCo_BLab$R2_Result_new)] <- ifelse(imp == 1, "Positive", "Negative")
  
  # Participants with N/NA/P
  set.seed(seed)
  imp <- rbinom(res[6], 1, res[5]/(res[5]+res[4]))
  
  KoCo_BLab$R2_Result_new[KoCo_BLab$R1_Result == "Negative" &
                            KoCo_BLab$R3_Result_imp == "Positive" &
                            !is.na(KoCo_BLab$R3_Result_imp) &
                            is.na(KoCo_BLab$R2_Result_new)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  ###
  # Impute N for R3 (to define who was positive/negative in the past for sero-incidence R4)
  ###
  
  KoCo_BLab$R3_Result_new <- KoCo_BLab$R3_Result_imp
  
  res <- table(KoCo_BLab$R3_Result_imp, KoCo_BLab$R4_Result_imp, KoCo_BLab$R2_Result_imp,
               useNA = "always")
  
  # Participants with N/NA/N
  set.seed(seed)
  imp <- rbinom(res[3], 1, res[2]/(res[2]+res[1]))
  
  KoCo_BLab$R3_Result_new[KoCo_BLab$R2_Result_imp == "Negative" &
                            !is.na(KoCo_BLab$R2_Result_imp) &
                            KoCo_BLab$R4_Result_imp == "Negative" &
                            !is.na(KoCo_BLab$R4_Result_imp) &
                            is.na(KoCo_BLab$R3_Result_new)] <- ifelse(imp == 1, "Positive", "Negative")
  
  # Participants with N/NA/P
  set.seed(seed)
  imp <- rbinom(res[6], 1, res[5]/(res[5]+res[4]))
  
  KoCo_BLab$R3_Result_new[KoCo_BLab$R2_Result_imp == "Negative" &
                            !is.na(KoCo_BLab$R2_Result_imp) &
                            KoCo_BLab$R4_Result_imp == "Positive" &
                            !is.na(KoCo_BLab$R4_Result_imp) &
                            is.na(KoCo_BLab$R3_Result_new)] <- ifelse(imp == 1, "Positive", "Negative")
  
  # Participants with NA/NA/N
  set.seed(seed)
  imp <- rbinom(res[21], 1, res[20]/(res[20]+res[19]))
  
  KoCo_BLab$R3_Result_new[is.na(KoCo_BLab$R2_Result_imp) &
                            KoCo_BLab$R4_Result_imp == "Negative" &
                            !is.na(KoCo_BLab$R4_Result_imp) &
                            is.na(KoCo_BLab$R3_Result_new)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  # Participants with NA/NA/P
  set.seed(seed)
  imp <- rbinom(res[24], 1, res[23]/(res[23]+res[22]))
  
  KoCo_BLab$R3_Result_new[is.na(KoCo_BLab$R2_Result_imp) &
                            KoCo_BLab$R4_Result_imp == "Positive" &
                            !is.na(KoCo_BLab$R4_Result_imp) &
                            is.na(KoCo_BLab$R3_Result_new)] <- ifelse(imp == 1, "Positive", "Negative")
  
  ###
  # Impute N for R4 (to define who was positive/negative in the past for sero-incidence R5)
  ###
  
  KoCo_BLab$R4_Result_new <- KoCo_BLab$R4_Result_imp
  
  res <- table(KoCo_BLab$R4_Result_imp, KoCo_BLab$R5_Result_imp, KoCo_BLab$R3_Result_imp,
               useNA = "always")
  
  # Participants with N/NA/N
  set.seed(seed)
  imp <- rbinom(res[3], 1, res[2]/(res[2]+res[1]))
  
  KoCo_BLab$R4_Result_new[KoCo_BLab$R3_Result_imp == "Negative" &
                            !is.na(KoCo_BLab$R3_Result_imp) &
                            KoCo_BLab$R5_Result_imp == "Negative" &
                            !is.na(KoCo_BLab$R5_Result_imp) &
                            is.na(KoCo_BLab$R4_Result_new)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  
  # Participants with N/NA/P
  set.seed(seed)
  imp <- rbinom(res[6], 1, res[5]/(res[5]+res[4]))
  
  KoCo_BLab$R4_Result_new[KoCo_BLab$R3_Result_imp == "Negative" &
                            !is.na(KoCo_BLab$R3_Result_imp) &
                            KoCo_BLab$R5_Result_imp == "Positive" &
                            !is.na(KoCo_BLab$R5_Result_imp) &
                            is.na(KoCo_BLab$R4_Result_new)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  # Participants with NA/NA/N
  set.seed(seed)
  imp <- rbinom(res[21], 1, res[20]/(res[20]+res[19]))
  
  KoCo_BLab$R4_Result_new[is.na(KoCo_BLab$R3_Result_imp) &
                            KoCo_BLab$R5_Result_imp == "Negative" &
                            !is.na(KoCo_BLab$R5_Result_imp) &
                            is.na(KoCo_BLab$R4_Result_new)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  # Participants with NA/NA/P
  set.seed(seed)
  imp <- rbinom(res[24], 1, res[23]/(res[23]+res[22]))
  
  KoCo_BLab$R4_Result_new[is.na(KoCo_BLab$R3_Result_imp) &
                            KoCo_BLab$R5_Result_imp == "Positive" &
                            !is.na(KoCo_BLab$R5_Result_imp) &
                            is.na(KoCo_BLab$R4_Result_new)] <- ifelse(imp == 1, "Positive", "Negative")
  
  ###
  # Impute S1 for R3
  ###
  
  res <- table(KoCo_BLab$R3_Result_S1_imp, KoCo_BLab$R3_Vaccination, KoCo_BLab$R3_Result_new, useNA = "always")
  
  
  # participants with unknown vaccination status and unknown R3_Result_S1 and R3_Result negative
  
  set.seed(seed)
  imp <- rbinom(res[9], 1, res[8]/(res[8]+res[7]))
  
  KoCo_BLab$R3_Result_S1_imp[KoCo_BLab$R3_Vaccination == "Unknown" &
                               KoCo_BLab$R3_Result_new == "Negative" &
                               !is.na(KoCo_BLab$R3_Result_new) &
                               is.na(KoCo_BLab$R3_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  # participants vaccinated with unknown R3_Result_S1 and R3_Result negative
  
  set.seed(seed)
  imp <- rbinom(res[6], 1, res[5]/(res[5]+res[4]))
  
  KoCo_BLab$R3_Result_S1_imp[KoCo_BLab$R3_Vaccination == "Positive" &
                               KoCo_BLab$R3_Result_new == "Negative" &
                               !is.na(KoCo_BLab$R3_Result_new) &
                               is.na(KoCo_BLab$R3_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  # participants not vaccinated with unknown R3_Result_S1 and R3_Result negative
  
  set.seed(seed)
  imp <- rbinom(res[3], 1, res[2]/(res[2]+res[1]))
  
  
  KoCo_BLab$R3_Result_S1_imp[KoCo_BLab$R3_Vaccination == "Negative" &
                               KoCo_BLab$R3_Result_new == "Negative" &
                               !is.na(KoCo_BLab$R3_Result_new) &
                               is.na(KoCo_BLab$R3_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  # participants with unknown vaccination status and unknown R3_Result_S1 and R3_Result positive
  
  set.seed(seed)
  imp <- rbinom(res[21], 1, res[20]/(res[20]+res[19]))
  
  KoCo_BLab$R3_Result_S1_imp[KoCo_BLab$R3_Vaccination == "Unknown" &
                               KoCo_BLab$R3_Result_new == "Positive" &
                               !is.na(KoCo_BLab$R3_Result_new) &
                               is.na(KoCo_BLab$R3_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  
  # participants vaccinated with unknown R3_Result_S1 and R3_Result positive
  
  set.seed(seed)
  imp <- rbinom(res[18], 1, res[17]/(res[17]+res[16]))
  
  
  KoCo_BLab$R3_Result_S1_imp[KoCo_BLab$R3_Vaccination == "Positive" &
                               KoCo_BLab$R3_Result_new == "Positive" &
                               !is.na(KoCo_BLab$R3_Result_new) &
                               is.na(KoCo_BLab$R3_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  # participants not vaccinated with unknown R3_Result_S1 and R3_Result positive
  
  set.seed(seed)
  imp <- rbinom(res[15], 1, res[14]/(res[14]+res[13]))
  
  
  KoCo_BLab$R3_Result_S1_imp[KoCo_BLab$R3_Vaccination == "Negative" &
                               KoCo_BLab$R3_Result_new == "Positive" &
                               !is.na(KoCo_BLab$R3_Result_new) &
                               is.na(KoCo_BLab$R3_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  ###
  # Impute S1 for R4
  ###
  
  res <- table(KoCo_BLab$R4_Result_S1_imp, KoCo_BLab$R4_Vaccination, KoCo_BLab$R4_Result_new, useNA = "always")
  
  
  # participants with unknown vaccination status and unknown R4_Result_S1 and R4_Result negative
  
  set.seed(seed)
  imp <- rbinom(res[9], 1, res[8]/(res[8]+res[7]))
  
  KoCo_BLab$R4_Result_S1_imp[KoCo_BLab$R4_Vaccination == "Unknown" &
                               KoCo_BLab$R4_Result_new == "Negative" &
                               !is.na(KoCo_BLab$R4_Result_new) &
                               is.na(KoCo_BLab$R4_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  # participants vaccinated with unknown R4_Result_S1 and R4_Result negative
  
  set.seed(seed)
  imp <- rbinom(res[6], 1, res[5]/(res[5]+res[4]))
  
  KoCo_BLab$R4_Result_S1_imp[KoCo_BLab$R4_Vaccination == "Positive" &
                               KoCo_BLab$R4_Result_new == "Negative" &
                               !is.na(KoCo_BLab$R4_Result_new) &
                               is.na(KoCo_BLab$R4_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  # participants not vaccinated with unknown R4_Result_S1 and R4_Result negative
  
  set.seed(seed)
  imp <- rbinom(res[3], 1, res[2]/(res[2]+res[1]))
  
  KoCo_BLab$R4_Result_S1_imp[KoCo_BLab$R4_Vaccination == "Negative" &
                               KoCo_BLab$R4_Result_new == "Negative" &
                               !is.na(KoCo_BLab$R4_Result_new) &
                               is.na(KoCo_BLab$R4_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  # participants with unknown vaccination status and unknown R4_Result_S1 and R4_Result positive
  
  set.seed(seed)
  imp <- rbinom(res[21], 1, res[20]/(res[20]+res[19]))
  
  KoCo_BLab$R4_Result_S1_imp[KoCo_BLab$R4_Vaccination == "Unknown" &
                               KoCo_BLab$R4_Result_new == "Positive" &
                               !is.na(KoCo_BLab$R4_Result_new) &
                               is.na(KoCo_BLab$R4_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  # participants vaccinated at least once and unknown R4_Result_S1 and R4_Result positive
  
  set.seed(seed)
  imp <- rbinom(res[18], 1, res[17]/(res[17]+res[16]))
  
  KoCo_BLab$R4_Result_S1_imp[KoCo_BLab$R4_Vaccination == "Positive" &
                               KoCo_BLab$R4_Result_new == "Positive" &
                               !is.na(KoCo_BLab$R4_Result_new) &
                               is.na(KoCo_BLab$R4_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  # participants not vaccinated at least once and unknown R4_Result_S1 and R4_Result positive
  
  set.seed(seed)
  imp <- rbinom(res[15], 1, res[14]/(res[14]+res[13]))
  
  KoCo_BLab$R4_Result_S1_imp[KoCo_BLab$R4_Vaccination == "Negative" &
                               KoCo_BLab$R4_Result_new == "Positive" &
                               !is.na(KoCo_BLab$R4_Result_new) &
                               is.na(KoCo_BLab$R4_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  ###
  # Impute S1 for R5
  ###
  
  res <- table(KoCo_BLab$R5_Result_S1_imp, KoCo_BLab$R5_Vaccination, KoCo_BLab$R5_Result_imp, useNA = "always")
  
  
  # participants with unknown vaccination status and unknown R5_Result_S1 and R5_Result negative
  
  set.seed(seed)
  imp <- rbinom(res[9], 1, res[8]/(res[8]+res[7]))
  
  KoCo_BLab$R5_Result_S1_imp[KoCo_BLab$R5_Vaccination == "Unknown" &
                               KoCo_BLab$R5_Result_imp == "Negative" &
                               !is.na(KoCo_BLab$R5_Result_imp) &
                               is.na(KoCo_BLab$R5_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  # participants vaccinated with unknown R5_Result_S1 and R5_Result negative
  
  set.seed(seed)
  imp <- rbinom(res[6], 1, res[5]/(res[5]+res[4]))
  
  KoCo_BLab$R5_Result_S1_imp[KoCo_BLab$R5_Vaccination == "Positive" &
                               KoCo_BLab$R5_Result_imp == "Negative" &
                               !is.na(KoCo_BLab$R5_Result_imp) &
                               is.na(KoCo_BLab$R5_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  # participants not vaccinated with unknown R5_Result_S1 and R5_Result negative
  
  set.seed(seed)
  imp <- rbinom(res[3], 1, res[2]/(res[2]+res[1]))
  
  KoCo_BLab$R5_Result_S1_imp[KoCo_BLab$R5_Vaccination == "Negative" &
                               KoCo_BLab$R5_Result_imp == "Negative" &
                               !is.na(KoCo_BLab$R5_Result_imp) &
                               is.na(KoCo_BLab$R5_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  # participants with unknown vaccination status and unknown R5_Result_S1 and R5_Result positive
  
  set.seed(seed)
  imp <- rbinom(res[21], 1, res[20]/(res[20]+res[19]))
  
  KoCo_BLab$R5_Result_S1_imp[KoCo_BLab$R5_Vaccination == "Unknown" &
                               KoCo_BLab$R5_Result_imp == "Positive" &
                               !is.na(KoCo_BLab$R5_Result_imp) &
                               is.na(KoCo_BLab$R5_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  # participants vaccinated at least once and unknown R5_Result_S1 and R5_Result positive
  
  set.seed(seed)
  imp <- rbinom(res[18], 1, res[17]/(res[17]+res[16]))
  
  KoCo_BLab$R5_Result_S1_imp[KoCo_BLab$R5_Vaccination == "Positive" &
                               KoCo_BLab$R5_Result_imp == "Positive" &
                               !is.na(KoCo_BLab$R5_Result_imp) &
                               is.na(KoCo_BLab$R5_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  # participants not vaccinated at least once and unknown R5_Result_S1 and R5_Result positive
  
  set.seed(seed)
  imp <- rbinom(res[15], 1, res[14]/(res[14]+res[13]))
  
  KoCo_BLab$R5_Result_S1_imp[KoCo_BLab$R5_Vaccination == "Negative" &
                               KoCo_BLab$R5_Result_imp == "Positive" &
                               !is.na(KoCo_BLab$R5_Result_imp) &
                               is.na(KoCo_BLab$R5_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  ###
  # Impute vaccination for R3
  ###
  
  res <- table(KoCo_BLab$R3_Result_S1_imp, KoCo_BLab$R3_Vaccination, KoCo_BLab$R3_Result_new, useNA = "always")
  
  
  # participants with unknown vaccination status and R3_Result_S1 positive and R3_Result negative
  
  set.seed(seed)
  imp <- rbinom(res[8], 1, res[5]/(res[5]+res[2]))
  
  
  KoCo_BLab$R3_Vaccination[(KoCo_BLab$R3_Vaccination == "Unknown" | is.na(KoCo_BLab$R3_Vaccination)) &
                             KoCo_BLab$R3_Result_new == "Negative" &
                             !is.na(KoCo_BLab$R3_Result_new) &
                             KoCo_BLab$R3_Result_S1_imp == "Positive" &
                             !is.na(KoCo_BLab$R3_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  # participants with unknown vaccination status and R3_Result_S1 negative and R3_Result negative
  
  set.seed(seed)
  imp <- rbinom(res[7], 1, res[4]/(res[4]+res[1]))
  
  KoCo_BLab$R3_Vaccination[(KoCo_BLab$R3_Vaccination == "Unknown" | is.na(KoCo_BLab$R3_Vaccination)) &
                             KoCo_BLab$R3_Result_new == "Negative" &
                             !is.na(KoCo_BLab$R3_Result_new) &
                             KoCo_BLab$R3_Result_S1_imp == "Negative" &
                             !is.na(KoCo_BLab$R3_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  # participants with unknown vaccination status and R3_Result_S1 positive and R3_Result positive
  
  set.seed(seed)
  imp <- rbinom(res[20], 1, res[17]/(res[17]+res[14]))
  
  
  KoCo_BLab$R3_Vaccination[(KoCo_BLab$R3_Vaccination == "Unknown" | is.na(KoCo_BLab$R3_Vaccination)) &
                             KoCo_BLab$R3_Result_new == "Positive" &
                             !is.na(KoCo_BLab$R3_Result_new) &
                             KoCo_BLab$R3_Result_S1_imp == "Positive" &
                             !is.na(KoCo_BLab$R3_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  # participants with unknown vaccination status and R3_Result_S1 negative and R3_Result positive
  
  set.seed(seed)
  imp <- rbinom(res[19], 1, res[16]/(res[16]+res[13]))
  
  
  KoCo_BLab$R3_Vaccination[(KoCo_BLab$R3_Vaccination == "Unknown" | is.na(KoCo_BLab$R3_Vaccination)) &
                             KoCo_BLab$R3_Result_new == "Positive" &
                             !is.na(KoCo_BLab$R3_Result_new) &
                             KoCo_BLab$R3_Result_S1_imp == "Negative" &
                             !is.na(KoCo_BLab$R3_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  ###
  # Impute vaccination for R4
  ###
  
  res <- table(KoCo_BLab$R4_Result_S1_imp, KoCo_BLab$R4_Vaccination, KoCo_BLab$R4_Result_new, useNA = "always")
  
  
  # participants with unknown vaccination status and R4_Result_S1 positive and R4_Result negative
  
  set.seed(seed)
  imp <- rbinom(res[8], 1, res[5]/(res[5]+res[2]))
  
  
  KoCo_BLab$R4_Vaccination[(KoCo_BLab$R4_Vaccination == "Unknown" | is.na(KoCo_BLab$R4_Vaccination)) &
                             KoCo_BLab$R4_Result_new == "Negative" &
                             !is.na(KoCo_BLab$R4_Result_new) &
                             KoCo_BLab$R4_Result_S1_imp == "Positive" &
                             !is.na(KoCo_BLab$R4_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  # participants with unknown vaccination status and R4_Result_S1 negative and R4_Result negative
  
  set.seed(seed)
  imp <- rbinom(res[7], 1, res[4]/(res[4]+res[1]))
  
  
  KoCo_BLab$R4_Vaccination[(KoCo_BLab$R4_Vaccination == "Unknown" | is.na(KoCo_BLab$R4_Vaccination)) &
                             KoCo_BLab$R4_Result_new == "Negative" &
                             !is.na(KoCo_BLab$R4_Result_new) &
                             KoCo_BLab$R4_Result_S1_imp == "Negative" &
                             !is.na(KoCo_BLab$R4_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  
  # participants with unknown vaccination status and R4_Result_S1 positive and R4_Result positive
  
  set.seed(seed)
  imp <- rbinom(res[20], 1, res[17]/(res[17]+res[14]))
  
  KoCo_BLab$R4_Vaccination[(KoCo_BLab$R4_Vaccination == "Unknown" | is.na(KoCo_BLab$R4_Vaccination)) &
                             KoCo_BLab$R4_Result_new == "Positive" &
                             !is.na(KoCo_BLab$R4_Result_new) &
                             KoCo_BLab$R4_Result_S1_imp == "Positive" &
                             !is.na(KoCo_BLab$R4_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  
  # participants with unknown vaccination status and R4_Result_S1 negative and R4_Result positive
  
  set.seed(seed)
  imp <- rbinom(res[19], 1, res[16]/(res[16]+res[13]))
  
  KoCo_BLab$R4_Vaccination[(KoCo_BLab$R4_Vaccination == "Unknown" | is.na(KoCo_BLab$R4_Vaccination)) &
                             KoCo_BLab$R4_Result_new == "Positive" &
                             !is.na(KoCo_BLab$R4_Result_new) &
                             KoCo_BLab$R4_Result_S1_imp == "Negative" &
                             !is.na(KoCo_BLab$R4_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  ###
  # Impute vaccination for R5
  ###
  
  res <- table(KoCo_BLab$R5_Result_S1_imp, KoCo_BLab$R5_Vaccination, KoCo_BLab$R5_Result_imp, useNA = "always")
  
  
  # participants with unknown vaccination status and R5_Result_S1 positive and R5_Result negative
  
  set.seed(seed)
  imp <- rbinom(res[8], 1, res[5]/(res[5]+res[2]))
  
  
  KoCo_BLab$R5_Vaccination[(KoCo_BLab$R5_Vaccination == "Unknown" | is.na(KoCo_BLab$R5_Vaccination)) &
                             KoCo_BLab$R5_Result_imp == "Negative" &
                             !is.na(KoCo_BLab$R5_Result_imp) &
                             KoCo_BLab$R5_Result_S1_imp == "Positive" &
                             !is.na(KoCo_BLab$R5_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  # participants with unknown vaccination status and R5_Result_S1 negative and R5_Result negative
  
  set.seed(seed)
  imp <- rbinom(res[7], 1, res[4]/(res[4]+res[1]))
  
  
  KoCo_BLab$R5_Vaccination[(KoCo_BLab$R5_Vaccination == "Unknown" | is.na(KoCo_BLab$R5_Vaccination)) &
                             KoCo_BLab$R5_Result_imp == "Negative" &
                             !is.na(KoCo_BLab$R5_Result_imp) &
                             KoCo_BLab$R5_Result_S1_imp == "Negative" &
                             !is.na(KoCo_BLab$R5_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  
  # participants with unknown vaccination status and R5_Result_S1 positive and R5_Result positive
  
  set.seed(seed)
  imp <- rbinom(res[20], 1, res[17]/(res[17]+res[14]))
  
  KoCo_BLab$R5_Vaccination[(KoCo_BLab$R5_Vaccination == "Unknown" | is.na(KoCo_BLab$R5_Vaccination)) &
                             KoCo_BLab$R5_Result_imp == "Positive" &
                             !is.na(KoCo_BLab$R5_Result_imp) &
                             KoCo_BLab$R5_Result_S1_imp == "Positive" &
                             !is.na(KoCo_BLab$R5_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  
  # participants with unknown vaccination status and R5_Result_S1 negative and R5_Result positive
  
  set.seed(seed)
  imp <- rbinom(res[19], 1, res[16]/(res[16]+res[13]))
  
  KoCo_BLab$R5_Vaccination[(KoCo_BLab$R5_Vaccination == "Unknown" | is.na(KoCo_BLab$R5_Vaccination)) &
                             KoCo_BLab$R5_Result_imp == "Positive" &
                             !is.na(KoCo_BLab$R5_Result_imp) &
                             KoCo_BLab$R5_Result_S1_imp == "Negative" &
                             !is.na(KoCo_BLab$R5_Result_S1_imp)] <- ifelse(imp == 1, "Positive", "Negative")
  
  
  ###
  # Save data set
  ###
  
  saveRDS(KoCo_BLab, paste0(here_koco_data("Imputed_Data"), "/Koco_R5latest_data_seed", seed, ".RDS"))
  
}

