#' This script computes the unweighted seroprevalence estimates + 95% Bootstrap CI 
#' for rounds 1, 2, 3, 4 and 5.

run_sp_boot <- function(boot = NULL){
 
  # boot = number of bootstrap samples for cluster bootstrap
  
  here_koco_data = function (...) here::here("Data", ...)
  here_koco_prev = function (...) here::here("Seroprevalence", ...)
  here_prev_figures = function (...) here_koco_prev("Figures", ...)
  here_prev_results = function (...) here_koco_prev("Results", ...)
  here_prev_scripts = function (...) here_koco_prev("Scripts", ...)
  
  ########
  # Data #
  ########
  
  
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
  # unweighted seroprevalence with bootstrap CI
  ###
  
  # test performance (from Olbricht et al, 2020)
  specificity <- 0.9972041
  sensitivity <- 0.8860104
  
  # calculate lower and upper bounds for each round
  
  prev <- sapply(c("R1_Result", "R2_Result_imp", "R3_Result_imp", "R4_Result_imp", "R5_Result_imp"), function(x){
    
    data <- KoCo_BLab
    
    # create data set without missing values
    data_full <- na.omit(data[, c("hh_id", x)])
    
    # seroprevalence
    sp_crude <- prop.table(table(data[, x]))[2]
    
    ## cluster bootstrap for confidence intervals
    
    data_full[, x] <- ifelse(data_full[, x] == "Positive", 1, 0)
    
    # placeholder for bootstrap estimates
    pos_rate_clu <- numeric(boot)
    
    # cluster by household id
    data_full <- as.data.table(data_full)
    setkey(data_full, "hh_id")
    
    # store number of households
    n_hh <- length(unique(data_full$hh_id))
    
    # set seed for replicability
    set.seed(22)
    
    # start loop
    for (i in 1:boot)
    {
      # create the bootstrap dataset by resampling clusters
      samp <- sample(unique(data_full$hh_id), n_hh, replace = TRUE)
      bootdat <- data_full[J(samp), allow.cartesian = TRUE]
      # calculate bootstrap estimate
      pos_rate_clu[i] <- mean(bootdat[[x]] == 1)
    }
    # results: point estimate and 95% percentile intervals
    c(mean(pos_rate_clu), quantile(pos_rate_clu, probs = c(0.025, 0.975)))
    
    # save results: crude...
    prev <- tibble(
      Prevalence = c("Mean", "Lower Bound", "Upper Bound"),
      crude = c(sp_crude, quantile(pos_rate_clu, probs = c(0.025, 0.975)))
    ) %>%
      #...and adjusted prevalence
      mutate(adjusted = (crude + specificity - 1) / (specificity + sensitivity - 1))
  })
  
  # extract results
  res <- as.data.frame(do.call(rbind, prev[-c(1, 4, 7, 10, 13)]))
  
  colnames(res) <- c("estimate", "lb_ci", "ub_ci")
  
  # add adjustment
  res$Adjust <- rep(c("unadjusted", "adjusted"), 5)
  
  # add rounds
  res$round <- rep(c("R1", "R2", "R3", "R4", "R5"), each = 2)
  
  ###
  # Save Results
  ###
  
  saveRDS(res, here_prev_results("sp_uw.RDS"))
  
   
}
  

