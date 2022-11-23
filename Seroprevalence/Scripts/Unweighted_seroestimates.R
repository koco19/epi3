#' This script computes the unweighted seroincidence and 
#' seroprevalence + seroincidence estimates
#' for vaccinated and non-vaccinated + 95% Bootstrap CI 
#' for rounds 2, 3, 4 and 5.

run_boot <- function(seed, boot = NULL){
  
  # boot = number of bootstrap samples for cluster bootstrap
  
  here_koco_data = function (...) here::here("Data", ...)
  here_koco_prev = function (...) here::here("Seroprevalence", ...)
  here_prev_figures = function (...) here_koco_prev("Figures", ...)
  here_prev_results = function (...) here_koco_prev("Results", ...)
  here_prev_scripts = function (...) here_koco_prev("Scripts", ...)
  
  
  ########
  # Data #
  ########
  

  KoCo_BLab <- readRDS(paste0(here_koco_data("Imputed_Data"), "/Koco_R5latest_data_seed", seed, ".RDS"))
  
  
  ###
  # unweighted seroincidence with bootstrap CI
  ###
  
  prev_neg <- sapply(c("R2_Result_imp", "R3_Result_imp", "R4_Result_imp", "R5_Result_imp"), function(x){
    
    data <- KoCo_BLab
    
    # define variable for the previous round
    y <- x
    y <- str_replace(y, substr(x, 2, 2), as.character(as.numeric(substr(x, 2, 2))-1))
    y <- str_replace(y, "imp", "new")
    if(substr(y, 2, 2) == "1") y <- substr(y, 1, 9)
    
    filt <- data[, y] == "Negative" & !is.na(data[, x])
    
    # create data set without missing values
    data_full <- na.omit(data[filt, c("hh_id", x)])
    
    # seroincidence
    si_crude <- prop.table(table(data[filt, x]))[2]
    
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
      Incidence = c("Mean", "Variance"),
      crude = c(si_crude, var(pos_rate_clu))
    )
  })
  
  
  # extract results
  res <- as.data.frame(do.call(rbind, prev_neg[-seq(1, 8, 2)]))
  
  colnames(res) <- c("estimate", "variance")
  
  
  # add rounds
  res$round <- c("R2", "R3", "R4", "R5")
  
  
  ###
  # Save Results
  ###
  
  saveRDS(res, paste0(here_prev_results("Unweighted"), "/si_uw_seed", seed, ".RDS"))
  
  ###
  # unweighted seroprevalence for vaccinated and non vaccinated with bootstrap CI
  ###
  
  prev_vax <- sapply(c("R3_Result_imp", "R4_Result_imp", "R5_Result_imp"), function(x){
    
    data <- KoCo_BLab
    
    # define variable for the vaccination
    y <- paste0(substr(x, 1, 3), "Vaccination")
    
    
    # create data set without missing values
    data_full <- na.omit(data[, c("hh_id", x, y)])
    
    data_full <- split(data_full, data_full[, y])
    
    prev <- lapply(data_full, function(z){
      
      # seroprevalence
      sp_crude <- prop.table(table(z[, x]))[2]
      
      ## cluster bootstrap for confidence intervals
      
      z[, x] <- ifelse(z[, x] == "Positive", 1, 0)
      
      # placeholder for bootstrap estimates
      pos_rate_clu <- numeric(boot)
      
      # cluster by household id
      z <- as.data.table(z)
      setkey(z, "hh_id")
      
      # store number of households
      n_hh <- length(unique(z$hh_id))
      
      # set seed for replicability
      set.seed(22)
      
      # start loop
      for (i in 1:boot)
      {
        # create the bootstrap dataset by resampling clusters
        samp <- sample(unique(z$hh_id), n_hh, replace = TRUE)
        bootdat <- z[J(samp), allow.cartesian = TRUE]
        # calculate bootstrap estimate
        pos_rate_clu[i] <- mean(bootdat[[x]] == 1)
      }
      # results: point estimate and 95% percentile intervals
      c(mean(pos_rate_clu), quantile(pos_rate_clu, probs = c(0.025, 0.975)))
      
      # save results: crude...
      prev <- tibble(
        Incidence = c("Mean", "Variance"),
        crude = c(sp_crude, var(pos_rate_clu))
      )
      
    })
    
  })
  
  
  # extract results
  res <- as.data.frame(do.call(rbind, prev_vax))
  
  # Add rounds
  res$round <- rep(c("R3", "R4", "R5"), each = 4)
  
  # Add vaccination status
  res$vax <- rep(c("Negative", "Positive"), each = 2, times = 3)
  
  # Reshape 1
  essai <- reshape2::melt(res, id = c("Incidence", "round", "vax"))
  
  # Reshape 2
  res_vax <- reshape(essai, timevar = "Incidence", idvar = c("round", "vax", "variable"), v.names = "value", direction = "wide")
  
  colnames(res_vax) <- c("round", "vax", "Adjust", "estimate", "variance")
  
  
  ###
  # Save Results
  ###
  
  saveRDS(res_vax, paste0(here_prev_results("Unweighted"), "/sp_vax_uw_seed", seed, ".RDS"))
  
  
  ###
  # unweighted seroincidence for vaccinated and non vaccinated with bootstrap CI
  ###
  
  prev_vax_incidence <- sapply(c("R4_Result_imp", "R5_Result_imp"), function(x){
    
    data <- KoCo_BLab
    
    # define variables N and S for the previous round
    y <- x
    y <- str_replace(y, substr(x, 2, 2), as.character(as.numeric(substr(x, 2, 2))-1))
    y <- str_replace(y, "imp", "new")

    z <- paste0(substr(y, 1, 3), "Result_S1_imp")
    
    filt <- data[, y] == "Negative" & !is.na(data[, x])
    
    # create data set without missing values
    data_full <- na.omit(data[filt, c("hh_id", x, z)])
    
    
    data_full <- split(data_full, data_full[, z])
    
    prev <- lapply(data_full, function(tab){
      
      # seroincidence
      si_crude <- prop.table(table(tab[, x]))[2]
      
      ## cluster bootstrap for confidence intervals
      
      tab[, x] <- ifelse(tab[, x] == "Positive", 1, 0)
      
      # placeholder for bootstrap estimates
      pos_rate_clu <- numeric(boot)
      
      # cluster by household id
      tab <- as.data.table(tab)
      setkey(tab, "hh_id")
      
      # store number of households
      n_hh <- length(unique(tab$hh_id))
      
      # set seed for replicability
      set.seed(22)
      
      # start loop
      for (i in 1:boot)
      {
        # create the bootstrap dataset by resampling clusters
        samp <- sample(unique(tab$hh_id), n_hh, replace = TRUE)
        bootdat <- tab[J(samp), allow.cartesian = TRUE]
        # calculate bootstrap estimate
        pos_rate_clu[i] <- mean(bootdat[[x]] == 1)
      }
      # results: point estimate and 95% percentile intervals
      c(mean(pos_rate_clu), quantile(pos_rate_clu, probs = c(0.025, 0.975)))
      
      # save results: crude...
      prev <- tibble(
        Incidence = c("Mean", "Variance"),
        crude = c(si_crude, var(pos_rate_clu))
      )
      
    })
    
  })
  
  
  # extract results
  res <- as.data.frame(do.call(rbind, prev_vax_incidence))
  
  # Add rounds
  res$round <- rep(c("R4", "R5"), each = 4)
  
  # Add vaccination status
  res$vax <- rep(c("Negative", "Positive"), each = 2, times = 2)
  
  # Reshape 1
  essai <- reshape2::melt(res, id = c("Incidence", "round", "vax"))
  
  # Reshape 2
  res_vax <- reshape(essai, timevar = "Incidence", idvar = c("round", "vax", "variable"), v.names = "value", direction = "wide")
  
  colnames(res_vax) <- c("round", "vax", "Adjust", "estimate", "variance")
  
  
  ###
  # Save Results
  ###
  
  
  saveRDS(res_vax, paste0(here_prev_results("Unweighted"), "/si_vax_uw_seed", seed, ".RDS"))
  
  
  
}
