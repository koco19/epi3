#' This script aggregates the unweighted results after multiple imputation into one single estimate.
#' The variance due to the multiple imputation procedure is added to the uncertainty.

aggregate_res_uw <- function(seeds = 1:100){
  
  # rm(list = ls())
  
  here_koco_scripts = function (...) here::here("Scripts", ...)
  here_koco_results = function (...) here::here("Results", ...)
  
  
  # Aggregate the results
  res_si <- list()
  res_si_vax <- list()
  res_sp_vax <- list()
  
  for(i in seeds){
    res_si[[i]] <- readRDS(paste0(here_koco_results("Unweighted"), "/si_uw_seed", i,".RDS"))
    res_si_vax[[i]] <- readRDS(paste0(here_koco_results("Unweighted"), "/si_vax_uw_seed", i,".RDS"))
    res_sp_vax[[i]] <- readRDS(paste0(here_koco_results("Unweighted"), "/sp_vax_uw_seed", i,".RDS"))
  }
  
  res.fin <- lapply(list(res_si, res_si_vax, res_sp_vax), function(res){
    
    # Estimates
    est <- lapply(res, function(x){
      x$estimate
    })
    
    est <- do.call(rbind, est)
    
    # Standard errors
    se <- lapply(res, function(x){
      sqrt(x$variance)
    })
    
    se <- do.call(rbind, se)
    
    # Combined estimates
    res.comb <- mi.meld(est, se)
    
    res.fin <- data.frame(estimate = t(res.comb$q.mi), 
                          lb_ci = t(res.comb$q.mi - qnorm(0.975) * res.comb$se.mi),
                          ub_ci = t(res.comb$q.mi + qnorm(0.975) * res.comb$se.mi))
    
    # Adjust estimates
    
    # test performance (from Olbricht et al, 2020)
    specificity <- 0.9972041
    sensitivity <- 0.8860104
    
    res_adj <- (res.fin + specificity - 1) / (specificity + sensitivity - 1)
    
    # Final results
    res.fin <- rbind(res.fin, res_adj)
  })
  
  # Save results seroincidence
  res.fin[[1]]$round <- rep(c("R2", "R3", "R4", "R5"), 2)
  res.fin[[1]]$Adjust <- rep(c("unadjusted", "adjusted"), each = 4)
  
  saveRDS(res.fin[[1]], here_koco_results("si_uw.RDS"))
  
  # Save results seroincidence vax no vax
  res.fin[[2]]$round <- rep(c("R4", "R5"), each = 2, times = 2)
  res.fin[[2]]$vax <- rep(c("Negative", "Positive"), 4)
  res.fin[[2]]$Adjust <- rep(c("unadjusted", "adjusted"), each = 4)
  
  saveRDS(res.fin[[2]], here_koco_results("si_vax_uw.RDS"))
  
  # Save results seroprevalence vax no vax
  res.fin[[3]]$round <- rep(c("R3", "R4", "R5"), each = 2, times = 2)
  res.fin[[3]]$vax <- rep(c("Negative", "Positive"), 6)
  res.fin[[3]]$Adjust <- rep(c("unadjusted", "adjusted"), each = 6)
  
  saveRDS(res.fin[[3]], here_koco_results("sp_vax_uw.RDS"))
  
}
