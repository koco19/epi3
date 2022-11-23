#' This script aggregates the weighted results after multiple imputation into one single estimate.
#' The variance due to the multiple imputation procedure is added to the sampling uncertainty.

aggregate_res_w <- function(seeds = 1:100, r = NULL, cal_vax = NULL){

  here_koco_data = function (...) here::here("Data", ...)
  here_koco_prev = function (...) here::here("Seroprevalence", ...)
  here_prev_figures = function (...) here_koco_prev("Figures", ...)
  here_prev_results = function (...) here_koco_prev("Results", ...)
  here_prev_scripts = function (...) here_koco_prev("Scripts", ...)
  
    
  # Number of iterations
  seeds <- 1:100
  
  
  # Aggregate the results
  res <- list()
  res_si <- list()
  res_si_vax <- list()
  res_sp_vax <- list()
  
  for(i in seeds){
    if (cal_vax == TRUE){
      res[[i]] <- read.csv(paste0(here_prev_results(paste0("R", r)), "/r", r, "_cum_sp_w_seed", i,".csv"))
    } else {
      res[[i]] <- read.csv(paste0(here_prev_results(paste0("R", r)), "/r", r, "_cum_sp_w_no_cal_seed", i,".csv"))
    }
    
  }
  
  # Estimates
  est <- lapply(res, function(x){
    x$estimate
  })
  
  est <- do.call(rbind, est)
  
  # Standard errors
  se <- lapply(res, function(x){
    (x$estimate - x$lb_ci)/qnorm(0.975)
  })
  
  se <- do.call(rbind, se)
  
  # Combined estimates
  res.comb <- mi.meld(est, se)
  
  
  res.fin <- data.frame(estimate = t(res.comb$q.mi), 
                        lb_ci = t(res.comb$q.mi - qnorm(0.975) * res.comb$se.mi),
                        ub_ci = t(res.comb$q.mi + qnorm(0.975) * res.comb$se.mi),
                        calculation = res[[1]]$Calculation,
                        adjust = res[[1]]$Adjust)
  
  # Save results
  if (cal_vax == TRUE){
    write.csv(res.fin, here_prev_results(paste0("r", r, "_cum_sp_w.csv")), row.names = FALSE)
  } else {
    write.csv(res.fin, here_prev_results(paste0("r", r, "_cum_sp_w_no_cal.csv")), row.names = FALSE)
  }
  
}