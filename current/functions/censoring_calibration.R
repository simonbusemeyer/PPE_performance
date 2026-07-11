# censoring_calibration.R

calibrate_censoring_grid <- function(lambdas, 
                                     n_patients, 
                                     max_time, 
                                     age_option = "Luo", 
                                     beta_age = 0.02, 
                                     target_censoring = 0.30, 
                                     tol = 0.01, 
                                     n_pilots = 3, 
                                     max_iter = 25) {
  
  # Initialize the final dataframe to be used directly in main_batch.R
  scenarios <- data.frame(
    lambda = numeric(),
    borne_a = numeric()
  )
  
  cat(sprintf("Starting Total Censoring Calibration...\nTarget Total Censoring: %.1f%% (±%.1f%%)\n\n", 
              target_censoring * 100, tol * 100))
  
  for (lam in lambdas) {
    current_borne_a <- max_time * 1.5 
    calibrated <- FALSE
    iteration <- 1
    
    while (!calibrated && iteration <= max_iter) {
      censoring_rates <- numeric(n_pilots)
      
      # 1. Generate pilot datasets
      for (i in 1:n_pilots) {
        df <- generate_data(
          lambda = lam, 
          age_option = age_option, 
          n = n_patients, 
          max_time = max_time,
          prop_female = 0,
          year.start_min = 2008, 
          year.start_max = 2010,
          beta_sex = 0, 
          beta_age = beta_age, 
          borne_a = current_borne_a
        )
        
        # 2. Isolate Total Censoring
        # Condition: Patient is censored (status == 0) for ANY reason (Random or Administrative)
        censoring_rates[i] <- mean(df$status == 0)
      }
      
      mean_censoring <- mean(censoring_rates)
      
      # 3. Check boundaries and adjust borne_a
      # If Total Censoring is too high, INCREASE borne_a
      if (mean_censoring > (target_censoring + tol)) {
        current_borne_a <- current_borne_a * 1.1 
        
        # If Total Censoring is too low, DECREASE borne_a
      } else if (mean_censoring < (target_censoring - tol)) {
        current_borne_a <- current_borne_a * 0.9 
        
        # Target reached
      } else {
        calibrated <- TRUE
        cat(sprintf("[SUCCESS] Lambda = %.4f | borne_a = %7.2f | Total Censoring = %4.1f%% | (Iters: %d)\n", 
                    lam, current_borne_a, mean_censoring * 100, iteration))
      }
      
      iteration <- iteration + 1
    }
    
    # if convergence wasn't reached
    if (!calibrated) {
      current_borne_a <- Inf
      cat(sprintf("[WARNING] Lambda = %.4f | Failed to converge. Closest Total Censoring = %4.1f%% | Setting borne_a = Inf\n", 
                  lam, mean_censoring * 100))
    }
    
    # Append to scenarios dataframe
    scenarios <- rbind(scenarios, data.frame(lambda = lam, borne_a = current_borne_a))
  }
  
  cat("\nCalibration Complete.\n")
  return(scenarios)
}
