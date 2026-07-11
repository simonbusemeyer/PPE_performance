# rltf_calibration.R
library(here)
source(here("current", "functions", "generate_dataModified_ng.R"))

calibrate_censoring_grid <- function(lambdas, 
                                n_patients, 
                                max_time, 
                                age_option = "E", 
                                beta_age = 0.02, 
                                target_censoring = 0.30, 
                                tol = 0.01, 
                                n_pilots = 5, 
                                max_iter = 50) {
  
  # Initialize the final dataframe to be used directly in main_batch.R
  scenarios <- data.frame(
    lambda = numeric(),
    borne_a = numeric()
  )
  
  cat(sprintf("Starting Random Loss to Follow-Up (RLTF) Calibration...\nTarget RLTF: %.1f%% (±%.1f%%)\n\n", 
              target_censoring * 100, tol * 100))
  
  for (lam in lambdas) {
    current_borne_a <- max_time * 1.5 
    calibrated <- FALSE
    iteration <- 1
    
    while (!calibrated && iteration <= max_iter) {
      rltf_rates <- numeric(n_pilots)
      
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
        
        # 2. Isolate Random Loss to Follow-up
        # Condition: Patient is censored (status == 0) AND this happened BEFORE administrative censoring
        rltf_rates[i] <- mean(df$status == 0 & df$observed_time < df$admin_cens)
      }
      
      mean_rltf <- mean(rltf_rates)
      
      # 3. Check boundaries and adjust borne_a
      # If RLTF is too high, -> INCREASE borne_a
      if (mean_rltf > (target_censoring + tol)) {
        current_borne_a <- current_borne_a * 1.1 
        
        # If RLTF is too low, DECREASE borne_a
      } else if (mean_rltf < (target_censoring - tol)) {
        current_borne_a <- current_borne_a * 0.9 
        
        # Target reached
      } else {
        calibrated <- TRUE
        cat(sprintf("[SUCCESS] Lambda = %.4f | borne_a = %7.2f | Random Censoring = %4.1f%% | (Iters: %d)\n", 
                    lam, current_borne_a, mean_rltf * 100, iteration))
      }
      
      iteration <- iteration + 1
    }
    
    # Fail-safe message if convergence wasn't reached
    if (!calibrated) {
      cat(sprintf("[WARNING] Lambda = %.4f | Failed to converge. Closest RLTF = %4.1f%% | Returning borne_a = %.2f\n", 
                  lam, mean_rltf * 100, current_borne_a))
    }
    
    # Append to scenarios dataframe
    scenarios <- rbind(scenarios, data.frame(lambda = lam, borne_a = current_borne_a))
  }
  
  cat("\nCalibration Complete.\n")
  return(scenarios)
}