# main_batch.R
rm(list=ls())

library(survival)
library(relsurv)
library(future.apply)
library(data.table)
library(here)

#source(here("fixes for later", "generate_dataModified_ng_corrections.R"))
source(here("current", "functions", "generate_dataModified_ng.R"))
source(here("current", "functions", "analyze_one_ng.R"))
source(here("current", "functions", "compute_metrics.R"))
#source(here("current", "functions", "rltf_calibration.R"))
source(here("current", "functions", "censoring_calibration.R"))
source(here("current", "functions", "nessie.R"))

# Global Parameters
n_patients <- 2000
age_option <- "Luo"
beta_age <- 0.02
beta_sex <- 0
year.start_min <- 2008
year.start_max <- 2010
prop_female <- 0
N_files <- 1000


#lambdas_to_run <- c(0.001, 0.01, 0.1) 
#lambdas_to_run <- c(0.0001, 0.0005, 0.001, 0.003, 0.005, 0.007, 0.01, 0.014, 0.019, 0.03, 0.055, 0.10)


#lambdas_to_run <- c(0.0001, 0.0002, 0.0005, 0.001, 0.002, 0.003, 0.004, 0.005, 0.007, 0.01, 0.014, 0.019, 0.03, 0.04, 0.055, 0.07, 0.10, 0.30)

lambdas_to_run <- c(0.004, 0.005, 0.007, 0.01, 0.014, 0.019, 0.03, 0.04, 0.055, 0.07, 0.10, 0.30) # use for Luo/LuoTrunc

# Calculate max_time
 # cat("Calculating dynamic max_time based on population expected survival...\n")
 # max_time <- determine_max_time(
 #   n_patients = n_patients,
 #   age_option = age_option,
 #   prop_female = prop_female,
 #   year.start_min = year.start_min,
 #   year.start_max = year.start_max
 # )

max_time <- 7

max_time_days <- max_time * 365.241
cat(sprintf("=> Dynamic max_time set to: %d years\n\n", max_time))
 #Run the calibration engine to dynamically generate the scenarios dataframe
scenarios <- calibrate_censoring_grid(
  lambdas = lambdas_to_run,
  n_patients = n_patients,
   max_time = max_time,
   age_option = age_option,
  beta_age = beta_age,
   target_censoring = 0.30, # target random loss to follow-up
   n_pilots = 3        # Adjust based on variance
 )

# Display the calculated scenarios to verify before main execution
print("=== Final Calibrated Scenarios ===")
print(scenarios)

# Reconcile output directories
dir_tables <- here("current", "outputs", "tables")
dir_data <- here("current", "outputs", "data")

start <- proc.time()

# Setup parallel backend once for the entire batch
options(future.globals.maxSize = 1000 * 1024^2) # Sets limit to ~1 GB
plan(multisession, workers = availableCores() - 1)
cat("Total Cores Available:", availableCores(), "\n")
cat("Active Parallel Workers:", nbrOfWorkers(), "\n")

# Execute Scenarios iteratively
for (i in seq_len(nrow(scenarios))) {
  
  lambda_scenario <- scenarios$lambda[i]
  borne_a_scenario <- scenarios$borne_a[i]
  
  cat(sprintf("\n--- Running scenario %d of %d: lambda = %.4f, borne_a = %.0f ---\n", 
              i, nrow(scenarios), lambda_scenario, borne_a_scenario))
  
  df <- vector("list", N_files)
  
  # Set seed per scenario to ensure comparable cohort generation across differing lambdas
  set.seed(12345) 
  
  # 1. Generate Data
  for (j in 1:N_files) {
    df[[j]] <- generate_data(
      lambda = lambda_scenario,     
      age_option = age_option,   
      n = n_patients,        
      max_time = max_time, 
      prop_female = prop_female,     
      year.start_min = year.start_min, 
      year.start_max = year.start_max,
      beta_sex = beta_sex,         
      beta_age = beta_age, 
      borne_a = borne_a_scenario         
    )
    df[[j]]$sim_id <- j
  }
  
  # 2. Analyze Data in Parallel
  results_scenarios <- future_lapply(df, function(single_df) {
    analyze_one(single_df, lambda = lambda_scenario, beta_age = beta_age, max_time = max_time)
  }, future.seed = TRUE)
  
  # 3. Aggregate Data Efficiently
  all_scenario_data <- rbindlist(df)
  
  all_scenario_data[, lambda := lambda_scenario]
  
  arrow::write_parquet(
    all_scenario_data, 
    sink = here("current", "outputs", "data", sprintf("simulated_cohort_lambda_%.4f.parquet", lambda_scenario))  )
  
  # 4. Calculate and Save Metrics
  metrics <- compute_metrics(results_list = results_scenarios, lambda_val = lambda_scenario, borne_a_val = borne_a_scenario)
  saveRDS(metrics, file = here("current", "outputs", "tables", sprintf("metrics_lambda_%.4f.rds", lambda_scenario)))}

# Close parallel backend to free up resources
plan(sequential)

elapsed <- proc.time() - start
cat("\nTotal execution time:\n")
print(elapsed)

# Aggregate Results
rds_files <- list.files(dir_tables, pattern = "metrics_lambda_.*\\.rds$", full.names = TRUE)
all_metrics_list <- lapply(rds_files, readRDS)

# better than do.call(rbind, ...)
final_results <- rbindlist(all_metrics_list, use.names = TRUE, fill = TRUE)
setDF(final_results)

saveRDS(final_results, here("current", "outputs", "tables", "final_results_complete.rds"))
write.csv(final_results, 
          file = here("current", "outputs", "tables", "final_results_complete.csv"),          row.names = FALSE)
