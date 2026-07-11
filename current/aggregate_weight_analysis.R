# =============================================================================
# aggregate_weight_analysis.R
# =============================================================================

library(data.table)
library(future.apply)
library(arrow)
library(here)

source(here("current", "functions", "extract_weight_distributions.R"))

options(future.globals.maxSize = 1000 * 1024^2) # 1 GB limit
plan(multisession, workers = availableCores() - 1)
cat("Active Parallel Workers:", nbrOfWorkers(), "\n")

parquet_files <- list.files(
  here("current", "outputs", "data"), 
  pattern = "simulated_cohort_lambda_.*\\.parquet$", 
  full.names = TRUE
)

start_time <- proc.time()

batch_summary_list <- future_lapply(parquet_files, function(p_file) {
  
  cohort_data <- arrow::read_parquet(p_file)
  setDT(cohort_data)
  
  current_lambda <- unique(cohort_data$lambda)
  
  # Extract metrics per simulation file (sim_id)
  file_metrics <- cohort_data[, extract_weight_metrics(.SD, horizons = c(1, 4, 7)), by = .(sim_id)]
  
  averaged <- file_metrics[, .(
    mean_n_at_risk                  = mean(n_at_risk, na.rm = TRUE),
    
    # Weight central tendency and upper-tail stability
    avg_weight_median               = mean(weight_median, na.rm = TRUE),
    avg_weight_iqr                  = mean(weight_iqr, na.rm = TRUE),
    avg_weight_p90                  = mean(weight_p90, na.rm = TRUE),
    avg_weight_p95                  = mean(weight_p95, na.rm = TRUE),
    avg_weight_p99                  = mean(weight_p99, na.rm = TRUE),
    mean_weight_max                 = mean(weight_max, na.rm = TRUE),
    max_weight_max                  = max(weight_max, na.rm = TRUE),
    
    # Baseline age demographics
    mean_survivor_baseline_age_mean = mean(survivor_mean_baseline_age, na.rm = TRUE),
    mean_survivor_baseline_age_p95  = mean(survivor_p95_baseline_age, na.rm = TRUE),
    mean_survivor_baseline_age_p99  = mean(survivor_p99_baseline_age, na.rm = TRUE),
    mean_survivor_baseline_age_max  = mean(survivor_max_baseline_age, na.rm = TRUE),
    max_survivor_baseline_age_max   = max(survivor_max_baseline_age, na.rm = TRUE),
    
    # Attained age demographics
    mean_survivor_attained_age_mean = mean(survivor_mean_attained_age, na.rm = TRUE),
    mean_survivor_attained_age_p95  = mean(survivor_p95_attained_age, na.rm = TRUE),
    mean_survivor_attained_age_p99  = mean(survivor_p99_attained_age, na.rm = TRUE),
    mean_survivor_attained_age_max  = mean(survivor_max_attained_age, na.rm = TRUE),
    max_survivor_attained_age_max   = max(survivor_max_attained_age, na.rm = TRUE)
  ), by = .(horizon_years, pp_age_class)]
  
  averaged[, lambda := current_lambda]
  return(averaged)
}, future.seed = TRUE)

plan(sequential)

elapsed <- proc.time() - start_time
cat("\nTotal extraction time:\n")
print(elapsed)

final_weight_analysis <- rbindlist(batch_summary_list, use.names = TRUE)
setcolorder(final_weight_analysis, c("lambda", "horizon_years", "pp_age_class"))
setorder(final_weight_analysis, lambda, horizon_years, pp_age_class)

saveRDS(
  final_weight_analysis, 
  here("current", "outputs", "tables", "final_weight_distribution_analysis.rds")
)
write.csv(
  final_weight_analysis, 
  here("current", "outputs", "tables", "final_weight_distribution_analysis.csv"), 
  row.names = FALSE
)