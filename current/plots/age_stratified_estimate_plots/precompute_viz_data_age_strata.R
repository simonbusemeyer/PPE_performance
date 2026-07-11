library(here)
library(dplyr)
library(stringr)

source_path <- here("current", "plots", "age_stratified_estimate_plots", "PPviz_parallel_function_age_strata.R")
if (!file.exists(source_path)) stop("CRITICAL: Function script not found at ", source_path)
source(source_path)

data_dir    <- here("current", "outputs", "data")
metrics_dir <- here("current", "outputs", "tables")
save_dir    <- here("current", "outputs", "plots_data_age_strata")

dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

# 1. Respect the target_lambdas from Quarto if they exist
if (exists("target_lambdas")) {
  message("Inheriting specific lambdas from Quarto environment...")
  lambda_values <- as.numeric(target_lambdas)
} else {
  # Fallback: run everything if this script is executed manually outside Quarto
  message("No targets specified. Processing all available lambda scenarios...")
  lambda_files <- list.files(data_dir, pattern = "^simulated_cohort_lambda_.*\\.parquet$", full.names = FALSE)
  lambda_values <- sort(as.numeric(stringr::str_remove(stringr::str_remove(lambda_files, "^simulated_cohort_lambda_"), "\\.parquet$")))
}

for (lambda_val in lambda_values) {
  message("Pre-computing age strata plot for Lambda = ", lambda_val)
  
  plot_data <- plot_ppviz_parallel(
    lambda_scenario = lambda_val,
    data_dir = data_dir,
    metrics_dir = metrics_dir,
    use_parallel = TRUE
  )
  
  # 2. Save using EXACT string formatting to avoid scientific notation mismatches
  save_path <- file.path(save_dir, paste0("ppviz_age_strata_lambda_", as.character(lambda_val), ".rds"))
  saveRDS(plot_data, save_path)
  
  rm(plot_data)
  gc()
}

message("SUCCESS: Saved pre-computed age strata plot objects into '", save_dir, "'")