# 1. Source the plotting function using relative paths
library(here)

# 1. Source the plotting function using absolute paths
source_path <- here("current", "plots", "PPviz_parallel_function.R")
if (!file.exists(source_path)) stop("CRITICAL: Function script not found at ", source_path)
source(source_path)

# 2. Define absolute input/output paths
data_dir    <- here("current", "outputs", "data")
metrics_dir <- here("current", "outputs", "tables")
save_dir    <- here("current", "outputs", "plots_data")

dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)

# 3. Locate and verify the simulation files
lambda_files <- list.files(data_dir, pattern = "^simulated_cohort_lambda_.*\\.parquet$", full.names = FALSE)
lambda_values <- sort(as.numeric(stringr::str_remove(stringr::str_remove(lambda_files, "^simulated_cohort_lambda_"), "\\.parquet$")))

# 4. Run parallel pre-computation
for (lambda_val in lambda_values) {
  message("Pre-computing plot coordinates for Lambda = ", lambda_val)
  
  plot_data <- plot_ppviz_parallel(
    lambda_scenario = lambda_val,
    data_dir = data_dir,
    metrics_dir = metrics_dir,
    use_parallel = TRUE
  )
  
  save_path <- file.path(save_dir, sprintf("ppviz_data_lambda_%.4f.rds", lambda_val))
  saveRDS(plot_data, save_path)
  
  # Dump RAM between parallel loops
  rm(plot_data)
  gc()
}

message("SUCCESS: Saved all pre-computed plot objects directly into '", save_dir, "'")