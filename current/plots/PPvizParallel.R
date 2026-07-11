# PPvizParallel.R
# ---------------------------------------------------------
# VISUALIZING POHAR-PERME VS THEORETICAL (Fully Optimized)
# ---------------------------------------------------------
library(survival)
library(relsurv)
library(future.apply)
library(arrow)
library(here)

# Parameters (Must match the target scenario)
lambda_scenario <- 0.018
max_time <- max_time 
beta_age <- 0.02
beta_sex <- 0
N_plot <- 20

# Load Data from Batch Output folder
data_path <- here("current", "outputs", "data", sprintf("simulated_cohort_lambda_%.4f.parquet", lambda_scenario))
metrics_path <- here("current", "outputs", "tables", sprintf("metrics_lambda_%.4f.rds", lambda_scenario))

if (!file.exists(data_path)) {
  stop(sprintf("Batch data file %s not found! Run main_batch.R first.", data_path))
}

all_simulated_data <- arrow::read_parquet(data_path)
metrics_data       <- readRDS(metrics_path)

# Extract and round pct_cancer
pct_cancer_viz <- round(metrics_data$pct_cancer[1] * 100, 1)

# Convert Time Units to Days for relsurv
all_simulated_data$age_days <- all_simulated_data$age * 365.241
all_simulated_data$observed_time_days <- all_simulated_data$observed_time * 365.241

# ---------------------------------------------------------
# OPTIMIZED: Calculate Theoretical Curve
# ---------------------------------------------------------
start <- proc.time()

t_seq_years <- seq(0, max_time, by = 0.1)

# 1. Calculate the baseline risk score for all patients at once
risk_scores <- exp(beta_sex * all_simulated_data$sex_num + 
                     beta_age * all_simulated_data$ageCentre)

# 2. Vectorize over the time sequence rather than the patients
surv_theo_mean <- sapply(t_seq_years, function(t) {
  mean(exp(-lambda_scenario * t * risk_scores))
})

# ---------------------------------------------------------
# OPTIMIZED & PARALLELIZED: Mean of Individual PP Estimates
# ---------------------------------------------------------
options(parallelly.fork.enable = TRUE)

t_seq_days <- t_seq_years * 365.241
unique_sims <- unique(all_simulated_data$sim_id)
n_sims <- length(unique_sims)

message("Splitting data into lists...")
sim_list <- split(all_simulated_data, all_simulated_data$sim_id)

message("Setting up future parallel plan...")
# Maximize efficiency: Forking on Mac/Linux, Background sessions on Windows
num_cores <- max(1, availableCores() - 1)
if (.Platform$OS.type == "unix") {
  plan(multicore, workers = num_cores)
} else {
  plan(multisession, workers = num_cores)
}

message(paste("Calculating individual PP curves across", num_cores, "cores..."))

pp_surv_list <- future_lapply(sim_list, function(data_sim) {
  if (nrow(data_sim) > 0) {
    
    # Explicit namespace calls to prevent scoping errors on worker nodes
    pp_sim <- relsurv::rs.surv(
      survival::Surv(observed_time_days, status) ~ 1,
      data = data_sim,
      ratetable = survival::survexp.us,
      rmap = list(age = age_days, sex = sex, year = year_diagnosis),
      method = "pohar-perme"
    )
    return(summary(pp_sim, times = t_seq_days, extend = TRUE)$surv)
    
  } else {
    return(rep(NA, length(t_seq_days)))
  }
}, future.seed = TRUE)

# Shut down multisession workers to free up RAM (does nothing under multicore)
plan(sequential)

# Bind the resulting list of vectors back into your matrix natively
pp_surv_matrix <- do.call(cbind, pp_surv_list)

# Calculate the mean and 95% CI
mean_pp_surv <- rowMeans(pp_surv_matrix, na.rm = TRUE)
se_pp_surv <- apply(pp_surv_matrix, 1, sd, na.rm = TRUE) / sqrt(n_sims)
lower_pp_surv <- mean_pp_surv - 1.96 * se_pp_surv
upper_pp_surv <- mean_pp_surv + 1.96 * se_pp_surv

message("Mean calculation complete.")

# ---------------------------------------------------------
# Plotting
# ---------------------------------------------------------
plot(
  0, type = "n", xlim = c(0, max_time), ylim = c(0.9, 1.0),
  xlab = "Time since diagnosis (Years)", ylab = "Net Survival Probability",
  main = paste0("Net Survival: PP vs Theoretical \n Proportion of deaths due to cancer: ", pct_cancer_viz, "%")
)
grid()

# Plot 20 random lines directly from the matrix
set.seed(54321)
sampled_cols <- sample(1:ncol(pp_surv_matrix), size = min(N_plot, ncol(pp_surv_matrix)), replace = FALSE)

for (i in sampled_cols) {
  lines(
    t_seq_years, 
    pp_surv_matrix[, i],
    col = rgb(0.2, 0.5, 0.8, alpha = 0.3),
    lwd = 1,
    type = "s"
  )
}

# Overlay Mean and CI
lines(t_seq_years, mean_pp_surv, col = "red", lwd = 3, type = "l")
#lines(t_seq_years, upper_pp_surv, col = "red", lwd = 1.5, lty = 3, type = "l") #not plotting CI for now
# lines(t_seq_years, lower_pp_surv, col = "red", lwd = 1.5, lty = 3, type = "l")
lines(t_seq_years, surv_theo_mean, col = "black", lwd = 3, lty = 2)

elapsed <- proc.time() - start
elapsed

# Legend
legend(
  "bottomleft",
  legend = c(
    paste0("Individual PP Estimates (First ", N_plot, ")"),
    paste0("Mean PP Estimate (N iterations=", n_sims, ") & 95% CI"),
    "Theoretical Net Survival Curve S(t)"
  ),
  col = c(rgb(0.2, 0.5, 0.8, alpha = 0.5), "red", "black"),
  lwd = c(2, 3, 3), lty = c(1, 1, 2), bty = "n", cex = 0.85
)
