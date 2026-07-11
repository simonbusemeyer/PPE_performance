library(here)

plot_ppviz_parallel <- function(
    lambda_scenario,
    data_dir = here("current", "outputs", "data"),
    metrics_dir = here("current", "outputs", "tables"),
    max_time = NULL,
    beta_age = 0.02,
    beta_sex = 0,
    n_plot = 20,
    t_by = 0.1,
    use_parallel = TRUE,
    workers = max(1, future::availableCores() - 2),
    seed = 54321
) {
  
  required_packages <- c("survival", "relsurv", "future", "future.apply", "arrow")
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]
  
  if (length(missing_packages) > 0) {
    stop(
      "Missing required package(s): ",
      paste(missing_packages, collapse = ", ")
    )
  }
  
  data_path <- file.path(
    data_dir,
    sprintf("simulated_cohort_lambda_%.4f.parquet", lambda_scenario)
  )
  
  metrics_path <- file.path(
    metrics_dir,
    sprintf("metrics_lambda_%.4f.rds", lambda_scenario)
  )
  
  if (!file.exists(data_path)) {
    stop(sprintf("Batch data file not found: %s", data_path))
  }
  
  if (!file.exists(metrics_path)) {
    stop(sprintf("Metrics file not found: %s", metrics_path))
  }
  
  all_simulated_data <- arrow::read_parquet(data_path)
  metrics_data <- readRDS(metrics_path)
  
  if (is.null(max_time)) {
    max_time <- max(all_simulated_data$observed_time, na.rm = TRUE)
  }
  
  required_cols <- c(
    "age",
    "observed_time",
    "sex_num",
    "ageCentre",
    "sim_id",
    "status",
    "sex",
    "year_diagnosis"
  )
  
  missing_cols <- setdiff(required_cols, names(all_simulated_data))
  
  if (length(missing_cols) > 0) {
    stop(
      "Simulated data is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  if (!"pct_cancer" %in% names(metrics_data)) {
    stop("metrics_data is missing required column: pct_cancer")
  }
  
  pct_cancer_viz <- round(metrics_data$pct_cancer[1] * 100, 1)
  
  all_simulated_data$age_days <- all_simulated_data$age * 365.241
  all_simulated_data$observed_time_days <- all_simulated_data$observed_time * 365.241
  
  t_seq_years <- seq(0, max_time, by = t_by)
  t_seq_days <- t_seq_years * 365.241
  
  risk_scores <- exp(
    beta_sex * all_simulated_data$sex_num +
      beta_age * all_simulated_data$ageCentre
  )
  
  surv_theo_mean <- sapply(t_seq_years, function(t) {
    mean(exp(-lambda_scenario * t * risk_scores))
  })
  
  # Filter out unneeded columns to optimize memory before splitting the data
  keep_cols <- c("sim_id", "observed_time_days", "status", "age_days", "sex", "year_diagnosis")
  all_simulated_data <- all_simulated_data[, ..keep_cols]
  
  unique_sims <- unique(all_simulated_data$sim_id)
  n_sims <- length(unique_sims)
  sim_list <- split(all_simulated_data, all_simulated_data$sim_id)
  
  # Remove unneeded objects so they aren't captured by the global environment
  rm(all_simulated_data, risk_scores, metrics_data)
  
  # release RAM immediately (Garbage Collection)
  gc()
  
  # Raise global memory transfer threshold to 4 GiB
  options(future.globals.maxSize = 4 * 1024^3)
  
  old_plan <- future::plan()
  on.exit(future::plan(future::sequential), add = TRUE)
  
  if (use_parallel) {
    if (.Platform$OS.type == "unix") {
      future::plan(future::multicore, workers = workers)
    } else {
      future::plan(future::multisession, workers = workers)
    }
  } else {
    future::plan(future::sequential)
  }
  
  pp_surv_list <- future.apply::future_lapply(
    sim_list,
    function(data_sim) {
      if (nrow(data_sim) > 0) {
        
        pp_sim <- relsurv::rs.surv(
          survival::Surv(observed_time_days, status) ~ 1,
          data = data_sim,
          ratetable = survival::survexp.us,
          rmap = list(
            age = age_days,
            sex = sex,
            year = year_diagnosis
          ),
          method = "pohar-perme"
        )
        
        summary(pp_sim, times = t_seq_days, extend = TRUE)$surv
        
      } else {
        rep(NA_real_, length(t_seq_days))
      }
    },
    future.seed = TRUE
  )
  
  future::plan(future::sequential)
  
  pp_surv_matrix <- do.call(cbind, pp_surv_list)
  
  mean_pp_surv <- rowMeans(pp_surv_matrix, na.rm = TRUE)
  
  if (n_sims > 1) {
    se_pp_surv <- apply(pp_surv_matrix, 1, sd, na.rm = TRUE) / sqrt(n_sims)
    lower_pp_surv <- mean_pp_surv - 1.96 * se_pp_surv
    upper_pp_surv <- mean_pp_surv + 1.96 * se_pp_surv
  } else {
    lower_pp_surv <- rep(NA_real_, length(mean_pp_surv))
    upper_pp_surv <- rep(NA_real_, length(mean_pp_surv))
  }
  
  plot(
    0,
    type = "n",
    xlim = c(0, max_time),
    ylim = c(0.8, 1.0),
    xlab = "Time since diagnosis (Years)",
    ylab = "Net Survival Probability",
    main = paste0(
      "Net Survival: PP vs Theoretical",
      "\nLambda = ", sprintf("%.4f", lambda_scenario),
      "; Proportion of deaths due to cancer: ", pct_cancer_viz, "%"
    )
  )
  
  grid()
  
  set.seed(seed)
  sampled_cols <- sample(
    seq_len(ncol(pp_surv_matrix)),
    size = min(n_plot, ncol(pp_surv_matrix)),
    replace = FALSE
  )
  
  for (i in sampled_cols) {
    lines(
      t_seq_years,
      pp_surv_matrix[, i],
      col = rgb(0.2, 0.5, 0.8, alpha = 0.3),
      lwd = 1,
      type = "s"
    )
  }
  
  lines(t_seq_years, mean_pp_surv, col = "red", lwd = 3, type = "l")
  lines(t_seq_years, surv_theo_mean, col = "black", lwd = 3, lty = 2)
  
  legend(
    "bottomleft",
    legend = c(
      paste0("Individual PP Estimates, sampled N = ", min(n_plot, ncol(pp_surv_matrix))),
      paste0("Mean PP Estimate, N iterations = ", n_sims),
      "Theoretical Net Survival Curve S(t)"
    ),
    col = c(rgb(0.2, 0.5, 0.8, alpha = 0.5), "red", "black"),
    lwd = c(2, 3, 3),
    lty = c(1, 1, 2),
    bty = "n",
    cex = 0.85
  )
  
  invisible(
    list(
      lambda = lambda_scenario,
      pct_cancer = pct_cancer_viz,
      t_seq_years = t_seq_years,
      pp_surv_matrix = pp_surv_matrix,
      mean_pp_surv = mean_pp_surv,
      lower_pp_surv = lower_pp_surv,
      upper_pp_surv = upper_pp_surv,
      surv_theo_mean = surv_theo_mean
    )
  )
}
