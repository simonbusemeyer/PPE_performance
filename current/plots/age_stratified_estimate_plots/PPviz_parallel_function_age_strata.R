library(here)
library(ggplot2)
library(dplyr)

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
  
  required_packages <- c("survival", "relsurv", "future", "future.apply", "arrow", "ggplot2", "dplyr")
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
  
  pct_overall_val <- metrics_data$pct_cancer[metrics_data$pp_age_class == "Overall"][1]
  pct_cancer_viz  <- round(pct_overall_val * 100, 1)
  
  all_simulated_data$age_days <- all_simulated_data$age * 365.241
  all_simulated_data$observed_time_days <- all_simulated_data$observed_time * 365.241
  
  # Stratify cohort by Age Group
  all_simulated_data$age_group <- ifelse(all_simulated_data$age < 65, "<65", ">=65")
  all_simulated_data$age_group <- factor(all_simulated_data$age_group, levels = c("<65", ">=65"))
  
  t_seq_years <- seq(0, max_time, by = t_by)
  t_seq_days <- t_seq_years * 365.241
  
  risk_scores <- exp(
    beta_sex * all_simulated_data$sex_num +
      beta_age * all_simulated_data$ageCentre
  )
  
  # Dynamically calculate theoretical mean based on selected age options
  rs_young <- risk_scores[all_simulated_data$age_group == "<65"]
  rs_old <- risk_scores[all_simulated_data$age_group == ">=65"]
  
  surv_theo_young <- sapply(t_seq_years, function(t) {
    mean(exp(-lambda_scenario * t * rs_young))
  })
  
  surv_theo_old <- sapply(t_seq_years, function(t) {
    mean(exp(-lambda_scenario * t * rs_old))
  })
  
  # Filter out unneeded columns to optimize memory before splitting the data
  keep_cols <- c("sim_id", "observed_time_days", "status", "age_days", "sex", "year_diagnosis", "age_group")
  all_simulated_data <- subset(all_simulated_data, select = keep_cols)
  
  unique_sims <- unique(all_simulated_data$sim_id)
  n_sims <- length(unique_sims)
  sim_list <- split(all_simulated_data, all_simulated_data$sim_id)
  
  # Remove unneeded objects
  rm(all_simulated_data, risk_scores, metrics_data, rs_young, rs_old)
  gc()
  
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
      
      # Helper function for isolated PP evaluation
      get_surv <- function(dat) {
        if (nrow(dat) > 0) {
          pp_sim <- relsurv::rs.surv(
            survival::Surv(observed_time_days, status) ~ 1,
            data = dat,
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
      }
      
      dat_young <- data_sim[data_sim$age_group == "<65", ]
      dat_old <- data_sim[data_sim$age_group == ">=65", ]
      
      list(
        young = get_surv(dat_young),
        old = get_surv(dat_old)
      )
    },
    future.seed = TRUE
  )
  
  future::plan(future::sequential)
  
  # Extract and compute means and standard errors for <65
  pp_mat_young <- do.call(cbind, lapply(pp_surv_list, `[[`, "young"))
  mean_young <- rowMeans(pp_mat_young, na.rm = TRUE)
  
  # Extract and compute means and standard errors for >=65
  pp_mat_old <- do.call(cbind, lapply(pp_surv_list, `[[`, "old"))
  mean_old <- rowMeans(pp_mat_old, na.rm = TRUE)
  
  if (n_sims > 1) {
    se_young <- apply(pp_mat_young, 1, sd, na.rm = TRUE) / sqrt(n_sims)
    ci_low_young <- mean_young - 1.96 * se_young
    ci_up_young <- mean_young + 1.96 * se_young
    
    se_old <- apply(pp_mat_old, 1, sd, na.rm = TRUE) / sqrt(n_sims)
    ci_low_old <- mean_old - 1.96 * se_old
    ci_up_old <- mean_old + 1.96 * se_old
  } else {
    ci_low_young <- ci_up_young <- rep(NA_real_, length(mean_young))
    ci_low_old <- ci_up_old <- rep(NA_real_, length(mean_old))
  }
  
  # --- ggplot2 VISUALIZATION ---
  
  df_mean <- data.frame(
    time = rep(t_seq_years, 2),
    age_group = factor(rep(c("<65", ">=65"), each = length(t_seq_years)), levels = c("<65", ">=65")),
    surv = c(mean_young, mean_old),
    lower = c(ci_low_young, ci_low_old),
    upper = c(ci_up_young, ci_up_old),
    theo = c(surv_theo_young, surv_theo_old)
  )
  
  set.seed(seed)
  sampled_cols <- sample(
    seq_len(n_sims),
    size = min(n_plot, n_sims),
    replace = FALSE
  )
  
  # Build long dataframe for individual step lines
  df_indiv <- do.call(rbind, lapply(sampled_cols, function(i) {
    rbind(
      data.frame(time = t_seq_years, age_group = factor("<65", levels = c("<65", ">=65")), sim_id = as.character(i), surv = pp_mat_young[, i]),
      data.frame(time = t_seq_years, age_group = factor(">=65", levels = c("<65", ">=65")), sim_id = as.character(i), surv = pp_mat_old[, i])
    )
  }))
  
  p <- ggplot2::ggplot() +
    # Faded individual simulation lines
    ggplot2::geom_step(
      data = df_indiv,
      ggplot2::aes(x = time, y = surv, group = interaction(sim_id, age_group), color = age_group),
      alpha = 0.2, linewidth = 0.5
    ) +
    # # Confidence Interval Ribbons
    # ggplot2::geom_ribbon(
    #   data = df_mean,
    #   ggplot2::aes(x = time, ymin = lower, ymax = upper, fill = age_group),
    #   alpha = 0.3
    # ) +
    # Confidence Interval Ribbons
    # Mean PP lines
    ggplot2::geom_step(
      data = df_mean,
      ggplot2::aes(x = time, y = surv, color = age_group),
      linewidth = 1.2
    ) +
    # Theoretical Mean lines
    ggplot2::geom_line(
      data = df_mean,
      ggplot2::aes(x = time, y = theo, color = age_group),
      linetype = "dashed", linewidth = 1
    ) +
    ggplot2::scale_color_manual(values = c("<65" = "#2c728e", ">=65" = "#d55e00")) +
    ggplot2::scale_fill_manual(values = c("<65" = "#2c728e", ">=65" = "#d55e00")) +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    ggplot2::coord_cartesian(ylim = c(0.2, 1.0)) +
    ggplot2::labs(
      title = paste0(
        "Net Survival: PP vs Theoretical\n",
        "Lambda = ", sprintf("%.4f", lambda_scenario),
        "; Proportion of deaths due to cancer: ", pct_cancer_viz, "%"
      ),
      x = "Time since diagnosis (Years)",
      y = "Net Survival Probability",
      color = "Age Group",
      fill = "Age Group"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      axis.title.x = ggplot2::element_text(size = 14),
      axis.text.x  = ggplot2::element_text(size = 14),
      legend.title = ggplot2::element_text(size = 14),
      legend.text  = ggplot2::element_text(size = 14),
      legend.position = "bottom",
      panel.grid.minor = ggplot2::element_blank()
    )
  
  print(p)
  
  invisible(
    list(
      plot_obj = p,
      lambda = lambda_scenario,
      pct_cancer = pct_cancer_viz,
      t_seq_years = t_seq_years,
      pp_mat_young = pp_mat_young,
      pp_mat_old = pp_mat_old,
      mean_young = mean_young,
      mean_old = mean_old,
      theo_young = surv_theo_young,
      theo_old = surv_theo_old
    )
  )
}