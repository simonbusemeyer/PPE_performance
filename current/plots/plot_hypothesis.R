# =============================================================================
# plot_hypothesis_functions.R
# Modular functions for generating validation plots in Quarto
# =============================================================================

library(ggplot2)
library(data.table)
library(patchwork)
library(here)

#' Plot Case 1: Upper-Tail Weight Distribution, Median, and Risk-Set Size
#'
#' @param data A data.table containing the weight distribution analysis outputs.
#' @param target_horizon Numeric. The horizon year to filter by (default: 7).
#' @param target_age_class Character. The age class stratum to isolate (default: ">=65").
#' @param save_path Character (optional). File path to save the plot PNG.
#' @return A patchwork ggplot object.
plot_case1_weights_and_riskset <- function(data, 
                                           target_horizon = 7, 
                                           target_age_class = ">=65", 
                                           save_path = NULL) {
  # Ensure data.table and isolate stratum safely
  dt <- as.data.table(data)
  df_sub <- copy(dt[horizon_years == target_horizon & pp_age_class == target_age_class])
  
  if (nrow(df_sub) == 0) {
    stop("No data found for the specified horizon and age class combinations.")
  }
  
  # 1. Upper-Tail Plot
  df_tail_long <- melt(
    df_sub, 
    id.vars = "lambda", 
    measure.vars = c("avg_weight_p95", "avg_weight_p99", "mean_weight_max"),
    variable.name = "Metric", 
    value.name = "Weight"
  )
  df_tail_long[, Metric := factor(
    Metric, 
    labels = c("95th Percentile (p95)", "99th Percentile (p99)", "Mean of Sample Maxima")
  )]
  
  p_upper_tail <- ggplot(df_tail_long, aes(x = lambda, y = Weight, color = Metric, shape = Metric)) +
    geom_line(linewidth = 1.1) + 
    geom_point(size = 3) +
    scale_color_manual(values = c("#e6ab02", "#d95f02", "#7570b3")) +
    labs(
      title = sprintf("Case 1A: Upper-Tail Weight Distribution at %d Years (%s Cohort)", target_horizon, target_age_class),
      subtitle = "Trajectories of the 95th percentile, 99th percentile, and sample maxima across excess hazard rates",
      x = expression(paste("Excess Hazard Rate (", lambda, ")")), 
      y = "Pohar-Perme Weight", 
      color = NULL, 
      shape = NULL
    ) + 
    theme_minimal(base_size = 13) + 
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
  
  # 2. Median Plot
  p_median <- ggplot(df_sub, aes(x = lambda, y = avg_weight_median)) +
    geom_line(color = "#1b9e77", linewidth = 1.2) + 
    geom_point(color = "#1b9e77", size = 3) +
    labs(
      title = sprintf("Case 1B: Median Weight Among %d-Year Survivors (%s Cohort)", target_horizon, target_age_class),
      x = expression(paste("Excess Hazard Rate (", lambda, ")")), 
      y = "Average Median Weight"
    ) + 
    theme_minimal(base_size = 13) + 
    theme(plot.title = element_text(face = "bold"))
  
  # 3. Risk-Set Size Plot
  p_riskset <- ggplot(df_sub, aes(x = lambda, y = mean_n_at_risk)) +
    geom_line(color = "#4daf4a", linewidth = 1.2) + 
    geom_point(color = "#4daf4a", size = 3) +
    labs(
      title = sprintf("Case 1C: Risk-Set Size at %d Years (%s Cohort)", target_horizon, target_age_class),
      subtitle = sprintf("Mean number of patients remaining at risk at %d years post-diagnosis across excess hazard rates", target_horizon),
      x = expression(paste("Excess Hazard Rate (", lambda, ")")), 
      y = "Mean Number at Risk"
    ) + 
    theme_minimal(base_size = 13) + 
    theme(plot.title = element_text(face = "bold"))
  
  # Combine into dashboard
  plot_combined <- p_upper_tail / p_median / p_riskset + plot_layout(heights = c(1.3, 1, 1))
  
  # Optional disk export
  if (!is.null(save_path)) {
    ggsave(save_path, plot = plot_combined, width = 9, height = 12)
  }
  
  return(plot_combined)
}


#' Plot Case 2: Survivor Age Demographics
#'
#' @param data A data.table containing the weight distribution analysis outputs.
#' @param target_horizon Numeric. The horizon year to filter by (default: 7).
#' @param target_age_class Character. The age class stratum to isolate (default: ">=65").
#' @param save_path Character (optional). File path to save the plot PNG.
#' @return A ggplot object.
plot_case2_survivor_age_profiles <- function(data, 
                                             target_horizon = 7, 
                                             target_age_class = ">=65", 
                                             save_path = NULL) {
  # Ensure data.table and isolate stratum safely
  dt <- as.data.table(data)
  df_sub <- copy(dt[horizon_years == target_horizon & pp_age_class == target_age_class])
  
  if (nrow(df_sub) == 0) {
    stop("No data found for the specified horizon and age class combinations.")
  }
  
  df_age_long <- melt(
    df_sub, 
    id.vars = "lambda", 
    measure.vars = c("mean_survivor_baseline_age_mean", "mean_survivor_baseline_age_p99", "mean_survivor_baseline_age_max", 
                     "mean_survivor_attained_age_mean", "mean_survivor_attained_age_p99", "mean_survivor_attained_age_max"),
    variable.name = "Metric", 
    value.name = "Age"
  )
  
  df_age_long[, `Age Type` := ifelse(grepl("baseline", Metric), 
                                     "Baseline Age (At Diagnosis)", 
                                     sprintf("Attained Age (At Year %d)", target_horizon))]
  
  df_age_long[, Statistic := factor(
    ifelse(grepl("_mean$", Metric), "Cohort Mean", 
           ifelse(grepl("_p99$", Metric), "99th Percentile (p99)", "Mean of Sample Maxima")),
    levels = c("Cohort Mean", "99th Percentile (p99)", "Mean of Sample Maxima")
  )]
  
  plot_case2 <- ggplot(df_age_long, aes(x = lambda, y = Age, color = Statistic, shape = Statistic)) +
    geom_line(linewidth = 1.1) + 
    geom_point(size = 3) +
    facet_wrap(~ `Age Type`, scales = "free_y") +
    scale_color_manual(values = c("#2c3e50", "#d95f02", "#e7298a")) +
    labs(
      title = sprintf("Case 2: Survivor Age Profiles at %d Years Post-Diagnosis (%s Cohort)", target_horizon, target_age_class),
      subtitle = "Baseline and attained age distributions (mean, 99th percentile, and sample maxima) across excess hazard rates",
      x = expression(paste("Excess Hazard Rate (", lambda, ")")), 
      y = "Patient Age (Years)", 
      color = NULL, 
      shape = NULL
    ) + 
    theme_minimal(base_size = 13) + 
    theme(
      plot.title = element_text(face = "bold"), 
      legend.position = "bottom",
      strip.background = element_rect(fill = "grey92", color = NA), 
      strip.text = element_text(face = "bold", size = 12)
    )
  
  # Optional disk export
  if (!is.null(save_path)) {
    ggsave(save_path, plot = plot_case2, width = 10, height = 6)
  }
  
  return(plot_case2)
}