plot_mean_se <- function(
    results_df,
    title = "PPE Mean Estimated Standard Error",
    x_lab = "Proportion of Cohort Experiencing Cancer Death vs Non-Cancer Death",
    y_lab = "Mean Estimated SE",
    followup_lab = "Follow-up"
) {
  
  required_cols <- c("time_t", "pct_cancer", "mean_se")
  missing_cols <- setdiff(required_cols, names(results_df))
  
  if (length(missing_cols) > 0) {
    stop(
      "results_df is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  results_plot_df <- results_df %>%
    dplyr::mutate(
      time_t_factor = factor(
        time_t,
        levels = sort(unique(time_t)),
        labels = paste("Year", sort(unique(time_t)))
      )
    )
  
  num_shapes <- length(unique(results_plot_df$time_t_factor))
  
  ggplot2::ggplot(
    results_plot_df,
    ggplot2::aes(x = pct_cancer, y = mean_se)
  ) +
    ggplot2::geom_point(
      ggplot2::aes(
        color = time_t_factor,
        shape = time_t_factor
      ),
      size = 3
    ) +
    ggplot2::scale_color_viridis_d(option = "plasma", end = 0.8, name = followup_lab) +
    ggplot2::scale_shape_manual(values = 0:(num_shapes - 1), name = followup_lab) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1)
    ) +
    ggplot2::labs(
      title = title,
      x = x_lab,
      y = y_lab
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5)
    )
}