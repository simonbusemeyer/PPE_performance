plot_ecr <- function(
    results_df,
    nominal_target = 0.95,
    title = "PPE Empirical Coverage Under Competing Risks",
    x_lab = "Proportion of Cohort Experiencing Cancer Death vs Non-Cancer Death",
    y_lab = "Empirical Coverage Rate",
    followup_lab = "Follow-up"
) {
  
  required_cols <- c("time_t", "pct_cancer", "ecr")
  missing_cols <- setdiff(required_cols, names(results_df))
  
  if (length(missing_cols) > 0) {
    stop(
      "results_df is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  target_label <- paste0(
    "Nominal Target (",
    scales::percent(nominal_target, accuracy = 1),
    ")"
  )
  
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
    ggplot2::aes(x = pct_cancer, y = ecr)
  ) +
    ggplot2::geom_hline(
      ggplot2::aes(
        yintercept = nominal_target,
        linetype = target_label
      ),
      color = "darkred",
      alpha = 0.8
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        color = time_t_factor,
        shape = time_t_factor
      ),
      size = 3
    ) +
    ggplot2::scale_color_viridis_d(option = "plasma", end = 0.8) +
    ggplot2::scale_shape_manual(values = 0:(num_shapes - 1), name = followup_lab) +
    ggplot2::scale_linetype_manual(
      values = stats::setNames("dashed", target_label)
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1)
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq(0.0, 1.0, by = 0.1)
    ) +
    ggplot2::coord_cartesian(ylim = c(0.0, 1.0)) +
    ggplot2::labs(
      title = title,
      x = x_lab,
      y = y_lab,
      color = followup_lab,
      shape = followup_lab,
      linetype = NULL
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
}