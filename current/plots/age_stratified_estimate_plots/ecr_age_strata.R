plot_ecr_by_age <- function(
    results_df,
    title = "PPE ECR Under Competing Risks by Age Class",
    x_lab = "Proportion of Cohort Experiencing Cancer Death vs Non-Cancer Death",
    y_lab = "ECR",
    followup_lab = "Follow-up"
) {
  
  # Added pp_age_class to required columns
  required_cols <- c("time_t", "pct_cancer", "ecr", "pp_age_class")
  missing_cols <- setdiff(required_cols, names(results_df))
  
  if (length(missing_cols) > 0) {
    stop(
      "results_df is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  results_plot_df <- results_df %>%
    dplyr::filter(pp_age_class != "Overall") |>
    
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
      ggplot2::aes(yintercept = 0.95, linetype = "Nominal Target (95%)"),
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
    # Facet by age class to create comparison panels
    ggplot2::facet_wrap(~ pp_age_class, scales = "fixed") +
    
    ggplot2::scale_color_viridis_d(option = "plasma", end = 0.8) +
    ggplot2::scale_shape_manual(values = 0:(num_shapes - 1), name = followup_lab) +
    ggplot2::scale_linetype_manual(
      values = c("No ECR" = "dashed")
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1)
    # ) +
    # ggplot2::scale_y_continuous(
    #   breaks = seq(-0.2, 0.2, by = 0.1)
    ) +
    # ggplot2::coord_cartesian( # uncomment for strict zooming if needed
    #   ylim = c(-0.02, 0.02)
    # ) +
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
      panel.grid.minor = ggplot2::element_blank(),
      # Add styling for the facet labels
      strip.background = ggplot2::element_rect(fill = "grey90", color = NA),
      strip.text = ggplot2::element_text(face = "bold", size = 12)
    )
}