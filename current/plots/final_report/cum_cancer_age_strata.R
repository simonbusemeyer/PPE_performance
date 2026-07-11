# cum_cancer_age_strata.R

plot_cum_cancer_by_age <- function(
    results_df,
    title = "Cumulative Cancer Deaths by Age Class",
    x_lab = "Proportion of Cohort Experiencing Cancer Death vs Non-Cancer Death",
    y_lab = "Mean Cumulative Cancer Deaths",
    followup_lab = "Follow-up"
) {
  
  required_cols <- c("time_t", "pct_cancer", "mean_cum_cancer", "mean_n_patients", "pp_age_class")
  missing_cols <- setdiff(required_cols, names(results_df))
  
  if (length(missing_cols) > 0) {
    stop(
      "results_df is missing required column(s): ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  results_plot_df <- results_df |> 
    # Exclude the Overall strata for this specific plot
    dplyr::filter(pp_age_class != "Overall") |>
    # Calculate the proportion on the y-axis
    dplyr::mutate(
      cum_cancer = mean_cum_cancer,
      time_t_factor = factor(
        time_t,
        levels = sort(unique(time_t)),
        labels = paste("Year", sort(unique(time_t)))
      )
    )
  
  num_shapes <- length(unique(results_plot_df$time_t_factor))
  
  ggplot2::ggplot(
    results_plot_df,
    ggplot2::aes(x = pct_cancer, y = cum_cancer)
  ) +
    ggplot2::geom_point(
      ggplot2::aes(
        color = time_t_factor,
        shape = time_t_factor
      ),
      size = 3
    ) +
    ggplot2::facet_wrap(~ pp_age_class, scales = "fixed") +
    ggplot2::scale_color_viridis_d(option = "plasma", end = 0.8) +
    ggplot2::scale_shape_manual(values = 0:(num_shapes - 1), name = followup_lab) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::comma
    ) +
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
      strip.background = ggplot2::element_rect(fill = "grey90", color = NA),
      strip.text = ggplot2::element_text(face = "bold", size = 12)
    )
}