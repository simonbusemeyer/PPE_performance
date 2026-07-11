ggplot(metrics, aes(x = med_n_at_risk_o85, y = estimation_error_sd)) +
  # Draw the trajectory path
  geom_path(aes(color = time_t), linewidth = 1, arrow = arrow(length = unit(0.15, "inches"))) +
  geom_point(aes(color = time_t), size = 2) +
  facet_wrap(~ lambda, scales = "free") +
  # Reverse X-axis because the sample size shrinks over time
  scale_x_reverse() +
  scale_color_viridis_c(option = "mako", direction = -1) +
  theme_minimal() +
  labs(title = "Phase Space: Estimator Variance vs. Effective Sample Size",
       subtitle = "Following the cohort over time (color) as sample shrinks (moving right to left)",
       x = "Median Patients at Risk (85+)",
       y = "Standard Deviation of Estimation Error (Diff)",
       color = "Time (Years)") +
  theme(legend.position = "bottom")