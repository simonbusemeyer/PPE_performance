# Coefficient of Variation (CV)
metrics_cv <- metrics %>%
  mutate(
    cv_at_risk_overall = sd_n_at_risk / mean_n_at_risk,
    cv_at_risk_o85     = sd_n_at_risk_o85 / mean_n_at_risk_o85
  ) %>%
  select(time_t, lambda, cv_at_risk_overall, cv_at_risk_o85, estimation_error_sd) %>%
  pivot_longer(cols = c(cv_at_risk_overall, cv_at_risk_o85, estimation_error_sd),
               names_to = "Metric", values_to = "Value")

ggplot(metrics_cv, aes(x = time_t, y = Value, color = Metric)) +
  geom_line(linewidth = 1) +
  geom_point() +
  facet_wrap(~ lambda, scales = "free_y") +
  theme_minimal() +
  scale_color_manual(values = c("cv_at_risk_overall" = "blue", 
                                "cv_at_risk_o85" = "purple", 
                                "estimation_error_sd" = "red"),
                     labels = c("CV of Overall Risk Set", "CV of 85+ Risk Set", "SD of Estimation Error (Diff)")) +
  labs(title = "Instability Tracking: Risk Set Variance vs Estimation Error",
       subtitle = "Does a spike in sample variability correlate with a spike in error?",
       x = "Time (Years)", y = "Standardized Variability") +
  theme(legend.position = "bottom")