ggplot(metrics, aes(x = time_t)) +
  # Parametric Spread
  geom_ribbon(aes(ymin = pmax(0, mean_n_at_risk_o85 - sd_n_at_risk_o85), 
                  ymax = mean_n_at_risk_o85 + sd_n_at_risk_o85), 
              fill = "#e74c3c", alpha = 0.2) +
  # Non-Parametric Spread
  geom_ribbon(aes(ymin = pmax(0, med_n_at_risk_o85 - (iqr_n_at_risk_o85/2)), 
                  ymax = med_n_at_risk_o85 + (iqr_n_at_risk_o85/2)), 
              fill = "#3498db", alpha = 0.4) +
  # Trend lines
  geom_line(aes(y = mean_n_at_risk_o85), color = "#c0392b", linetype = "dashed", linewidth = 0.8) +
  geom_line(aes(y = med_n_at_risk_o85), color = "#2980b9", linewidth = 0.8) +
  facet_wrap(~ lambda, scales = "free_y") +
  theme_minimal() +
  labs(title = "Distribution Skewness Detection: Mean vs Median",
       subtitle = "Red = Mean ± SD | Blue = Median ± IQR",
       x = "Time (Years)", y = "Patients at Risk (85+)")