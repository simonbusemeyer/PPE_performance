# Focus on a specific follow-up time
plot_variance <- final_results %>%
  filter(time_t == 7) 

ggplot(plot_variance, aes(x = lambda)) +
  # IQR Ribbon
  geom_ribbon(aes(ymin = mean_n_at_risk - (iqr_n_at_risk/2), 
                  ymax = mean_n_at_risk + (iqr_n_at_risk/2)), 
              fill = "blue", alpha = 0.2) +
  # Mean Line
  geom_line(aes(y = mean_n_at_risk), color = "blue", linewidth = 1) +
  theme_minimal() +
  scale_x_log10() +
  labs(
    title = "Overall Risk Set Size at 7 Years across \u03BB",
    subtitle = "Solid line: Mean | Ribbon: Interquartile Range (IQR)",
    x = expression("Lambda ("*lambda*") [Log Scale]"),
    y = "Number of Patients at Risk"
  )