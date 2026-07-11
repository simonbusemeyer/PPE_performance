# Calculate the Coefficient of Variation for the IQR
metrics <- metrics %>%
  mutate(iqr_cv_overall = iqr_n_at_risk / med_n_at_risk)

ggplot(metrics, aes(x = factor(time_t), y = factor(lambda), fill = iqr_cv_overall)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "grey90") +
  theme_minimal() +
  labs(title = "Relative Risk-Set Instability Matrix",
       subtitle = "Color intensity represents the IQR relative to the Median",
       x = "Evaluation Horizon (Years)", 
       y = expression("Background Hazard (" * lambda * ")"),
       fill = "Relative IQR\nDispersion") +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))