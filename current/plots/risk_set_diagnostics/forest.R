library(ggplot2)
library(dplyr)

# Filter for a time point
metrics_t7 <- metrics %>%
  filter(time_t == 7)

ggplot(metrics_t7, aes(x = factor(lambda), y = med_n_at_risk_o85)) +
  # Use pointrange to show Median and approximated IQR bounds
  geom_pointrange(aes(ymin = pmax(0, med_n_at_risk_o85 - (iqr_n_at_risk_o85/2)), 
                      ymax = med_n_at_risk_o85 + (iqr_n_at_risk_o85/2)),
                  color = "#2c3e50", linewidth = 1, fatten = 4) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Risk Set Size at 7 Years by Background Hazard",
       subtitle = "Point = Median | Error Bars = Appoximated IQR across 1000 simulations",
       x = expression("Background Hazard Scenario (" * lambda * ")"),
       y = "Patients at Risk (85+ Age Group)") +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank())