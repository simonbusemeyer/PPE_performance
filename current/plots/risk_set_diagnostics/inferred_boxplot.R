library(ggplot2)
library(dplyr)

# 85+ age group
ggplot(metrics, aes(x = time_t)) +
  geom_ribbon(aes(ymin = pmax(0, med_n_at_risk_o85 - (iqr_n_at_risk_o85/2)), 
                  ymax = med_n_at_risk_o85 + (iqr_n_at_risk_o85/2)), 
              fill = "#2980b9", alpha = 0.4) +
  # median line
  geom_line(aes(y = med_n_at_risk_o85), color = "#2c3e50", linewidth = 1) +
  facet_wrap(~ lambda, scales = "free_y") +
  theme_minimal() +
  labs(title = "Non-Parametric Attrition Track (85+ Age Group)",
       subtitle = "Solid line = Median; Shaded band = Approximated IQR (Middle 50% of simulations)",
       x = "Time (Years)", y = "Median Patients at Risk")