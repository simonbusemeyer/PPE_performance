library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Filter for a specific follow up horizon
plot_composition <- final_results %>%
  filter(time_t == 7) %>%
  select(lambda, mean_n_at_risk, mean_cum_cancer, mean_cum_other, mean_cum_censored) %>%
  pivot_longer(cols = -lambda, names_to = "Status", values_to = "Count") %>%
  mutate(Status = factor(Status, 
                         levels = c("mean_n_at_risk", "mean_cum_censored", "mean_cum_other", "mean_cum_cancer"),
                         labels = c("Still at Risk", "Censored", "Other Cause Dead", "Cancer Dead")))

ggplot(plot_composition, aes(x = as.factor(lambda), y = Count, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#2ca02c", "#7f7f7f", "#1f77b4", "#d62728")) +
  theme_minimal() +
  labs(
    title = "Average Cohort Composition at 7 Years",
    x = expression(lambda),
    y = "Number of Patients",
    fill = "Patient Status"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# long format
events_long <- metrics %>%
  select(time_t, lambda, mean_n_at_risk, mean_cum_cancer, mean_cum_other, mean_cum_censored) %>%
  rename(
    `Still at Risk` = mean_n_at_risk,
    `Cancer Deaths` = mean_cum_cancer,
    `Other Deaths` = mean_cum_other,
    `Censored` = mean_cum_censored
  ) %>%
  pivot_longer(cols = -c(time_t, lambda), names_to = "Status", values_to = "Count") %>%

  mutate(Status = factor(Status, levels = c("Still at Risk", "Censored", "Other Deaths", "Cancer Deaths")))

ggplot(events_long, aes(x = time_t, y = Count, fill = Status)) +
  geom_area(alpha = 0.85, color = "white", linewidth = 0.2) +
  facet_wrap(~ lambda) +
  scale_fill_manual(values = c("Still at Risk" = "#2c3e50", "Censored" = "#bdc3c7", 
                               "Other Deaths" = "#e67e22", "Cancer Deaths" = "#c0392b")) +
  theme_minimal() +
  labs(title = "Cohort Status Over Time",
       x = "Time (Years)", y = "Average Number of Patients") +
  theme(legend.position = "right")