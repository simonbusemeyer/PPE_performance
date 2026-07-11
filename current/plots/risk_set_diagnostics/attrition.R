# Plot age group depletion for a specific lambda
plot_age <- final_results %>%
  filter(lambda == 0.3) %>%
  select(time_t, mean_n_at_risk_u65, mean_n_at_risk_65_85, mean_n_at_risk_o85) %>%
  pivot_longer(cols = -time_t, names_to = "Age_Group", values_to = "Mean_At_Risk") %>%
  mutate(Age_Group = case_when(
    grepl("u65", Age_Group) ~ "< 65",
    grepl("65_85", Age_Group) ~ "65 - 85",
    grepl("o85", Age_Group) ~ "85+"
  ))

ggplot(plot_age, aes(x = time_t, y = Mean_At_Risk, color = Age_Group, group = Age_Group)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = "Effective Risk Set Depletion by Age Group (\u03BB = 0.018)",
    x = "Years Since Diagnosis",
    y = "Average Number at Risk",
    color = "Age Group"
  )


risk_data_long <- metrics %>%
  select(time_t, lambda, 
         `<65_mean` = mean_n_at_risk_u65, `<65_sd` = sd_n_at_risk_u65,
         `65-85_mean` = mean_n_at_risk_65_85, `65-85_sd` = sd_n_at_risk_65_85,
         `85+_mean` = mean_n_at_risk_o85, `85+_sd` = sd_n_at_risk_o85) %>%
  pivot_longer(cols = -c(time_t, lambda), 
               names_to = c("age_group", ".value"), 
               names_sep = "_")

# Plot
ggplot(risk_data_long, aes(x = time_t, color = age_group, fill = age_group)) +
  geom_line(aes(y = mean), linewidth = 1) +
  geom_ribbon(aes(ymin = pmax(0, mean - sd), ymax = mean + sd), alpha = 0.2, color = NA) +
  facet_wrap(~ lambda, scales = "free_y") +
  theme_minimal() +
  labs(title = "Average Risk Set Depletion by Age Group",
       subtitle = "Shaded areas represent ±1 SD across 1000 simulations",
       x = "Time (Years)", y = "Number of Patients at Risk",
       color = "Age Group", fill = "Age Group") +
  theme(legend.position = "bottom")