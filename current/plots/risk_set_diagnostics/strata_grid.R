library(tidyr)

# long format
age_strata_long <- metrics %>%
  select(time_t, lambda, 
         mean_n_at_risk_u65, sd_n_at_risk_u65,
         mean_n_at_risk_65_85, sd_n_at_risk_65_85,
         mean_n_at_risk_o85, sd_n_at_risk_o85) %>%
  pivot_longer(cols = -c(time_t, lambda),
               names_to = c(".value", "age_group"),
               names_pattern = "(mean|sd)_n_at_risk_(.*)") %>%
  mutate(age_group = factor(age_group, levels = c("u65", "65_85", "o85"), 
                            labels = c("< 65", "65 - 85", "85+")))

# Filter for a subset
lambdas_to_plot <- c(0.0005, 0.005, 0.019, 0.055, 0.10)

ggplot(age_strata_long %>% filter(lambda %in% lambdas_to_plot), 
       aes(x = time_t)) +
  geom_ribbon(aes(ymin = pmax(0, mean - sd), ymax = mean + sd, fill = age_group), alpha = 0.3) +
  geom_line(aes(y = mean, color = age_group), linewidth = 1) +
  # Create a matrix: Lambda (cols) x Age Group (rows)
  facet_grid(age_group ~ lambda, scales = "free_y") +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_bw() +
  labs(title = "Stratified Attrition Trajectories (Mean ± 1 SD)",
       x = "Time (Years)",
       y = "Patients at Risk") +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "grey95"))




