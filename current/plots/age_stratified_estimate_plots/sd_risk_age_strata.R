library(ggplot2)
library(dplyr)

plot_sd_vs_risk_age_strata <- function(sim_data) {
  sim_data %>%
    # Isolate the specific age strata, excluding the "Overall" aggregate
    filter(pp_age_class %in% c("<65", ">=65")) %>% 
    ggplot(aes(x = mean_n_at_risk, 
               y = estimate_sd, 
               color = as.factor(lambda), 
               shape = as.factor(time_t))) +
    geom_point(alpha = 0.6, size = 2) +
    facet_wrap(~ pp_age_class, scales = "free_x") +
    labs(
      x = "Average Number at Risk",
      y = "Empirical Standard Deviation",
      title = "Empirical Variability vs. Risk Set Depletion",
      color = "Lambda",
      shape = "Time (t)"
  ) +
    theme_bw() +
    theme(
      strip.background = element_rect(fill = "grey95", color = NA),
      strip.text = element_text(face = "bold"),
      # Réduction de la taille de la légende
      legend.position = "right",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.key.size = unit(0.5, "lines") # Réduit l'espacement entre les éléments
    )
}

# Example usage:
# plot_sd_vs_risk_age_strata(df)