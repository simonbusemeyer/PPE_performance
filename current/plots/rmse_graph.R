library(tidyverse)
library(scales)
library(here)

results_df <- read_csv(here("current", "outputs", "tables", "final_results_complete.csv"))

# Convert time_t to a categorical factor
results_df <- results_df %>%
  mutate(time_t_factor = factor(time_t, labels = paste("Year", sort(unique(time_t)))))

ggplot(results_df, aes(x = pct_cancer, y = rmse)) +
  # Reference line at 0 (representing an ideal estimator with zero error)
  geom_hline(aes(yintercept = 0, linetype = "Ideal (No Error)"), color = "darkred", alpha = 0.8) +
  # Scatter points stratified by both color and shape
  geom_point(aes(color = time_t_factor, shape = time_t_factor), size = 3) +
  
  # Scales & Styling
  scale_color_viridis_d(option = "plasma", end = 0.8) + 
  scale_shape_discrete() + 
  scale_linetype_manual(values = c("Ideal (No Error)" = "dashed")) +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  # Adjusted y-axis breaks for evaluating precise variations in RMSE
  scale_y_continuous(breaks = seq(0, 0.3, by = 0.05)) +
  coord_cartesian(ylim = c(0, 0.15)) +
  
  # Labels
  labs(
    title = "PPE Root Mean Square Error Under Competing Risks",
    x = "Proportion of Cohort Experiencing Cancer Death vs Non-Cancer Death",
    y = "RMSE",
    color = "Follow-up",
    shape = "Follow-up",
    linetype = NULL 
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    
    axis.title.x = element_text(size = 14),
    axis.text.x  = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text  = element_text(size = 14),
    
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )


