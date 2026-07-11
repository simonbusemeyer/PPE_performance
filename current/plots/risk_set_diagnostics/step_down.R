event_prop <- metrics %>%
  mutate(
    total_events = mean_cum_cancer + mean_cum_other + mean_cum_censored  )

ggplot(metrics, aes(x = time_t)) +
  geom_line(aes(y = mean_cum_cancer), color = "#c0392b", linewidth = 1) +
  geom_ribbon(aes(ymin = pmax(0, med_cum_cancer - (iqr_cum_cancer/2)), 
                  ymax = med_cum_cancer + (iqr_cum_cancer/2)), 
              fill = "#c0392b", alpha = 0.2) +
  facet_wrap(~ lambda, scales = "free_y") +
  theme_minimal() +
  labs(title = "Cumulative cancer deaths and dispersion",
       subtitle = "line = mean | ribbon = IQR (n_files Dispersion)",
       x = "Time (years)", y = "number of cumulative cancer deaths")