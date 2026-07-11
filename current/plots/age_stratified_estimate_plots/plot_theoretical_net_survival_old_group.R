# =============================================================================
# plot_theoretical_net_survival_old_group.R
# Neutral diagnostic plot of theoretical net survival for the Old Group (Age >= 65)
# =============================================================================

library(ggplot2)
library(data.table)

# Simulation Setup to match main simulation
set.seed(123)
lambda_grid <- seq(0.01, 0.50, by = 0.01)
beta_age    <- 0.02   
t_eval      <- 7
n_sim       <- 10000

# Recreate Luo Age Distribution
u <- runif(n_sim)
age_full <- numeric(n_sim)
age_full[u <= 0.462]                 <- runif(sum(u <= 0.462), min = 50, max = 65)
age_full[u > 0.462 & u <= 0.981]     <- runif(sum(u > 0.462 & u <= 0.981), min = 65, max = 85)
age_full[u > 0.981]                  <- runif(sum(u > 0.981), min = 85, max = 95)

# age Centered on full-cohort mean to match data-generating model.
theoretical_mean_cohort <- sum(c(0.462, 0.519, 0.020) * c(57.5, 75.0, 90.0))

# Isolate the Older Stratum (Age >= 65) & Center
old_group_ages <- age_full[age_full >= 65]
age_centre_old <- old_group_ages - theoretical_mean_cohort

# Vectorized Evaluation of Theoretical Net Survival
surv_mean <- sapply(lambda_grid, function(lam) {
  mean(exp(-lam * t_eval * exp(beta_age * age_centre_old)))
})

p50_age_c <- quantile(age_centre_old, 0.50)
surv_p50  <- sapply(lambda_grid, function(lam) exp(-lam * t_eval * exp(beta_age * p50_age_c)))

age90_c   <- 90.0 - theoretical_mean_cohort
surv_age90 <- sapply(lambda_grid, function(lam) exp(-lam * t_eval * exp(beta_age * age90_c)))

# Data.Table for Plotting
dt_plot <- data.table(
  lambda = rep(lambda_grid, 3),
  survival = c(surv_mean, surv_p50, surv_age90),
  metric = factor(
    rep(c("Older Stratum Average (mean S_net)", 
          sprintf("Stratum Median Age (~%.1f yrs)", p50_age_c + theoretical_mean_cohort), 
          "Very Old Profile (Fixed Age 90)"), 
        each = length(lambda_grid)),
    levels = c("Older Stratum Average (mean S_net)", 
               sprintf("Stratum Median Age (~%.1f yrs)", p50_age_c + theoretical_mean_cohort), 
               "Very Old Profile (Fixed Age 90)")
  )
)

# inspection values at lambda = 0.30
val_03_mean  <- surv_mean[which.min(abs(lambda_grid - 0.30))]
val_03_age90 <- surv_age90[which.min(abs(lambda_grid - 0.30))]
cat(sprintf("At lambda = 0.30 (7 Years) | Stratum Avg Surv: %.2f%% | Age 90 Profile Surv: %.2f%%\n", 
            val_03_mean * 100, val_03_age90 * 100))

p_old_group <- ggplot(dt_plot, aes(x = lambda, y = survival, color = metric, linetype = metric)) +
  geom_line(linewidth = 1.1) +
  geom_vline(xintercept = 0.30, linetype = "dashed", color = "#525252", linewidth = 0.8) +
  geom_point(data = dt_plot[lambda == 0.30], aes(x = lambda, y = survival), size = 3.5, show.legend = FALSE) +
  annotate("text", x = 0.30, y = val_03_mean + 0.07, 
           label = sprintf("lambda == 0.30~'('~S[net] == %.3f~')'", val_03_mean), 
           parse = TRUE, hjust = -0.05, color = "#2c3e50", size = 4.2, fontface = "bold") +
  annotate("text", x = 0.30, y = max(val_03_age90 + 0.05, 0.04), 
           label = sprintf("Age~90~'('~S[net] == %.3f~')'", val_03_age90), 
           parse = TRUE, hjust = -0.05, color = "#d95f02", size = 3.9) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), labels = scales::percent_format(accuracy = 1)) +
  scale_x_continuous(breaks = seq(0, 0.5, 0.05)) +
  scale_color_manual(values = c("#2c3e50", "#1b9e77", "#d95f02")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotdash")) +
  labs(
    title = "Theoretical 7-Year Net Survival: Older Patient Stratum (Age ≥ 65)",
    subtitle = "Checking whether theoretical net survival approaches zero at high excess hazard rates (λ = 0.30)",
    x = expression(paste("Excess Hazard Scale Parameter (", lambda, ")")),
    y = expression(paste("7-Year Theoretical Net Survival   ", S[net](7), )),
    color = "Cohort Profile",
    linetype = "Cohort Profile"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray30", size = 11),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p_old_group)
