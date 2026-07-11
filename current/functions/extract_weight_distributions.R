# =============================================================================
# extract_weight_distributions.R
# Computes age-stratified Pohar-Perme weight distributions, baseline age, and attained age
# =============================================================================

library(survival)
library(data.table)

extract_weight_metrics <- function(df, horizons = c(1, 4, 7)) {
  
  # check if input is a data.table without altering external objects in parent environment
  dt <- as.data.table(df)
  
  # check if age class exists and is formatted as a factor with set levels
  if (!"pp_age_class" %in% names(dt)) {
    dt[, pp_age_class := cut(age, breaks = c(0, 65, Inf), right = FALSE, labels = c("<65", ">=65"))]
  }
  dt[, pp_age_class := factor(pp_age_class, levels = c("<65", ">=65"))]
  
  results_list <- list()
  
  for (t_yr in horizons) {
    
    # 1. Identify risk set strictly at time t
    at_risk <- dt[observed_time >= t_yr]
    if (nrow(at_risk) == 0) next
    
    t_days <- t_yr * 365.241
    
    # 2. Exact individual expected population survival Sp(t | z_i)
    sp_indiv <- survival::survexp(
      Surv(rep(t_days, nrow(at_risk)), rep(1, nrow(at_risk))) ~ 1, 
      data = at_risk, 
      ratetable = survexp.us, 
      rmap = list(
        age  = age * 365.241, 
        sex  = sex, 
        year = year_diagnosis
      ), 
      times = t_days, 
      cohort = FALSE
    )
    
    sp_indiv <- as.numeric(sp_indiv)
    if (length(sp_indiv) != nrow(at_risk)) {
      stop(sprintf(
        "ERROR at horizon %d: Expected %d individual expected survival values, but survexp returned %d.",
        t_yr, nrow(at_risk), length(sp_indiv)
      ))
    }
    
    # 3. compute attained age
    at_risk[, weight := 1 / pmax(sp_indiv, 1e-8)]
    at_risk[, attained_age := age + t_yr]
    
    # 4. data.table aggregation by age stratum ONLY
    strat_summary <- at_risk[, .(
      n_at_risk                  = .N,
      weight_median              = quantile(weight, 0.50, na.rm = TRUE, names = FALSE),
      weight_iqr                 = IQR(weight, na.rm = TRUE),
      weight_p90                 = quantile(weight, 0.90, na.rm = TRUE, names = FALSE),
      weight_p95                 = quantile(weight, 0.95, na.rm = TRUE, names = FALSE),
      weight_p99                 = quantile(weight, 0.99, na.rm = TRUE, names = FALSE),
      weight_max                 = max(weight, na.rm = TRUE),
      
      # Baseline Age Demographics
      survivor_mean_baseline_age = mean(age, na.rm = TRUE),
      survivor_p95_baseline_age  = quantile(age, 0.95, na.rm = TRUE, names = FALSE),
      survivor_p99_baseline_age  = quantile(age, 0.99, na.rm = TRUE, names = FALSE),
      survivor_max_baseline_age  = max(age, na.rm = TRUE),
      
      # Attained Age Demographics
      survivor_mean_attained_age = mean(attained_age, na.rm = TRUE),
      survivor_p95_attained_age  = quantile(attained_age, 0.95, na.rm = TRUE, names = FALSE),
      survivor_p99_attained_age  = quantile(attained_age, 0.99, na.rm = TRUE, names = FALSE),
      survivor_max_attained_age  = max(attained_age, na.rm = TRUE)
    ), by = .(pp_age_class)]
    
    # Add horizon_years after aggregation
    strat_summary[, horizon_years := t_yr]
    strat_summary[, pp_age_class := as.character(pp_age_class)]
    
    # 5. Create Overall summary
    overall_summary <- at_risk[, .(
      pp_age_class               = "Overall",
      n_at_risk                  = .N,
      weight_median              = quantile(weight, 0.50, na.rm = TRUE, names = FALSE),
      weight_iqr                 = IQR(weight, na.rm = TRUE),
      weight_p90                 = quantile(weight, 0.90, na.rm = TRUE, names = FALSE),
      weight_p95                 = quantile(weight, 0.95, na.rm = TRUE, names = FALSE),
      weight_p99                 = quantile(weight, 0.99, na.rm = TRUE, names = FALSE),
      weight_max                 = max(weight, na.rm = TRUE),
      survivor_mean_baseline_age = mean(age, na.rm = TRUE),
      survivor_p95_baseline_age  = quantile(age, 0.95, na.rm = TRUE, names = FALSE),
      survivor_p99_baseline_age  = quantile(age, 0.99, na.rm = TRUE, names = FALSE),
      survivor_max_baseline_age  = max(age, na.rm = TRUE),
      survivor_mean_attained_age = mean(attained_age, na.rm = TRUE),
      survivor_p95_attained_age  = quantile(attained_age, 0.95, na.rm = TRUE, names = FALSE),
      survivor_p99_attained_age  = quantile(attained_age, 0.99, na.rm = TRUE, names = FALSE),
      survivor_max_attained_age  = max(attained_age, na.rm = TRUE)
    )]
    overall_summary[, horizon_years := t_yr]
    
    results_list[[as.character(t_yr)]] <- rbind(strat_summary, overall_summary, use.names = TRUE)
  }
  
  res_df <- rbindlist(results_list, use.names = TRUE, fill = TRUE)
  res_df[, pp_age_class := factor(pp_age_class, levels = c("<65", ">=65", "Overall"))]
  setcolorder(res_df, c("horizon_years", "pp_age_class"))
  
  return(res_df)
}