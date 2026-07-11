# =============================================================================
# compute_metrics.R
# =============================================================================

compute_metrics <- function(results_list, lambda_val, borne_a_val) {
  dt <- data.table::rbindlist(results_list)
  
  metrics <- dt[, .(
    absolute_bias       = abs(mean(diff, na.rm = TRUE)),
    bias                = mean(diff, na.rm = TRUE),
    rmse                = sqrt(mean(diff^2, na.rm = TRUE)),
    estimate_sd         = sd(net_surv_pp, na.rm = TRUE),
    estimation_error_sd = sd(diff, na.rm = TRUE),
    mean_se             = mean(se, na.rm = TRUE),
    ecr                 = mean(covered, na.rm = TRUE),
    
    # Stratified and Overall base metrics calculated natively
    pct_cancer          = mean(pct_cancer, na.rm = TRUE),
    censoring_rate      = mean(cens_rate, na.rm = TRUE),
    n_deaths_cancer     = mean(n_deaths_cancer, na.rm = TRUE),
    n_deaths_other      = mean(n_deaths_other, na.rm = TRUE),
    
    # Risk-set diagnostics
    mean_n_patients        = mean(n_patients, na.rm = TRUE),
    mean_n_at_risk         = mean(n_at_risk, na.rm = TRUE),
    sd_n_at_risk           = sd(n_at_risk, na.rm = TRUE),
    med_n_at_risk          = median(n_at_risk, na.rm = TRUE),
    iqr_n_at_risk          = IQR(n_at_risk, na.rm = TRUE),
    
    mean_cum_cancer        = mean(cum_cancer, na.rm = TRUE),
    sd_cum_cancer          = sd(cum_cancer, na.rm = TRUE),
    med_cum_cancer         = median(cum_cancer, na.rm = TRUE),
    iqr_cum_cancer         = IQR(cum_cancer, na.rm = TRUE),
    
    mean_cum_other         = mean(cum_other, na.rm = TRUE),
    sd_cum_other           = sd(cum_other, na.rm = TRUE),
    med_cum_other          = median(cum_other, na.rm = TRUE),
    iqr_cum_other          = IQR(cum_other, na.rm = TRUE),
    
    mean_cum_total_deaths  = mean(cum_total_deaths, na.rm = TRUE),
    sd_cum_total_deaths    = sd(cum_total_deaths, na.rm = TRUE),
    med_cum_total_deaths   = median(cum_total_deaths, na.rm = TRUE),
    iqr_cum_total_deaths   = IQR(cum_total_deaths, na.rm = TRUE),
    
    mean_cum_censored      = mean(cum_censored, na.rm = TRUE),
    sd_cum_censored        = sd(cum_censored, na.rm = TRUE),
    med_cum_censored       = median(cum_censored, na.rm = TRUE),
    iqr_cum_censored       = IQR(cum_censored, na.rm = TRUE)
    
  ), by = .(pp_age_class, time_t = time)]
  
  metrics[, se_calibration_ratio := mean_se / estimation_error_sd]
  
  # Only assign the overarching scenario parameters here
  metrics[, `:=`(
    lambda  = lambda_val,
    borne_a = borne_a_val
  )]
  
  data.table::setcolorder(metrics, c("lambda", "borne_a", "pp_age_class", "time_t", 
                                     "censoring_rate", "pct_cancer", "n_deaths_cancer", "n_deaths_other"))
  
  return(as.data.frame(metrics))
}