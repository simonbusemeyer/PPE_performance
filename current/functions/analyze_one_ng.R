# =============================================================================
# analyze_one.R
# =============================================================================

analyze_one <- function(df, lambda, beta_age, max_time) {
  times_years <- sort(unique(c(1, 4, ceiling((max_time + 1)/2), max_time)))
  times_days <- times_years * 365.241
  
  df$pp_age_class <- cut(df$age, breaks = c(0, 65, Inf), right = FALSE, labels = c("<65", ">=65"))
  df$pp_age_class <- droplevels(df$pp_age_class)
  
  # --- 1. POHAR-PERME ESTIMATIONS ---
  # Stratified Fit
  pp_fit_strat <- rs.surv(
    Surv(observed_time*365.241, status) ~ pp_age_class,
    data = df, ratetable = survexp.us, 
    rmap = list(age = age * 365.241, sex = sex, year = year_diagnosis), 
    method = "pohar-perme", add.times = times_days
  )
  pp_summ_strat <- summary(pp_fit_strat, times = times_days, extend = TRUE) 
  
  # Overall Fit
  pp_fit_overall <- rs.surv(
    Surv(observed_time*365.241, status) ~ 1,
    data = df, ratetable = survexp.us, 
    rmap = list(age = age * 365.241, sex = sex, year = year_diagnosis), 
    method = "pohar-perme", add.times = times_days
  )
  pp_summ_overall <- summary(pp_fit_overall, times = times_days, extend = TRUE) 
  
  # Combine PP Estimates
  pp_df <- rbind(
    data.frame(
      time_days      = pp_summ_strat$time,
      time           = pp_summ_strat$time / 365.241,
      pp_age_class   = sub("^pp_age_class=", "", as.character(pp_summ_strat$strata)),
      net_surv_pp    = pp_summ_strat$surv,
      se             = pp_summ_strat$std.err,
      net_surv_lower = pp_summ_strat$lower,
      net_surv_upper = pp_summ_strat$upper
    ),
    data.frame(
      time_days      = pp_summ_overall$time,
      time           = pp_summ_overall$time / 365.241,
      pp_age_class   = "Overall",
      net_surv_pp    = pp_summ_overall$surv,
      se             = pp_summ_overall$std.err,
      net_surv_lower = pp_summ_overall$lower,
      net_surv_upper = pp_summ_overall$upper
    )
  )
  pp_df$time <- times_years[match(round(pp_df$time, 8), round(times_years, 8))]
  
  # --- 2. THEORETICAL NET SURVIVAL ---
  theo_list <- lapply(split(df, df$pp_age_class), function(dfg){
    data.frame(
      pp_age_class = as.character(unique(dfg$pp_age_class)),
      time = times_years,
      net_surv_theo = sapply(times_years, function(t) mean(exp(-lambda * t * exp(beta_age * dfg$ageCentre))))
    )
  })
  
  theo_overall <- data.frame(
    pp_age_class = "Overall",
    time = times_years,
    net_surv_theo = sapply(times_years, function(t) mean(exp(-lambda * t * exp(beta_age * df$ageCentre))))
  )
  theo_df <- do.call(rbind, c(theo_list, list(theo_overall)))
  
  # --- 3. DIAGNOSTICS ---
  calc_diag <- function(dfg, group_name) {
    
    # 1. Global Anchors (Path B) - Calculated over the entire follow-up for this subgroup
    n_deaths_cancer_global <- sum(dfg$status == 1 & dfg$event_type == "cancer")
    n_deaths_other_global  <- sum(dfg$status == 1 & dfg$event_type == "other")
    n_deaths_total_global  <- n_deaths_cancer_global + n_deaths_other_global
    
    global_pct_cancer <- ifelse(n_deaths_total_global > 0, n_deaths_cancer_global / n_deaths_total_global, NA_real_)
    global_cens_rate  <- mean(dfg$status == 0)
    
    # 2. Time-bounded Metrics - Calculated strictly up to time 't'
    do.call(rbind, lapply(times_years, function(t) {
      
      cum_cancer   <- sum(dfg$observed_time < t & dfg$event_type == "cancer")
      cum_other    <- sum(dfg$observed_time < t & dfg$event_type == "other")
      cum_total    <- sum(dfg$observed_time < t & dfg$status == 1)
      cum_censored <- sum(dfg$observed_time < t & dfg$event_type == "censored")
      
      data.frame(
        pp_age_class      = group_name,
        time              = t,
        n_patients        = nrow(dfg),
        n_at_risk         = sum(dfg$observed_time >= t),
        cum_cancer        = cum_cancer,
        cum_other         = cum_other,
        cum_total_deaths  = cum_total,
        cum_censored      = cum_censored,
        n_deaths_cancer   = n_deaths_cancer_global, 
        n_deaths_other    = n_deaths_other_global,  
        pct_cancer        = global_pct_cancer,     
        cens_rate         = global_cens_rate      
      )
    }))
  }
  
  diag_strat <- lapply(levels(df$pp_age_class), function(grp) calc_diag(df[df$pp_age_class == grp, ], grp))
  diag_overall <- calc_diag(df, "Overall")
  diag_df <- do.call(rbind, c(diag_strat, list(diag_overall)))
  
  # --- 4. MERGE ---
  res <- merge(pp_df, theo_df, by = c("pp_age_class", "time"), all.x = TRUE)
  res <- merge(res, diag_df, by = c("pp_age_class", "time"), all.x = TRUE)
  
  res$diff <- res$net_surv_pp - res$net_surv_theo
  res$covered <- with(res, net_surv_lower <= net_surv_theo & net_surv_theo <= net_surv_upper)
  
  res$pp_age_class <- factor(res$pp_age_class, levels = c("<65", ">=65", "Overall"))
  res <- res[order(res$pp_age_class, res$time), ]
  rownames(res) <- NULL
  
  return(res)
}