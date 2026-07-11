# =============================================================================
# nessie.R
# =============================================================================
library(survival)
library(relsurv)

determine_max_time <- function(n_patients, age_option, prop_female, year.start_min, year.start_max) {
  
  # 1. Generate Demographic Skeleton to obtain the age, sex, and year_diagnosis distributions
  df_skeleton <- generate_data(
    lambda = 0.001,       
    age_option = age_option, 
    n = n_patients, 
    max_time = 100,       
    prop_female = prop_female,
    year.start_min = year.start_min, 
    year.start_max = year.start_max,
    beta_sex = 0, 
    beta_age = 0, 
    borne_a = Inf        
  )
  
  # 2. Force a 100-year theoretical follow-up to observe the cohort's entire natural lifespan.
  df_skeleton$observed_time <- 100
  df_skeleton$status <- 0
  
  # 3. Stratify ages
 # breaks <- pretty(df_skeleton$age, n = 5)
#  df_skeleton$agegr <- cut(df_skeleton$age, breaks = breaks, include.lowest = TRUE)
  
 # Create a dummy grouping variable to bypass the nessie() ~1 bug for averaging c.exp.surv
  df_skeleton$dummy_group <- sample(c("A", "B"), size = nrow(df_skeleton), replace = TRUE)  
  # 4. Evaluate conditional expected survival over a century
  nessie_out <- relsurv::nessie(
    Surv(observed_time * 365.241, status) ~ dummy_group, #not subsetting for now
    #Surv(observed_time * 365.241, status) ~ sex + agegr, 
    data = df_skeleton,
    ratetable = survexp.us,
    times = seq(0, 100, by = 1), # Sequence required to map the drop-off point
    rmap = list(age = age * 365.241, sex = sex, year = year_diagnosis)
  )
  
  # 5. Extract and truncate
  nessie_matrix <- nessie_out$mata
  calc_max_time <- ceiling((mean(nessie_matrix[, "c.exp.surv"], na.rm = TRUE))/2)
  
  return(calc_max_time)
}

