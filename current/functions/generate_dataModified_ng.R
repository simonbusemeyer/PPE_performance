# =============================================================================
# generate_data.R
# Fonctions de génération de données de survie
# =============================================================================

generate_data <- function(lambda,
                          age_option,
                          n,
                          max_time,
                          prop_female,
                          year.start_min,
                          year.start_max,
                          beta_sex,
                          beta_age,
                          borne_a) {
  
  # Covariables generation: Age
  if (age_option == "A") {
    # Option A: age ~ Uniform[80, 90)
    age <- runif(n, min = 80, max = 90)
    
  } else if (age_option == "C") {
    # Option C: age ~ Uniform[15, 40)
    age <- runif(n, min = 15, max = 40)
    
  } else if (age_option == "D") {
    # Option D: age ~ Uniform[50, 75)
    age <- trunc(runif(n, min = 50, max = 75))
    
  } else if (age_option == "E") {
    # Option E: age ~ Uniform[50, 60)
    age <- runif(n, min = 50, max = 60)
    
  } else if (age_option == "F") {
    # Option F: age ~ Discrete-Uniform[80, 90)
    age <- trunc(runif(n, min = 80, max = 90))
    
  } else if (age_option == "Luo") {
    # Option Luo: Empirical distribution from Luo et al. 2023 (Prostate Cancer)
    # <65 (46.2%), 65-85 (51.9%), >85 (2.0%)
    u <- runif(n)
    age <- numeric(n)
    
    idx1 <- u <= 0.462
    idx2 <- u > 0.462 & u <= 0.981
    idx3 <- u > 0.981
    
    # clinical bounds: [50-65), [65-85), [85-95)
    age[idx1] <- runif(sum(idx1), min = 50, max = 65)
    age[idx2] <- runif(sum(idx2), min = 65, max = 85)
    age[idx3] <- runif(sum(idx3), min = 85, max = 95)
    
  } else if (age_option == "LuoTrunc") {
    # Option LuoTrunc: Truncated Empirical distribution from Luo et al. 2023 (Prostate Cancer)
    # trunc(<65 (46.2%), 65-85 (51.9%), >85 (2.0%))
    u <- runif(n)
    age <- numeric(n)
    
    idx1 <- u <= 0.462
    idx2 <- u > 0.462 & u <= 0.981
    idx3 <- u > 0.981
    
    # clinical bounds: [50-65), [65-85), [85-95)
    age[idx1] <- trunc(runif(sum(idx1), min = 50, max = 65))
    age[idx2] <- trunc(runif(sum(idx2), min = 65, max = 85))
    age[idx3] <- trunc(runif(sum(idx3), min = 85, max = 95))
    
  } else if (age_option == "LuoOlder") {
    # Option LuoOlder: Skewed older
    u <- runif(n)
    age <- numeric(n)
    
    # CHANGE THIS VALUE to skew older. 
    # Example: 0.85 means 85% are 65-85, and 15% are 85-95.
    p2_prime <- 0.00 
    
    idx1 <- u <= p2_prime
    idx2 <- u > p2_prime
    
    age[idx1] <- runif(sum(idx1), min = 65, max = 85)
    age[idx2] <- runif(sum(idx2), min = 85, max = 95)
    
  } else {
    stop("age_option must be 'A', 'C', 'D', 'E', 'F', 'Luo', 'LuoTrunc', or 'LuoOlder'")
  }
  
  # Dynamic Calculation of Theoretical Means
  bounds <- switch(age_option,
                   "A"        = c(80, 90),
                   "C"        = c(15, 40),
                   "D"        = c(50, 75),
                   "E"        = c(50, 60),
                   "F"        = c(80, 90),
                   "Luo"      = NULL, 
                   "LuoTrunc" = NULL,
                   "LuoOlder" = NULL)
  
  if (!is.null(bounds)) {
    if (age_option %in% c("D", "F")) {
      theoretical_mean <- (bounds[1] + (bounds[2] - 1)) / 2
    } else {
      theoretical_mean <- mean(bounds)
    }
  } else {
    # Expected value E[X] = sum( P(Interval) * E[X | Interval] )
    
    if (age_option == "Luo") {
      weights <- c(0.462, 0.519, 0.020)
      means <- c(mean(c(50, 65)), mean(c(65, 85)), mean(c(85, 95)))
      theoretical_mean <- sum(weights * means)
      
    } else if (age_option == "LuoTrunc") {
      weights <- c(0.462, 0.519, 0.020)
      means <- c((50 + 64)/2, (65 + 84)/2, (85 + 94)/2)
      theoretical_mean <- sum(weights * means)
      
    } else if (age_option == "LuoOlder") {
      # MATCH the p2_prime distribution set above
      # e.g., 0.85 and 0.15
      weights <- c(0.00, 1) 
      means <- c(mean(c(65, 85)), mean(c(85, 95)))
      theoretical_mean <- sum(weights * means)
    }
  } 
  ageCentre <- age - theoretical_mean
  
  uSex <- runif(n)
  pFem <- prop_female
  sex  <- rbinom(n, size = 1, prob = prop_female)   # hommes=0, femmes=1
  
  if (year.start_min != year.start_max) {
    year.start <- as.Date(paste0(
      sample(year.start_min:year.start_max, n, replace = TRUE),
      "-01-01"
    )) +
      sample(0:364, n, replace = TRUE)
  } else {
    year.start <- as.Date(paste0(year.start_min, "-01-01")) +
      sample(0:364, n, replace = TRUE)
  }
  
  # T_E generation
  tempuS <- runif(n)
  exp.betaz <- exp(beta_sex * sex + beta_age * ageCentre)
  
  ui <- runif(n)
  tpsSpe <- -log(ui) / (lambda * exp.betaz)
  
  # T_P generation
  tpsGene <- rep(0, n)
  TauxAtt <- rep(NA, n)
  sexNom <- ifelse(sex == 0, "male", "female")

  f1 <- function(i) {
    # Instead of drawing a new uniform variable annually, draw a single lifetime survival probability 
    # for exact continuous inversion.
    H_target <- -log(runif(1))
    H_accum <- 0
    t_years <- 0
    
    i.age <- which(attr(survexp.us, which = "dimnames")[[1]] == trunc(age[i]))
    i.sex <- which(attr(survexp.us, which = "dimnames")[[2]] == sexNom[i])
    i.year <- which(attr(survexp.us, which = "dimnames")[[3]] == format(year.start[i], "%Y"))
    
    if (length(i.age) == 0 || length(i.sex) == 0 || length(i.year) == 0) return(NA)
    
    max.i.age <- length(attributes(survexp.us)$dimnames[[1]])
    max.i.year <- length(attributes(survexp.us)$dimnames[[3]])
    
    repeat {
      h_year <- 365.241 * survexp.us[i.age, i.sex, i.year]
      #if the patient will accumulate more hazard than the random H_target, 
      #then we calculate the exact time of death within that year
      if (H_accum + h_year >= H_target) {
        frac <- (H_target - H_accum) / h_year
        return(t_years + frac)
      }
      
      H_accum <- H_accum + h_year
      t_years <- t_years + 1
      i.age <- min(i.age + 1, max.i.age)
      i.year <- min(i.year + 1, max.i.year)
    }
  }
  tpsGene <- sapply(1:n, f1)
  
  # tpsGene <- tpsGene * 365.241
  # tpsSpe  <- tpsSpe * 365.241
  tpsSurv <- pmin(tpsGene, tpsSpe)
  
  # CENSORING
  borne_a = borne_a
  
  # censoring time
  # tpsCens <- runif(n, min = 0, max = borne_a)
  if (is.finite(borne_a)) {
    tpsCens <- runif(n, min = 0, max = borne_a)
  } else {
    tpsCens <- rep(Inf, n)
  }
  
  temps <- pmin(tpsCens, tpsSurv)
  temps2 <- pmin(tpsCens, tpsSpe)
  
  statut     <- ifelse(temps == tpsCens, 0, 1)
  
  # cause of death
  cause <- ifelse(tpsSurv == tpsSpe & temps != tpsCens, 1, 0)
  
  # hypothetical world
  cause2     <- ifelse(temps2 == tpsCens, 0, 1)
  
  # administrative censoring
  
  statut[temps > max_time]   <-  0
  cause[temps > max_time] <- 0
  # hypothetical world
  cause2[temps2 > max_time]  <-  0
  
  temps[temps > max_time]    <- max_time
  # hypothetical world
  temps2[temps2 > max_time]  <- max_time
  
  # Added event_type 
  event_type <- ifelse(temps == tpsCens | temps == max_time, "censored",
                       ifelse(temps == tpsSpe, "cancer", "other"))
  
  result <- data.frame(
    patient_id = 1:n,
    age = age,
    ageCentre = ageCentre,
    sex_num = sex,
    sex = factor(sexNom, levels = c("male", "female")),
    tpsCens = tpsCens,
    tpsGene = tpsGene,
    tpsSpe = tpsSpe,
    year_diagnosis = year.start,
    observed_time = temps,
    status = statut,
    cause = cause,
    event_type = event_type,
    hypothetical_time = temps2,
    hypothetical_status = cause2,
    admin_cens = max_time
  )
  
  return(result)
}