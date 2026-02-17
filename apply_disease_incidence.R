#' Apply disease incidence probabilities and simulate disease occurrence
#'
#' @param population Data frame with individual characteristics
#' @param incidence_probabilities Data frame with baseline incidence rates
#' @param smoking_relative_risks Data frame with smoking relative risks
#' @param blood_pressure_relative_risks Data frame with BP relative risks  
#' @param bmi_relative_risks Data frame with BMI relative risks
#' @param diseases Character vector of diseases to calculate
#' @param apply_incidence Logical, whether to probabilistically apply disease incidence
#' @param current_year Numeric, current simulation year for tracking incidence timing
#' @param seed Numeric seed for reproducible random sampling
#' @return Data frame with added probability columns, disease status, and incidence year
apply_disease_incidence <- function(
    population,
    incidence_probabilities,
    smoking_relative_risks,
    blood_pressure_relative_risks,
    bmi_relative_risks,
    diseases = c("CHD", "stroke", "COPD", "lung_cancer", "colorectal_cancer"),
    apply_incidence = TRUE,
    current_year,
    seed = NULL
) {
  
  if (!all(diseases %in% c("CHD", "stroke", "COPD", "lung_cancer", "colorectal_cancer"))) {
    stop("Invalid disease names. Must be one of: CHD, stroke, COPD, lung_cancer, colorectal_cancer")
  }
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  updated_population <- population
  n_population <- nrow(updated_population)
  
  # Pre-compute age groups
  updated_population$age_group_incidence <- get_age_group_incidence_vectorized(updated_population$age)
  updated_population$age_group_smoking <- get_age_group_smoking_vectorized(updated_population$age)
  updated_population$age_group_bp <- get_age_group_bp_vectorized(updated_population$age)
  updated_population$age_group_bmi <- get_age_group_bmi_vectorized(updated_population$age)
  
  # Standardize sex labels
  updated_population$sex_standard <- ifelse(updated_population$sex_label == "Men", "Male", "Female")
  
  # Initialize disease columns
  for (disease in diseases) {
    updated_population[[paste0(disease, "_probability")]] <- 0
    updated_population[[paste0(disease, "_baseline_prob")]] <- 0
    updated_population[[paste0(disease, "_smoking_rr")]] <- 1
    updated_population[[paste0(disease, "_bp_rr")]] <- 1
    updated_population[[paste0(disease, "_bmi_rr")]] <- 1
    updated_population[[paste0(disease, "_combined_rr")]] <- 1
    
    if (apply_incidence) {
      disease_col <- ifelse(disease == "lung_cancer", "lung_cancer", 
                            ifelse(disease == "colorectal_cancer", "colorectal_cancer", disease))
      
      if (!disease_col %in% names(updated_population)) {
        updated_population[[disease_col]] <- FALSE
      }
      
      if (paste0(disease_col, "_years") %in% names(updated_population)) {
        updated_population[[paste0(disease_col, "_years")]] <- 
          ifelse(is.na(updated_population[[paste0(disease_col, "_years")]]), 0, updated_population[[paste0(disease_col, "_years")]])
      }
      
      incidence_year_col <- paste0(disease_col, "_incidence_year")
      if (!incidence_year_col %in% names(updated_population)) {
        updated_population[[incidence_year_col]] <- NA
      }
    }
  }
  
  # Process each disease
  for (disease in diseases) {
    
    # Get baseline incidence probabilities
    baseline_probs <- get_baseline_incidence_vectorized(
      incidence_probabilities,
      disease,
      updated_population$age_group_incidence,
      updated_population$sex_standard
    )
    
    # Get smoking relative risks
    smoking_rrs <- get_smoking_relative_risk_vectorized(
      smoking_relative_risks,
      disease,
      updated_population$sex_standard,
      updated_population$smoking_status,
      updated_population$age_group_smoking
    )
    
    # Get blood pressure relative risks
    bp_rrs <- get_bp_relative_risk_vectorized(
      blood_pressure_relative_risks,
      disease,
      updated_population$sex_standard,
      updated_population$sbp,
      updated_population$age_group_bp
    )
    
    # Get BMI relative risks
    bmi_rrs <- get_bmi_relative_risk_vectorized(
      bmi_relative_risks,
      disease,
      updated_population$sex_standard,
      updated_population$bmi_category,
      updated_population$age_group_bmi,
      updated_population$age
    )
    
    # Calculate combined relative risk and final probabilities
    combined_rrs <- smoking_rrs * bp_rrs * bmi_rrs
    final_probs <- baseline_probs * combined_rrs
    
    # Store results
    updated_population[[paste0(disease, "_probability")]] <- final_probs
    updated_population[[paste0(disease, "_baseline_prob")]] <- baseline_probs
    updated_population[[paste0(disease, "_smoking_rr")]] <- smoking_rrs
    updated_population[[paste0(disease, "_bp_rr")]] <- bp_rrs
    updated_population[[paste0(disease, "_bmi_rr")]] <- bmi_rrs
    updated_population[[paste0(disease, "_combined_rr")]] <- combined_rrs
    
    # Apply incidence probabilistically if requested
    if (apply_incidence) {
      disease_col <- ifelse(disease == "lung_cancer", "lung_cancer", 
                            ifelse(disease == "colorectal_cancer", "colorectal_cancer", disease))
      
      current_disease_status <- updated_population[[disease_col]]
      
      # Only apply to individuals without current disease
      eligible_mask <- !current_disease_status & !is.na(current_disease_status)
      n_eligible <- sum(eligible_mask)
      
      if (n_eligible > 0) {
        # Generate random draws for eligible individuals
        random_draws <- runif(n_eligible)
        eligible_probs <- final_probs[eligible_mask]
        
        # Determine new cases
        new_cases <- random_draws < eligible_probs
        
        # Update disease status
        updated_population[eligible_mask, disease_col][new_cases] <- TRUE
        
        # Update years since disease onset
        if (paste0(disease_col, "_years") %in% names(updated_population)) {
          updated_population[eligible_mask, paste0(disease_col, "_years")][new_cases] <- 0
        }
        
        # Update incidence year
        incidence_year_col <- paste0(disease_col, "_incidence_year")
        if (incidence_year_col %in% names(updated_population)) {
          updated_population[eligible_mask, incidence_year_col][new_cases] <- current_year
        }
      }
    }
  }
  
  # Remove temporary columns
  updated_population$age_group_incidence <- NULL
  updated_population$age_group_smoking <- NULL
  updated_population$age_group_bp <- NULL
  updated_population$age_group_bmi <- NULL
  updated_population$sex_standard <- NULL
  
  return(updated_population)
}

#' Age group functions
#' @param ages Numeric vector of ages
#' @return Character vector of age groups

get_age_group_incidence_vectorized <- function(ages) {
  age_groups <- character(length(ages))
  age_groups[ages < 1] <- "0 years"
  age_groups[ages >= 1 & ages <= 4] <- "1 - 4 years"
  age_groups[ages >= 5 & ages <= 9] <- "5 - 9 years"
  age_groups[ages >= 10 & ages <= 14] <- "10 - 14 years"
  age_groups[ages >= 15 & ages <= 19] <- "15 - 19 years"
  age_groups[ages >= 20 & ages <= 24] <- "20 - 24 years"
  age_groups[ages >= 25 & ages <= 29] <- "25 - 29 years"
  age_groups[ages >= 30 & ages <= 34] <- "30 - 34 years"
  age_groups[ages >= 35 & ages <= 39] <- "35 - 39 years"
  age_groups[ages >= 40 & ages <= 44] <- "40 - 44 years"
  age_groups[ages >= 45 & ages <= 49] <- "45 - 49 years"
  age_groups[ages >= 50 & ages <= 54] <- "50 - 54 years"
  age_groups[ages >= 55 & ages <= 59] <- "55 - 59 years"
  age_groups[ages >= 60 & ages <= 64] <- "60 - 64 years"
  age_groups[ages >= 65 & ages <= 69] <- "65 - 69 years"
  age_groups[ages >= 70 & ages <= 74] <- "70 - 74 years"
  age_groups[ages >= 75 & ages <= 79] <- "75 - 79 years"
  age_groups[ages >= 80 & ages <= 84] <- "80 - 84 years"
  age_groups[ages >= 85] <- "85+ years"
  return(age_groups)
}

get_age_group_smoking_vectorized <- function(ages) {
  age_groups <- character(length(ages))
  age_groups[ages < 35] <- "<35 years"
  age_groups[ages >= 35 & ages <= 39] <- "35 - 39 years"
  age_groups[ages >= 40 & ages <= 44] <- "40 - 44 years"
  age_groups[ages >= 45 & ages <= 49] <- "45 - 49 years"
  age_groups[ages >= 50 & ages <= 54] <- "50 - 54 years"
  age_groups[ages >= 55 & ages <= 59] <- "55 - 59 years"
  age_groups[ages >= 60 & ages <= 64] <- "60 - 64 years"
  age_groups[ages >= 65] <- "65+ years"
  return(age_groups)
}

get_age_group_bp_vectorized <- function(ages) {
  age_groups <- character(length(ages))
  age_groups[ages >= 25 & ages <= 34] <- "25 - 34 years"
  age_groups[ages >= 35 & ages <= 44] <- "35 - 44 years"
  age_groups[ages >= 45 & ages <= 54] <- "45 - 54 years"
  age_groups[ages >= 55 & ages <= 64] <- "55 - 64 years"
  age_groups[ages >= 65 & ages <= 74] <- "65 - 74 years"
  age_groups[ages >= 75 & ages <= 84] <- "75 - 84 years"
  age_groups[ages >= 85] <- "85+ years"
  # Default for ages < 25
  age_groups[ages < 25] <- "25 - 34 years"
  return(age_groups)
}

get_age_group_bmi_vectorized <- function(ages) {
  age_groups <- character(length(ages))
  age_groups[ages >= 0 & ages <= 20] <- "0 - 20 years"
  age_groups[ages >= 21 & ages <= 25] <- "21 - 25 years"
  age_groups[ages >= 26 & ages <= 64] <- "26 - 64 years"
  age_groups[ages >= 65 & ages <= 69] <- "65 - 69 years"
  age_groups[ages >= 70 & ages <= 79] <- "70 - 79 years"
  age_groups[ages >= 80] <- "80+ years"
  return(age_groups)
}

#' Baseline incidence lookup
#' @param incidence_probabilities Data frame with baseline rates
#' @param disease Character disease name
#' @param age_groups Character vector of age groups
#' @param sexes Character vector of sex labels
#' @return Numeric vector of baseline probabilities
get_baseline_incidence_vectorized <- function(incidence_probabilities, disease, age_groups, sexes) {
  disease_lookup <- list(
    "CHD" = "CHD",
    "stroke" = "Stroke", 
    "COPD" = "COPD",
    "lung_cancer" = "Lung_Cancer",
    "colorectal_cancer" = "Colorectal_Cancer"
  )
  
  disease_name <- disease_lookup[[disease]]
  
  # Create lookup table from incidence probabilities
  lookup_data <- incidence_probabilities[
    incidence_probabilities$disease_std == disease_name,
    c("age", "sex_label", "baseline_incidence_prob_rr1")
  ]
  
  # Create combined keys for matching
  lookup_keys <- paste(lookup_data$age, lookup_data$sex_label, sep = "|||")
  population_keys <- paste(age_groups, sexes, sep = "|||")
  
  # Match and extract probabilities
  match_indices <- match(population_keys, lookup_keys)
  probabilities <- lookup_data$baseline_incidence_prob_rr1[match_indices]
  
  # Replace NAs with 0 and warn
  na_mask <- is.na(probabilities)
  if (any(na_mask)) {
    unique_missing <- unique(population_keys[na_mask])
    warning(paste("No baseline incidence found for", disease, "in combinations:", 
                  paste(unique_missing, collapse = ", "), "- returning 0"))
    probabilities[na_mask] <- 0
  }
  
  return(probabilities)
}

#' Smoking relative risk lookup
#' @param smoking_rr Data frame with smoking relative risks
#' @param disease Character disease name
#' @param sexes Character vector of sex labels
#' @param smoking_statuses Character vector of smoking statuses
#' @param age_groups Character vector of age groups
#' @return Numeric vector of relative risks
get_smoking_relative_risk_vectorized <- function(smoking_rr, disease, sexes, smoking_statuses, age_groups) {
  
  disease_lookup <- list(
    "CHD" = "CHD",
    "stroke" = "Stroke",
    "COPD" = "COPD", 
    "lung_cancer" = "Lung_Cancer",
    "colorectal_cancer" = "Colorectal_Cancer"
  )
  
  disease_name <- disease_lookup[[disease]]
  
  # Map smoking status
  smoking_lookup <- list(
    "Never smoker" = "Never_Smoked",
    "Current smoker" = "Current_Smoker",
    "Former smoker" = "Former_Smoker"
  )
  
  smoking_mapped <- smoking_lookup[smoking_statuses]
  smoking_mapped[is.na(smoking_mapped)] <- "Never_Smoked"
  smoking_mapped <- unlist(smoking_mapped)
  
  # Filter smoking RR data for this disease
  disease_data <- smoking_rr[smoking_rr$Disease == disease_name, ]
  
  if (nrow(disease_data) == 0) {
    warning(paste("No smoking RR data found for disease:", disease_name))
    return(rep(1, length(sexes)))
  }
  
  lookup_keys_exact <- paste(disease_data$Sex, disease_data$Smoking_Status, disease_data$Age, sep = "|||")
  population_keys_exact <- paste(sexes, smoking_mapped, age_groups, sep = "|||")
  match_indices_exact <- match(population_keys_exact, lookup_keys_exact)
  
  relative_risks <- disease_data$Relative_Risk[match_indices_exact]
  
  # For colorectal cancer, broad age range for missing matches
  if (disease_name == "Colorectal_Cancer") {
    na_mask <- is.na(relative_risks)
    if (any(na_mask)) {
      # Try with broad age range
      population_keys_broad <- paste(sexes[na_mask], smoking_mapped[na_mask], "18 - 110 years", sep = "|||")
      match_indices_broad <- match(population_keys_broad, lookup_keys_exact)
      relative_risks[na_mask] <- disease_data$Relative_Risk[match_indices_broad]
    }
  }
  
  # Handle remaining NAs
  na_mask <- is.na(relative_risks)
  if (any(na_mask)) {
    unique_missing <- unique(population_keys_exact[na_mask])
    warning(paste("No smoking RR found for", disease_name, "in combinations:", 
                  paste(unique_missing, collapse = ", "), "- returning 1"))
    relative_risks[na_mask] <- 1
  }
  
  return(relative_risks)
}

#' Blood pressure relative risk calculation
#' @param bp_rr Data frame with BP relative risks
#' @param disease Character disease name
#' @param sexes Character vector of sex labels
#' @param sbps Numeric vector of systolic BP values
#' @param age_groups Character vector of age groups
#' @return Numeric vector of relative risks
get_bp_relative_risk_vectorized <- function(bp_rr, disease, sexes, sbps, age_groups) {
  
  disease_lookup <- list(
    "CHD" = "CHD",
    "stroke" = "Stroke",
    "COPD" = "COPD",
    "lung_cancer" = "Lung_Cancer", 
    "colorectal_cancer" = "Colorectal_Cancer"
  )
  
  disease_name <- disease_lookup[[disease]]
  
  # Initialize return vector
  relative_risks <- rep(1, length(sexes))
  
  if (!disease_name %in% unique(bp_rr$Disease)) {
    return(relative_risks)
  }
  
  if (disease_name == "Colorectal_Cancer") {
    hypertension_statuses <- ifelse(sbps >= 140, "Hypertension", "No_Hypertension")
    
    # Filter BP data for colorectal cancer
    cc_data <- bp_rr[bp_rr$Disease == disease_name, ]
    
    # Create lookup
    lookup_keys <- paste(cc_data$Sex, cc_data$Hypertension_Status, sep = "|||")
    population_keys <- paste(sexes, hypertension_statuses, sep = "|||")
    match_indices <- match(population_keys, lookup_keys)
    
    relative_risks <- cc_data$Relative_Risk[match_indices]
    
    na_mask <- is.na(relative_risks)
    if (any(na_mask)) {
      warning(paste("No BP RR found for", disease_name, "- returning 1 for missing values"))
      relative_risks[na_mask] <- 1
    }
    
    return(relative_risks)
  }
  
  if (disease_name %in% c("CHD", "Stroke")) {
    # Filter data for per 10mmHg calculations
    disease_data <- bp_rr[
      bp_rr$Disease == disease_name & bp_rr$Hypertension_Status == "per_10mm_Hg", 
    ]
    
    if (nrow(disease_data) == 0) {
      return(relative_risks)
    }
    
    # Create lookup for RR per 10mmHg by sex and age
    lookup_keys <- paste(disease_data$Sex, disease_data$Age, sep = "|||")
    population_keys <- paste(sexes, age_groups, sep = "|||")
    match_indices <- match(population_keys, lookup_keys)
    
    rr_per_10mmhg <- disease_data$Relative_Risk[match_indices]
    
    # Calculate relative risks based on SBP
    reference_sbp <- 115
    mmhg_increases <- pmax(0, (sbps - reference_sbp) / 10)  # Only increases above reference
    
    # Apply RR calculation vectorized
    relative_risks <- ifelse(
      !is.na(rr_per_10mmhg) & sbps > reference_sbp,
      rr_per_10mmhg ^ mmhg_increases,
      1
    )
    
    na_mask <- is.na(rr_per_10mmhg)
    if (any(na_mask)) {
      warning(paste("No BP RR found for", disease_name, "- returning 1 for missing values"))
      relative_risks[na_mask] <- 1
    }
  }
  
  return(relative_risks)
}

#' BMI relative risk lookup
#' @param bmi_rr Data frame with BMI relative risks
#' @param disease Character disease name
#' @param sexes Character vector of sex labels
#' @param bmi_categories Character vector of BMI categories
#' @param age_groups Character vector of age groups
#' @param actual_ages Numeric vector of actual ages (for precise colorectal cancer mapping)
#' @return Numeric vector of relative risks
get_bmi_relative_risk_vectorized <- function(bmi_rr, disease, sexes, bmi_categories, age_groups, actual_ages = NULL) {
  
  disease_lookup <- list(
    "CHD" = "CHD",
    "stroke" = "Stroke",
    "COPD" = "COPD",
    "lung_cancer" = "Lung_Cancer",
    "colorectal_cancer" = "Colorectal_Cancer"
  )
  
  disease_name <- disease_lookup[[disease]]
  
  # Initialize return vector
  relative_risks <- rep(1, length(sexes))
  
  if (!disease_name %in% unique(bmi_rr$Disease)) {
    return(relative_risks)
  }
  
  # Map BMI categories
  bmi_lookup <- list(
    "normal_weight" = "Normal",
    "Normal weight" = "Normal",
    "overweight" = "Overweight",
    "Overweight" = "Overweight",
    "obese" = "Obese",
    "Obese" = "Obese",
    "underweight" = "Normal",
    "Underweight" = "Normal"
  )
  
  bmi_mapped <- bmi_lookup[bmi_categories]
  bmi_mapped[is.na(bmi_mapped)] <- "Normal"
  bmi_mapped <- unlist(bmi_mapped)
  
  # Handle colorectal cancer age group mapping
  if (disease_name == "Colorectal_Cancer" && !is.null(actual_ages)) {
    age_groups_use <- character(length(actual_ages))
    age_groups_use[actual_ages <= 20] <- "0 - 20 years"
    age_groups_use[actual_ages > 20 & actual_ages <= 45] <- "21 - 45 years"
    age_groups_use[actual_ages > 45] <- "45+ years"
  } else if (disease_name == "Colorectal_Cancer") {
    # Fallback mapping when actual_ages not available
    age_groups_use <- character(length(age_groups))
    age_groups_use[age_groups == "0 - 20 years"] <- "0 - 20 years"
    age_groups_use[age_groups == "21 - 25 years"] <- "21 - 45 years"
    age_groups_use[age_groups %in% c("26 - 64 years", "65 - 69 years", "70 - 79 years", "80+ years")] <- "45+ years"
    age_groups_use[age_groups_use == ""] <- "45+ years"  # Default for unmapped
  } else {
    age_groups_use <- age_groups
  }
  
  # Filter BMI data for this disease
  disease_data <- bmi_rr[bmi_rr$Disease == disease_name, ]
  
  if (nrow(disease_data) == 0) {
    return(relative_risks)
  }
  
  # Create lookup
  lookup_keys <- paste(disease_data$Sex, disease_data$BMI_Status, disease_data$Age, sep = "|||")
  population_keys <- paste(sexes, bmi_mapped, age_groups_use, sep = "|||")
  match_indices <- match(population_keys, lookup_keys)
  
  relative_risks <- disease_data$Relative_Risk[match_indices]
  
  # Handle NAs
  na_mask <- is.na(relative_risks)
  if (any(na_mask)) {
    unique_missing <- unique(population_keys[na_mask])
    warning(paste("No BMI RR found for", disease_name, "in combinations:", 
                  paste(unique_missing, collapse = ", "), "- returning 1"))
    relative_risks[na_mask] <- 1
  }
  
  return(relative_risks)
}

# Original individual functions for backward compatibility
get_age_group_incidence <- function(age) {
  return(get_age_group_incidence_vectorized(age))
}

get_age_group_smoking <- function(age) {
  return(get_age_group_smoking_vectorized(age))
}

get_age_group_bp <- function(age) {
  return(get_age_group_bp_vectorized(age))
}

get_age_group_bmi <- function(age) {
  return(get_age_group_bmi_vectorized(age))
}

get_baseline_incidence <- function(incidence_probabilities, disease, age_group, sex) {
  return(get_baseline_incidence_vectorized(incidence_probabilities, disease, age_group, sex))
}

get_smoking_relative_risk <- function(smoking_rr, disease, sex, smoking_status, age_group) {
  return(get_smoking_relative_risk_vectorized(smoking_rr, disease, sex, smoking_status, age_group))
}

get_bp_relative_risk <- function(bp_rr, disease, sex, sbp, age_group) {
  return(get_bp_relative_risk_vectorized(bp_rr, disease, sex, sbp, age_group))
}

get_bmi_relative_risk <- function(bmi_rr, disease, sex, bmi_category, age_group, actual_age = NULL) {
  return(get_bmi_relative_risk_vectorized(bmi_rr, disease, sex, bmi_category, age_group, actual_age))
}

# Example Usage
# population <- apply_disease_incidence(
#   population = population,
#   current_year = current_year,
#   incidence_probabilities,
#   smoking_relative_risks,
#   blood_pressure_relative_risks,
#   bmi_relative_risks,
#   diseases = c("CHD", "stroke", "COPD", "lung_cancer", "colorectal_cancer"),
#   apply_incidence = TRUE,
#   seed = 9001
# )