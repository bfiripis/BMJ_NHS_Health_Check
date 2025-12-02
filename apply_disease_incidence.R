#' Apply disease incidence probabilities and simulate disease occurrence
#' Integrated with ex-smoker relative risk calculations
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
  
  for (i in 1:nrow(updated_population)) {
    individual <- updated_population[i, ]
    age <- individual$age
    sex <- ifelse(individual$sex_label == "Men", "Male", "Female")
    smoking_status <- individual$smoking_status
    sbp <- individual$sbp
    bmi_category <- individual$bmi_category
    
    age_group_incidence <- get_age_group_incidence(age)
    age_group_smoking <- get_age_group_smoking(age)
    age_group_bp <- get_age_group_bp(age)
    age_group_bmi <- get_age_group_bmi(age)
    
    for (disease in diseases) {
      baseline_prob <- get_baseline_incidence(
        incidence_probabilities, 
        disease, 
        age_group_incidence, 
        sex
      )
      
      smoking_rr <- get_smoking_relative_risk(
        smoking_relative_risks,
        disease,
        sex,
        smoking_status,
        age_group_smoking
      )
      
      bp_rr <- get_bp_relative_risk(
        blood_pressure_relative_risks,
        disease,
        sex,
        sbp,
        age_group_bp
      )
      
      bmi_rr <- get_bmi_relative_risk(
        bmi_relative_risks,
        disease,
        sex,
        bmi_category,
        age_group_bmi,
        age  # Pass actual age for colorectal cancer 45+ split
      )
      
      combined_rr <- smoking_rr * bp_rr * bmi_rr
      final_prob <- baseline_prob * combined_rr
      
      updated_population[i, paste0(disease, "_probability")] <- final_prob
      updated_population[i, paste0(disease, "_baseline_prob")] <- baseline_prob
      updated_population[i, paste0(disease, "_smoking_rr")] <- smoking_rr
      updated_population[i, paste0(disease, "_bp_rr")] <- bp_rr
      updated_population[i, paste0(disease, "_bmi_rr")] <- bmi_rr
      updated_population[i, paste0(disease, "_combined_rr")] <- combined_rr
      
      if (apply_incidence) {
        disease_col <- ifelse(disease == "lung_cancer", "lung_cancer", 
                              ifelse(disease == "colorectal_cancer", "colorectal_cancer", disease))
        
        current_disease_status <- updated_population[i, disease_col]
        
        if (!current_disease_status && !is.na(current_disease_status)) {
          random_draw <- runif(1)
          
          if (random_draw < final_prob) {
            updated_population[i, disease_col] <- TRUE
            
            if (paste0(disease_col, "_years") %in% names(updated_population)) {
              updated_population[i, paste0(disease_col, "_years")] <- 0
            }
            
            incidence_year_col <- paste0(disease_col, "_incidence_year")
            if (incidence_year_col %in% names(updated_population)) {
              updated_population[i, incidence_year_col] <- current_year
            }
          }
        }
      }
    }
  }
  
  return(updated_population)
}

#' Get age group for incidence data
#' @param age Numeric age
#' @return Character age group
get_age_group_incidence <- function(age) {
  if (age < 1) return("0 years")
  if (age >= 1 & age <= 4) return("1 - 4 years")
  if (age >= 5 & age <= 9) return("5 - 9 years")
  if (age >= 10 & age <= 14) return("10 - 14 years")
  if (age >= 15 & age <= 19) return("15 - 19 years")
  if (age >= 20 & age <= 24) return("20 - 24 years")
  if (age >= 25 & age <= 29) return("25 - 29 years")
  if (age >= 30 & age <= 34) return("30 - 34 years")
  if (age >= 35 & age <= 39) return("35 - 39 years")
  if (age >= 40 & age <= 44) return("40 - 44 years")
  if (age >= 45 & age <= 49) return("45 - 49 years")
  if (age >= 50 & age <= 54) return("50 - 54 years")
  if (age >= 55 & age <= 59) return("55 - 59 years")
  if (age >= 60 & age <= 64) return("60 - 64 years")
  if (age >= 65 & age <= 69) return("65 - 69 years")
  if (age >= 70 & age <= 74) return("70 - 74 years")
  if (age >= 75 & age <= 79) return("75 - 79 years")
  if (age >= 80 & age <= 84) return("80 - 84 years")
  if (age >= 85) return("85+ years")
}

#' Get age group for smoking relative risks
#' @param age Numeric age 
#' @return Character age group
get_age_group_smoking <- function(age) {
  if (age < 35) return("<35 years")
  if (age >= 35 & age <= 39) return("35 - 39 years")
  if (age >= 40 & age <= 44) return("40 - 44 years")
  if (age >= 45 & age <= 49) return("45 - 49 years")
  if (age >= 50 & age <= 54) return("50 - 54 years")
  if (age >= 55 & age <= 59) return("55 - 59 years")
  if (age >= 60 & age <= 64) return("60 - 64 years")
  if (age >= 65) return("65+ years")
}

#' Get age group for blood pressure relative risks
#' @param age Numeric age
#' @return Character age group
get_age_group_bp <- function(age) {
  if (age >= 25 & age <= 34) return("25 - 34 years")
  if (age >= 35 & age <= 44) return("35 - 44 years")
  if (age >= 45 & age <= 54) return("45 - 54 years")
  if (age >= 55 & age <= 64) return("55 - 64 years")
  if (age >= 65 & age <= 74) return("65 - 74 years")
  if (age >= 75 & age <= 84) return("75 - 84 years")
  if (age >= 85) return("85+ years")
  return("25 - 34 years")
}

#' Get age group for BMI relative risks  
#' @param age Numeric age
#' @return Character age group
get_age_group_bmi <- function(age) {
  if (age >= 0 & age <= 20) return("0 - 20 years")
  if (age >= 21 & age <= 25) return("21 - 25 years")
  if (age >= 26 & age <= 64) return("26 - 64 years")
  if (age >= 65 & age <= 69) return("65 - 69 years")
  if (age >= 70 & age <= 79) return("70 - 79 years")
  if (age >= 80) return("80+ years")
  return("26 - 64 years")
}

#' Get baseline incidence probability
#' @param incidence_probabilities Data frame with baseline rates
#' @param disease Character disease name
#' @param age_group Character age group  
#' @param sex Character sex
#' @return Numeric baseline probability
get_baseline_incidence <- function(incidence_probabilities, disease, age_group, sex) {
  disease_lookup <- list(
    "CHD" = "CHD",
    "stroke" = "Stroke", 
    "COPD" = "COPD",
    "lung_cancer" = "Lung_Cancer",
    "colorectal_cancer" = "Colorectal_Cancer"
  )
  
  disease_name <- disease_lookup[[disease]]
  sex_label <- ifelse(sex == "Male", "Male", "Female")
  
  match_row <- incidence_probabilities[
    incidence_probabilities$disease_std == disease_name &
      incidence_probabilities$age == age_group &
      incidence_probabilities$sex_label == sex_label,
  ]
  
  if (nrow(match_row) == 0) {
    warning(paste("No baseline incidence found for", disease, age_group, sex_label, "- returning 0"))
    return(0)
  }
  
  return(match_row$baseline_incidence_prob_rr1[1])
}

#' Get smoking relative risk using static former smoker relative risks
#' @param smoking_rr Data frame with smoking relative risks
#' @param disease Character disease name
#' @param sex Character sex
#' @param smoking_status Character smoking status
#' @param age_group Character age group
#' @return Numeric relative risk
get_smoking_relative_risk <- function(smoking_rr, disease, sex, smoking_status, age_group) {
  
  # Map disease names to match the data structure
  disease_lookup <- list(
    "CHD" = "CHD",
    "stroke" = "Stroke",
    "COPD" = "COPD", 
    "lung_cancer" = "Lung_Cancer",
    "colorectal_cancer" = "Colorectal_Cancer"
  )
  
  disease_name <- disease_lookup[[disease]]
  
  # Map smoking status to match data structure
  smoking_lookup <- list(
    "Never smoker" = "Never_Smoked",
    "Current smoker" = "Current_Smoker",
    "Former smoker" = "Former_Smoker"  # Use the actual Former_Smoker relative risks from data
  )
  
  smoking_name <- smoking_lookup[[smoking_status]]
  if (is.null(smoking_name)) {
    warning(paste("Unknown smoking status:", smoking_status, "- using Never_Smoked"))
    smoking_name <- "Never_Smoked"
  }
  
  # First try exact age group match
  match_row <- smoking_rr[
    smoking_rr$Disease == disease_name &
      smoking_rr$Sex == sex &
      smoking_rr$Smoking_Status == smoking_name &
      smoking_rr$Age == age_group,
  ]
  
  # If no exact match and it's colorectal cancer, try the broad age range
  if (nrow(match_row) == 0 && disease_name == "Colorectal_Cancer") {
    match_row <- smoking_rr[
      smoking_rr$Disease == disease_name &
        smoking_rr$Sex == sex &
        smoking_rr$Smoking_Status == smoking_name &
        smoking_rr$Age == "18 - 110 years",
    ]
  }
  
  if (nrow(match_row) == 0) {
    warning(paste("No smoking RR found for", disease, sex, smoking_name, age_group, "- returning 1"))
    return(1)
  }
  
  return(match_row$Relative_Risk[1])
}

#' Get blood pressure relative risk
#' @param bp_rr Data frame with BP relative risks
#' @param disease Character disease name
#' @param sex Character sex
#' @param sbp Numeric systolic BP
#' @param age_group Character age group
#' @return Numeric relative risk
get_bp_relative_risk <- function(bp_rr, disease, sex, sbp, age_group) {
  
  # Convert disease name to match BP data format
  disease_lookup <- list(
    "CHD" = "CHD",
    "stroke" = "Stroke",
    "COPD" = "COPD",
    "lung_cancer" = "Lung_Cancer", 
    "colorectal_cancer" = "Colorectal_Cancer"
  )
  
  disease_name <- disease_lookup[[disease]]
  
  if (!disease_name %in% unique(bp_rr$Disease)) {
    return(1)
  }
  
  if (disease_name == "Colorectal_Cancer") {
    hypertension_status <- ifelse(sbp >= 140, "Hypertension", "No_Hypertension")
    
    match_row <- bp_rr[
      bp_rr$Disease == disease_name &
        bp_rr$Sex == sex &
        bp_rr$Hypertension_Status == hypertension_status,
    ]
    
    if (nrow(match_row) == 0) {
      warning(paste("No BP RR found for", disease_name, sex, hypertension_status, "- returning 1"))
      return(1)
    }
    
    return(match_row$Relative_Risk[1])
  }
  
  if (disease_name %in% c("CHD", "Stroke")) {
    
    match_row <- bp_rr[
      bp_rr$Disease == disease_name &
        bp_rr$Sex == sex &
        bp_rr$Hypertension_Status == "per_10mm_Hg" &
        bp_rr$Age == age_group,
    ]
    
    if (nrow(match_row) == 0) {
      warning(paste("No BP RR found for", disease_name, sex, age_group, "- returning 1"))
      return(1)
    }
    
    reference_sbp <- 120
    if (sbp <= reference_sbp) return(1)
    
    mmhg_increase <- (sbp - reference_sbp) / 10
    rr_per_10mmhg <- match_row$Relative_Risk[1]
    
    return(rr_per_10mmhg ^ mmhg_increase)
  }
  
  return(1)
}

#' Get BMI relative risk
#' @param bmi_rr Data frame with BMI relative risks
#' @param disease Character disease name
#' @param sex Character sex
#' @param bmi_category Character BMI category
#' @param age_group Character age group
#' @param actual_age Numeric actual age (for precise colorectal cancer mapping)
#' @return Numeric relative risk
get_bmi_relative_risk <- function(bmi_rr, disease, sex, bmi_category, age_group, actual_age = NULL) {
  
  disease_lookup <- list(
    "CHD" = "CHD",
    "stroke" = "Stroke",
    "COPD" = "COPD",
    "lung_cancer" = "Lung_Cancer",
    "colorectal_cancer" = "Colorectal_Cancer"
  )
  
  disease_name <- disease_lookup[[disease]]
  
  if (!disease_name %in% unique(bmi_rr$Disease)) {
    return(1)
  }
  
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
  
  bmi_name <- bmi_lookup[[bmi_category]]
  if (is.null(bmi_name)) {
    warning(paste("Unknown BMI category:", bmi_category, "- using Normal"))
    bmi_name <- "Normal"
  }
  
  if (disease_name == "Colorectal_Cancer") {
    # Map age groups from get_age_group_bmi() to colorectal cancer BMI age groups
    # Colorectal cancer uses: "0 - 20 years", "21 - 45 years", "45+ years"
    if (!is.null(actual_age)) {
      # Use actual age for precise mapping
      if (actual_age <= 20) {
        age_group_use <- "0 - 20 years"
      } else if (actual_age <= 45) {
        age_group_use <- "21 - 45 years"
      } else {
        age_group_use <- "45+ years"
      }
    } else {
      # Fallback to age_group mapping when actual_age not available
      if (age_group == "0 - 20 years") {
        age_group_use <- "0 - 20 years"
      } else if (age_group == "21 - 25 years") {
        age_group_use <- "21 - 45 years"
      } else if (age_group == "26 - 64 years") {
        # Conservative approach - use "45+ years" since most in this range are 45+
        age_group_use <- "45+ years"
      } else {
        age_group_use <- "45+ years"
      }
    }
  } else {
    # For other diseases, use the age group as-is
    age_group_use <- age_group
  }
  
  match_row <- bmi_rr[
    bmi_rr$Disease == disease_name &
      bmi_rr$Sex == sex &
      bmi_rr$BMI_Status == bmi_name &
      bmi_rr$Age == age_group_use,
  ]
  
  if (nrow(match_row) == 0) {
    warning(paste("No BMI RR found for", disease_name, sex, bmi_name, age_group_use, "- returning 1"))
    return(1)
  }
  
  return(match_row$Relative_Risk[1])
}

# Example Usage
# population <- apply_disease_incidence(
#   population = population,
#   current_year = current_year
#   incidence_probabilities,
#   smoking_relative_risks,
#   blood_pressure_relative_risks,
#   bmi_relative_risks,
#   diseases = c("CHD", "stroke", "COPD", "lung_cancer", "colorectal_cancer"),
#   apply_incidence = TRUE,
#   seed = 9001
# )