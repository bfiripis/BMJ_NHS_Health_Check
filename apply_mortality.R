#' Apply mortality probabilities and simulate death occurrence
#'
#' @param population Data frame with individual characteristics and disease status
#' @param mortality_probabilities Data frame with baseline mortality rates
#' @param diseases Character vector of diseases to consider for disease-specific mortality
#' @param apply_mortality Logical, whether to probabilistically apply mortality
#' @param current_year Numeric, current simulation year for tracking death timing
#' @param seed Numeric seed for reproducible random sampling
#' @return Data frame with added mortality probability columns, alive status updated, death year, and cause of death
apply_mortality <- function(
    population,
    mortality_probabilities,
    diseases = c("CHD", "stroke", "COPD", "lung_cancer", "colorectal_cancer"),
    apply_mortality = TRUE,
    current_year,
    seed = NULL
) {
  
  # Validate disease names against actual data
  available_diseases <- unique(mortality_probabilities$disease)
  invalid_diseases <- setdiff(diseases, available_diseases)
  
  if (length(invalid_diseases) > 0) {
    stop(paste("Invalid disease names:", paste(invalid_diseases, collapse = ", "), 
               "\nAvailable diseases:", paste(available_diseases, collapse = ", ")))
  }
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  updated_population <- population
  
  # Initialize mortality probability columns for each disease
  for (disease in diseases) {
    updated_population[[paste0(disease, "_mortality_prob")]] <- 0
  }
  
  # Initialize other-cause mortality probability
  updated_population[["other_cause_mortality_prob"]] <- 0
  updated_population[["total_mortality_prob"]] <- 0
  
  # Process each individual
  for (i in 1:nrow(updated_population)) {
    individual <- updated_population[i, ]
    
    # Skip if not alive
    if ("alive" %in% names(individual) && individual$alive == FALSE && !is.na(individual$alive)) {
      next
    }
    
    age <- individual$age
    sex <- ifelse(individual$sex_label == "Men", "Male", "Female")
    age_group <- get_age_group_mortality(age)
    
    # Initialize probability tracking
    disease_mortality_probs <- numeric(length(diseases))
    names(disease_mortality_probs) <- diseases
    other_cause_prob <- 0
    
    # Get other-cause mortality probability first
    other_cause_prob <- get_other_cause_mortality(
      mortality_probabilities,
      age_group,
      sex
    )
    
    updated_population[i, "other_cause_mortality_prob"] <- other_cause_prob
    
    # Calculate disease-specific mortality probabilities for diseases the individual has
    for (j in seq_along(diseases)) {
      disease <- diseases[j]
      disease_col <- get_disease_column_name(disease)
      
      # Check if individual has this disease
      if (disease_col %in% names(individual) && 
          individual[[disease_col]] == TRUE && 
          !is.na(individual[[disease_col]])) {
        
        disease_mortality_prob <- get_disease_mortality(
          mortality_probabilities,
          disease,
          age_group,
          sex
        )
        
        disease_mortality_probs[j] <- disease_mortality_prob
        updated_population[i, paste0(disease, "_mortality_prob")] <- disease_mortality_prob
      }
    }
    
    # Calculate total mortality probability
    # Use competing risks approach: 1 - product of survival probabilities
    survival_probs <- c(1 - other_cause_prob, 1 - disease_mortality_probs)
    total_survival_prob <- prod(survival_probs)
    total_mortality_prob <- 1 - total_survival_prob
    
    updated_population[i, "total_mortality_prob"] <- total_mortality_prob
    
    # Apply mortality if requested
    if (apply_mortality && total_mortality_prob > 0) {
      random_draw <- runif(1)
      
      if (random_draw < total_mortality_prob) {
        # Death occurs - determine cause
        updated_population[i, "alive"] <- FALSE
        updated_population[i, "death_year"] <- current_year
        
        # Determine cause of death using proportional allocation
        cause_probs <- c(other_cause_prob, disease_mortality_probs)
        cause_names <- c("other_cause", diseases)
        
        # Only include causes with positive probabilities
        positive_causes <- cause_probs > 0
        cause_probs_positive <- cause_probs[positive_causes]
        cause_names_positive <- cause_names[positive_causes]
        
        if (length(cause_probs_positive) > 0) {
          # Normalize probabilities for cause determination
          normalized_probs <- cause_probs_positive / sum(cause_probs_positive)
          cumulative_probs <- cumsum(normalized_probs)
          
          cause_draw <- runif(1)
          cause_index <- which(cause_draw <= cumulative_probs)[1]
          
          updated_population[i, "cause_of_death"] <- cause_names_positive[cause_index]
        } else {
          updated_population[i, "cause_of_death"] <- "other_cause"
        }
      }
    }
  }
  
  return(updated_population)
}

#' Get age group for mortality data - CORRECTED TO MATCH MORTALITY DATA
#' @param age Numeric age
#' @return Character age group
get_age_group_mortality <- function(age) {
  # Handle potential NA values
  if (is.na(age)) {
    warning("NA age provided to get_age_group_mortality - returning default '25 - 29 years'")
    return("25 - 29 years")
  }
  
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
  if (age >= 85 & age <= 89) return("85 - 89 years")
  if (age >= 90 & age <= 94) return("90 - 94 years")
  if (age >= 95) return("95+ years")
}

#' Get disease column name for mortality lookup - FIXED TO MATCH ACTUAL DATA
#' @param disease Character disease name (as it appears in mortality data)
#' @return Character column name in population data
get_disease_column_name <- function(disease) {
  # Map from mortality data disease names to population data column names
  disease_col_lookup <- list(
    "CHD" = "chd",
    "stroke" = "stroke", 
    "COPD" = "copd",
    "lung_cancer" = "lung_cancer",
    "colorectal_cancer" = "colorectal_cancer"
  )
  
  return(disease_col_lookup[[disease]])
}

#' Get disease-specific mortality probability - FIXED
#' @param mortality_probabilities Data frame with mortality rates
#' @param disease Character disease name (exactly as in data)
#' @param age_group Character age group
#' @param sex Character sex
#' @return Numeric disease mortality probability
get_disease_mortality <- function(mortality_probabilities, disease, age_group, sex) {
  # Disease names should match exactly what's in the data
  # Based on your data: CHD, colorectal_cancer, COPD, lung_cancer, stroke
  
  # Find matching row
  match_row <- mortality_probabilities[
    mortality_probabilities$disease == disease &
      mortality_probabilities$age == age_group &
      mortality_probabilities$sex_label == sex,
  ]
  
  if (nrow(match_row) == 0) {
    warning(paste("No disease mortality found for", disease, age_group, sex, "- returning 0"))
    return(0)
  }
  
  # Return disease-specific mortality probability (handling NA values)
  disease_mortality <- match_row$annual_disease_mortality_prob[1]
  if (is.na(disease_mortality)) {
    return(0)
  }
  
  return(disease_mortality)
}

#' Get other-cause mortality probability - FIXED VERSION
#' @param mortality_probabilities Data frame with mortality rates
#' @param age_group Character age group
#' @param sex Character sex
#' @return Numeric other-cause mortality probability
get_other_cause_mortality <- function(mortality_probabilities, age_group, sex) {
  # Find matching rows
  match_rows <- mortality_probabilities[
    mortality_probabilities$age == age_group &
      mortality_probabilities$sex_label == sex,
  ]
  
  if (nrow(match_rows) == 0) {
    warning(paste("No other-cause mortality found for", age_group, sex, "- returning 0"))
    return(0)
  }
  
  # Get the first row (they should all have the same other-cause mortality)
  match_row <- match_rows[1, ]
  
  other_cause_mortality <- match_row$other_cause_annual_prob[1]
  if (is.na(other_cause_mortality)) {
    return(0)
  }
  
  return(other_cause_mortality)
}

# Example Usage
# population <- apply_mortality(
#   population = population_with_diseases,
#   mortality_probabilities = mortality_probabilities,
#   diseases = c("CHD", "stroke", "COPD", "lung_cancer", "colorectal_cancer"),
#   apply_mortality = TRUE,
#   current_year = current_year,
#   seed = 9001
# )