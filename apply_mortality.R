#' Apply mortality probabilities and simulate death occurrence
#' Optimized version with vectorized operations
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
  n_population <- nrow(updated_population)
  
  # Pre-compute age groups and sex labels vectorized
  updated_population$age_group_mortality <- get_age_group_mortality_vectorized(updated_population$age)
  updated_population$sex_standard <- ifelse(updated_population$sex_label == "Men", "Male", "Female")
  
  # Initialize mortality probability columns for each disease
  for (disease in diseases) {
    updated_population[[paste0(disease, "_mortality_prob")]] <- 0
  }
  
  # Initialize other-cause and total mortality probability columns
  updated_population[["other_cause_mortality_prob"]] <- 0
  updated_population[["total_mortality_prob"]] <- 0
  
  # Initialize death-related columns if they don't exist
  if (!"alive" %in% names(updated_population)) {
    updated_population$alive <- TRUE
  }
  if (!"death_year" %in% names(updated_population)) {
    updated_population$death_year <- NA
  }
  if (!"cause_of_death" %in% names(updated_population)) {
    updated_population$cause_of_death <- NA
  }
  
  # Identify alive individuals (skip already dead individuals)
  alive_mask <- updated_population$alive == TRUE & !is.na(updated_population$alive)
  alive_indices <- which(alive_mask)
  n_alive <- length(alive_indices)
  
  if (n_alive == 0) {
    # Remove temporary columns and return if no one is alive
    updated_population$age_group_mortality <- NULL
    updated_population$sex_standard <- NULL
    return(updated_population)
  }
  
  # Get other-cause mortality probabilities vectorized for all alive individuals
  other_cause_probs <- get_other_cause_mortality_vectorized(
    mortality_probabilities,
    updated_population$age_group_mortality[alive_mask],
    updated_population$sex_standard[alive_mask]
  )
  
  updated_population$other_cause_mortality_prob[alive_mask] <- other_cause_probs
  
  # Initialize survival probability matrix for competing risks calculation
  survival_probs_matrix <- matrix(1, nrow = n_alive, ncol = length(diseases) + 1)
  survival_probs_matrix[, 1] <- 1 - other_cause_probs  # Other-cause survival
  
  # Process each disease vectorized
  for (j in seq_along(diseases)) {
    disease <- diseases[j]
    disease_col <- get_disease_column_name(disease)
    
    # Check which alive individuals have this disease
    if (disease_col %in% names(updated_population)) {
      has_disease_mask <- updated_population[[disease_col]][alive_mask] == TRUE & 
        !is.na(updated_population[[disease_col]][alive_mask])
      
      if (any(has_disease_mask)) {
        # Get disease mortality probabilities for those with the disease
        disease_mortality_probs <- rep(0, n_alive)
        
        disease_mortality_probs[has_disease_mask] <- get_disease_mortality_vectorized(
          mortality_probabilities,
          disease,
          updated_population$age_group_mortality[alive_mask][has_disease_mask],
          updated_population$sex_standard[alive_mask][has_disease_mask]
        )
        
        # Update disease-specific mortality probability column
        updated_population[alive_indices, paste0(disease, "_mortality_prob")] <- disease_mortality_probs
        
        # Update survival probability matrix
        survival_probs_matrix[, j + 1] <- 1 - disease_mortality_probs
      }
    }
  }
  
  # Calculate total mortality probability using competing risks
  total_survival_probs <- apply(survival_probs_matrix, 1, prod)
  total_mortality_probs <- 1 - total_survival_probs
  
  updated_population$total_mortality_prob[alive_mask] <- total_mortality_probs
  
  # Apply mortality if requested
  if (apply_mortality && any(total_mortality_probs > 0)) {
    # Generate random draws for all alive individuals
    random_draws <- runif(n_alive)
    
    # Determine who dies
    death_mask <- random_draws < total_mortality_probs
    death_indices <- alive_indices[death_mask]
    n_deaths <- length(death_indices)
    
    if (n_deaths > 0) {
      # Update alive status and death year
      updated_population$alive[death_indices] <- FALSE
      updated_population$death_year[death_indices] <- current_year
      
      # Determine cause of death vectorized
      causes_of_death <- determine_cause_of_death_vectorized(
        updated_population[death_indices, ],
        diseases
      )
      
      updated_population$cause_of_death[death_indices] <- causes_of_death
    }
  }
  
  # Remove temporary columns
  updated_population$age_group_mortality <- NULL
  updated_population$sex_standard <- NULL
  
  return(updated_population)
}

#' Vectorized age group function for mortality
#' @param ages Numeric vector of ages
#' @return Character vector of age groups
get_age_group_mortality_vectorized <- function(ages) {
  # Handle potential NA values
  na_mask <- is.na(ages)
  age_groups <- character(length(ages))
  
  if (any(na_mask)) {
    warning("NA ages provided to get_age_group_mortality_vectorized - using default '25 - 29 years'")
    age_groups[na_mask] <- "25 - 29 years"
  }
  
  valid_ages <- ages[!na_mask]
  valid_groups <- character(length(valid_ages))
  
  valid_groups[valid_ages < 1] <- "0 years"
  valid_groups[valid_ages >= 1 & valid_ages <= 4] <- "1 - 4 years"
  valid_groups[valid_ages >= 5 & valid_ages <= 9] <- "5 - 9 years"
  valid_groups[valid_ages >= 10 & valid_ages <= 14] <- "10 - 14 years"
  valid_groups[valid_ages >= 15 & valid_ages <= 19] <- "15 - 19 years"
  valid_groups[valid_ages >= 20 & valid_ages <= 24] <- "20 - 24 years"
  valid_groups[valid_ages >= 25 & valid_ages <= 29] <- "25 - 29 years"
  valid_groups[valid_ages >= 30 & valid_ages <= 34] <- "30 - 34 years"
  valid_groups[valid_ages >= 35 & valid_ages <= 39] <- "35 - 39 years"
  valid_groups[valid_ages >= 40 & valid_ages <= 44] <- "40 - 44 years"
  valid_groups[valid_ages >= 45 & valid_ages <= 49] <- "45 - 49 years"
  valid_groups[valid_ages >= 50 & valid_ages <= 54] <- "50 - 54 years"
  valid_groups[valid_ages >= 55 & valid_ages <= 59] <- "55 - 59 years"
  valid_groups[valid_ages >= 60 & valid_ages <= 64] <- "60 - 64 years"
  valid_groups[valid_ages >= 65 & valid_ages <= 69] <- "65 - 69 years"
  valid_groups[valid_ages >= 70 & valid_ages <= 74] <- "70 - 74 years"
  valid_groups[valid_ages >= 75 & valid_ages <= 79] <- "75 - 79 years"
  valid_groups[valid_ages >= 80 & valid_ages <= 84] <- "80 - 84 years"
  valid_groups[valid_ages >= 85 & valid_ages <= 89] <- "85 - 89 years"
  valid_groups[valid_ages >= 90 & valid_ages <= 94] <- "90 - 94 years"
  valid_groups[valid_ages >= 95] <- "95+ years"
  
  age_groups[!na_mask] <- valid_groups
  
  return(age_groups)
}

#' Vectorized other-cause mortality lookup
#' @param mortality_probabilities Data frame with mortality rates
#' @param age_groups Character vector of age groups
#' @param sexes Character vector of sex labels
#' @return Numeric vector of other-cause mortality probabilities
get_other_cause_mortality_vectorized <- function(mortality_probabilities, age_groups, sexes) {
  
  # Create lookup table for other-cause mortality
  # Get unique combinations to avoid duplication
  lookup_data <- unique(mortality_probabilities[, c("age", "sex_label", "other_cause_annual_prob")])
  
  # Create combined keys for matching
  lookup_keys <- paste(lookup_data$age, lookup_data$sex_label, sep = "|||")
  population_keys <- paste(age_groups, sexes, sep = "|||")
  
  # Match and extract probabilities
  match_indices <- match(population_keys, lookup_keys)
  probabilities <- lookup_data$other_cause_annual_prob[match_indices]
  
  # Handle NAs
  na_mask <- is.na(probabilities)
  if (any(na_mask)) {
    unique_missing <- unique(population_keys[na_mask])
    warning(paste("No other-cause mortality found for combinations:", 
                  paste(unique_missing, collapse = ", "), "- returning 0"))
    probabilities[na_mask] <- 0
  }
  
  return(probabilities)
}

#' Vectorized disease-specific mortality lookup
#' @param mortality_probabilities Data frame with mortality rates
#' @param disease Character disease name
#' @param age_groups Character vector of age groups
#' @param sexes Character vector of sex labels
#' @return Numeric vector of disease-specific mortality probabilities
get_disease_mortality_vectorized <- function(mortality_probabilities, disease, age_groups, sexes) {
  
  # Filter mortality data for this disease
  disease_data <- mortality_probabilities[mortality_probabilities$disease == disease, ]
  
  if (nrow(disease_data) == 0) {
    warning(paste("No mortality data found for disease:", disease))
    return(rep(0, length(age_groups)))
  }
  
  # Create lookup
  lookup_keys <- paste(disease_data$age, disease_data$sex_label, sep = "|||")
  population_keys <- paste(age_groups, sexes, sep = "|||")
  match_indices <- match(population_keys, lookup_keys)
  
  probabilities <- disease_data$annual_disease_mortality_prob[match_indices]
  
  # Handle NAs
  na_mask <- is.na(probabilities)
  if (any(na_mask)) {
    unique_missing <- unique(population_keys[na_mask])
    warning(paste("No disease mortality found for", disease, "in combinations:", 
                  paste(unique_missing, collapse = ", "), "- returning 0"))
    probabilities[na_mask] <- 0
  }
  
  return(probabilities)
}

#' Vectorized cause of death determination
#' @param death_population Data frame of individuals who died
#' @param diseases Character vector of diseases
#' @return Character vector of causes of death
determine_cause_of_death_vectorized <- function(death_population, diseases) {
  n_deaths <- nrow(death_population)
  causes_of_death <- character(n_deaths)
  
  for (i in 1:n_deaths) {
    individual <- death_population[i, ]
    
    # Collect mortality probabilities
    other_cause_prob <- individual$other_cause_mortality_prob
    disease_probs <- numeric(length(diseases))
    names(disease_probs) <- diseases
    
    for (j in seq_along(diseases)) {
      disease <- diseases[j]
      prob_col <- paste0(disease, "_mortality_prob")
      if (prob_col %in% names(individual)) {
        disease_probs[j] <- individual[[prob_col]]
      }
    }
    
    # Combine all cause probabilities
    cause_probs <- c(other_cause_prob, disease_probs)
    cause_names <- c("other_cause", diseases)
    
    # Only include causes with positive probabilities
    positive_causes <- cause_probs > 0 & !is.na(cause_probs)
    
    if (any(positive_causes)) {
      cause_probs_positive <- cause_probs[positive_causes]
      cause_names_positive <- cause_names[positive_causes]
      
      # Normalize probabilities for cause determination
      normalized_probs <- cause_probs_positive / sum(cause_probs_positive)
      cumulative_probs <- cumsum(normalized_probs)
      
      cause_draw <- runif(1)
      cause_index <- which(cause_draw <= cumulative_probs)[1]
      
      causes_of_death[i] <- cause_names_positive[cause_index]
    } else {
      causes_of_death[i] <- "other_cause"
    }
  }
  
  return(causes_of_death)
}

#' Get disease column name for mortality lookup
#' @param disease Character disease name
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

# Keep original individual functions for compatibility
get_age_group_mortality <- function(age) {
  return(get_age_group_mortality_vectorized(age))
}

get_disease_mortality <- function(mortality_probabilities, disease, age_group, sex) {
  return(get_disease_mortality_vectorized(mortality_probabilities, disease, age_group, sex))
}

get_other_cause_mortality <- function(mortality_probabilities, age_group, sex) {
  return(get_other_cause_mortality_vectorized(mortality_probabilities, age_group, sex))
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