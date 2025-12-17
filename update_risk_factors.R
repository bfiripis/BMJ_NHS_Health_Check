#' Update Risk Factors for All Individuals Based on Time-Dependent Distributions 
#' 
#' Updates BMI and blood pressure for all individuals based on longitudinal 
#' HSE distribution data for the current year. Age groups are recalculated 
#' - smoking status handled separately using transition probability matrices.
#'
#' @param population Data frame containing individual-level population data with 
#'   columns for age, sex_label, current_age_group, and percentile ranks
#' @param longitudinal_hse_distributions List containing longitudinal HSE distribution data with 
#'   projections for BMI and SBP by age group, sex, and year
#' @param current_year Numeric year for which to extract distributions
#'
#' @return Data frame with updated risk factors for all individuals based on
#'   current year distributions while maintaining NHS Health Check intervention effects
#'   and preserving recalculated percentile ranks
#'
#' @export
update_risk_factors <- function(population, longitudinal_hse_distributions, current_year, verbose = TRUE) {
  
  # Calculate new age groups based on current age
  population$new_age_group <- case_when(
    population$age < 16 ~ "0-16",
    population$age < 25 ~ "16 - 24 years",
    population$age < 35 ~ "25 - 34 years",
    population$age < 45 ~ "35 - 44 years",
    population$age < 55 ~ "45 - 54 years",
    population$age < 65 ~ "55 - 64 years",
    population$age < 75 ~ "65 - 74 years",
    TRUE ~ "75+ years"
  )
  
  # Initialize persistent effect columns if they don't exist
  if (!"nhs_bmi_reduction" %in% names(population)) {
    population$nhs_bmi_reduction <- 0
  }
  if (!"nhs_sbp_reduction" %in% names(population)) {
    population$nhs_sbp_reduction <- 0
  }
  
  # Extract year-specific distributions for BMI and SBP
  bmi_year_data <- longitudinal_hse_distributions$projections$bmi %>%
    dplyr::filter(year == current_year)
  
  sbp_year_data <- longitudinal_hse_distributions$projections$sbp %>%
    dplyr::filter(year == current_year)
  
  # Count individuals with active intervention effects
  active_bmi_effects <- sum(!is.na(population$nhs_bmi_reduction) & population$nhs_bmi_reduction != 0)
  active_sbp_effects <- sum(!is.na(population$nhs_sbp_reduction) & population$nhs_sbp_reduction != 0)
  
  if (verbose) {
    cat("Updating risk factors for", nrow(population), "individuals based on", current_year, "distributions\n")
    if (active_bmi_effects > 0 || active_sbp_effects > 0) {
      cat("Maintaining", active_bmi_effects, "BMI intervention effects and", active_sbp_effects, "SBP intervention effects\n")
    }
  }
  
  # Filter population to exclude 0-16 age group
  eligible_pop <- population$new_age_group != "0-16"
  
  if (any(eligible_pop)) {
    # Merge with BMI distribution data
    pop_with_bmi_dist <- population[eligible_pop, ] %>%
      left_join(bmi_year_data, by = c("new_age_group" = "age_group", "sex_label"))
    
    # Filter out individuals without distribution data
    has_bmi_dist <- !is.na(pop_with_bmi_dist$prob_Healthy_weight) & 
      !is.na(pop_with_bmi_dist$prob_Overweight) & 
      !is.na(pop_with_bmi_dist$prob_Obese)
    
    if (any(has_bmi_dist)) {
      pop_with_bmi_dist <- pop_with_bmi_dist[has_bmi_dist, ]
      
      # Generate BMI values based on percentile ranks and categories
      healthy_mask <- pop_with_bmi_dist$bmi_percentile_rank <= pop_with_bmi_dist$prob_Healthy_weight
      overweight_mask <- pop_with_bmi_dist$bmi_percentile_rank <= 
        (pop_with_bmi_dist$prob_Healthy_weight + pop_with_bmi_dist$prob_Overweight) & 
        !healthy_mask
      obese_mask <- !healthy_mask & !overweight_mask
      
      # Initialize new BMI values
      new_bmi <- numeric(nrow(pop_with_bmi_dist))
      new_bmi_category <- character(nrow(pop_with_bmi_dist))
      
      # Vectorized BMI generation
      new_bmi[healthy_mask] <- pmax(18.5, pmin(rnorm(sum(healthy_mask), mean = 22, sd = 2), 24.99))
      new_bmi_category[healthy_mask] <- "Normal weight"
      
      new_bmi[overweight_mask] <- pmax(25, pmin(rnorm(sum(overweight_mask), mean = 27, sd = 1.5), 29.99))
      new_bmi_category[overweight_mask] <- "Overweight"
      
      new_bmi[obese_mask] <- pmax(30, rnorm(sum(obese_mask), mean = 33, sd = 3))
      new_bmi_category[obese_mask] <- "Obese"
      
      # Apply NHS Health Check persistent BMI effects vectorized
      has_bmi_effect <- !is.na(pop_with_bmi_dist$nhs_bmi_reduction) & pop_with_bmi_dist$nhs_bmi_reduction != 0
      new_bmi[has_bmi_effect] <- new_bmi[has_bmi_effect] + pop_with_bmi_dist$nhs_bmi_reduction[has_bmi_effect]
      
      # Recategorize BMI after applying effects
      new_bmi_category[has_bmi_effect] <- case_when(
        new_bmi[has_bmi_effect] < 18.5 ~ "Underweight",
        new_bmi[has_bmi_effect] < 25 ~ "Normal weight",
        new_bmi[has_bmi_effect] < 30 ~ "Overweight",
        TRUE ~ "Obese"
      )
      
      # Update population BMI values
      eligible_indices_with_dist <- which(eligible_pop)[has_bmi_dist]
      population$bmi[eligible_indices_with_dist] <- new_bmi
      population$bmi_category[eligible_indices_with_dist] <- new_bmi_category
    }
    
    # Merge with SBP distribution data
    pop_with_sbp_dist <- population[eligible_pop, ] %>%
      left_join(sbp_year_data, by = c("new_age_group" = "age_group", "sex_label"))
    
    # Filter out individuals without distribution data
    has_sbp_dist <- !is.na(pop_with_sbp_dist$`prob__120_mmHg`) & 
      !is.na(pop_with_sbp_dist$`prob_120_140_mmHg`) & 
      !is.na(pop_with_sbp_dist$`prob__140_mmHg`)
    
    if (any(has_sbp_dist)) {
      pop_with_sbp_dist <- pop_with_sbp_dist[has_sbp_dist, ]
      
      # Vectorized SBP category assignment
      normotensive_mask <- pop_with_sbp_dist$sbp_percentile_rank <= pop_with_sbp_dist$`prob__120_mmHg`
      prehypertensive_mask <- pop_with_sbp_dist$sbp_percentile_rank <= 
        (pop_with_sbp_dist$`prob__120_mmHg` + pop_with_sbp_dist$`prob_120_140_mmHg`) & 
        !normotensive_mask
      hypertensive_mask <- !normotensive_mask & !prehypertensive_mask
      
      # Initialize new SBP values
      new_sbp <- numeric(nrow(pop_with_sbp_dist))
      new_sbp_category <- character(nrow(pop_with_sbp_dist))
      
      # Vectorized SBP generation
      new_sbp[normotensive_mask] <- pmax(90, pmin(rnorm(sum(normotensive_mask), mean = 110, sd = 8), 119))
      new_sbp_category[normotensive_mask] <- "Normotensive"
      
      new_sbp[prehypertensive_mask] <- pmax(120, pmin(rnorm(sum(prehypertensive_mask), mean = 130, sd = 6), 139))
      new_sbp_category[prehypertensive_mask] <- "Prehypertensive"
      
      new_sbp[hypertensive_mask] <- pmax(140, rnorm(sum(hypertensive_mask), mean = 150, sd = 10))
      new_sbp_category[hypertensive_mask] <- "Hypertensive"
      
      # Apply NHS Health Check persistent SBP effects vectorized
      has_sbp_effect <- !is.na(pop_with_sbp_dist$nhs_sbp_reduction) & pop_with_sbp_dist$nhs_sbp_reduction != 0
      new_sbp[has_sbp_effect] <- new_sbp[has_sbp_effect] + pop_with_sbp_dist$nhs_sbp_reduction[has_sbp_effect]
      
      # Recategorize SBP after applying effects
      new_sbp_category[has_sbp_effect] <- case_when(
        new_sbp[has_sbp_effect] < 120 ~ "Normotensive",
        new_sbp[has_sbp_effect] < 140 ~ "Prehypertensive",
        TRUE ~ "Hypertensive"
      )
      
      # Update population SBP values
      eligible_indices_with_sbp_dist <- which(eligible_pop)[has_sbp_dist]
      population$sbp[eligible_indices_with_sbp_dist] <- new_sbp
      population$sbp_category[eligible_indices_with_sbp_dist] <- new_sbp_category
    }
  }
  
  # Update current_age_group to new_age_group for all individuals
  population$current_age_group <- population$new_age_group
  
  # Remove the temporary new_age_group column
  population$new_age_group <- NULL
  
  return(population)
}


#' Update Percentile Ranks for Standard Annual Resampling
#'
#' Updates percentile ranks for individuals based on their position in 
#' current year distributions.
#'
#' @param population Data frame with updated risk factors
#' @param longitudinal_hse_distributions Distribution data
#' @param current_year Current year
#' @return Population with updated percentile ranks
update_percentile_ranks <- function(population, longitudinal_hse_distributions, current_year) {
  
  # Extract year-specific distributions for BMI and SBP
  bmi_year_data <- longitudinal_hse_distributions$projections$bmi %>%
    dplyr::filter(year == current_year)
  
  sbp_year_data <- longitudinal_hse_distributions$projections$sbp %>%
    dplyr::filter(year == current_year)
  
  if (verbose) cat("Updating percentile ranks for", nrow(population), "individuals based on", current_year, "distributions\n")
  
  # Check if individuals have had NHS Health Check interventions this year
  had_nhs_check_this_year <- !is.na(population$nhs_last_health_check_year) & 
    population$nhs_last_health_check_year == current_year
  
  # Filter eligible population (exclude 0-16 age group and recent NHS Health Check recipients)
  eligible_pop <- !is.na(population$current_age_group) & 
    population$current_age_group != "0-16" & 
    !had_nhs_check_this_year
  
  if (any(eligible_pop)) {
    # Merge with BMI distribution data for eligible population
    eligible_indices <- which(eligible_pop)
    pop_subset <- population[eligible_indices, ]
    
    pop_with_bmi_dist <- pop_subset %>%
      left_join(bmi_year_data, by = c("current_age_group" = "age_group", "sex_label"))
    
    # Vectorized BMI percentile rank updates
    valid_bmi <- !is.na(pop_with_bmi_dist$bmi) & !is.na(pop_with_bmi_dist$prob_Healthy_weight)
    
    if (any(valid_bmi)) {
      # Initialize new percentile ranks
      new_bmi_percentiles <- numeric(nrow(pop_with_bmi_dist))
      
      # Vectorized categorization and percentile assignment
      healthy_weight_mask <- valid_bmi & pop_with_bmi_dist$bmi < 25
      overweight_mask <- valid_bmi & pop_with_bmi_dist$bmi >= 25 & pop_with_bmi_dist$bmi < 30
      obese_mask <- valid_bmi & pop_with_bmi_dist$bmi >= 30
      
      # Generate random percentiles within appropriate ranges
      new_bmi_percentiles[healthy_weight_mask] <- runif(sum(healthy_weight_mask), 
                                                        0, pop_with_bmi_dist$prob_Healthy_weight[healthy_weight_mask])
      
      new_bmi_percentiles[overweight_mask] <- runif(sum(overweight_mask), 
                                                    pop_with_bmi_dist$prob_Healthy_weight[overweight_mask],
                                                    pop_with_bmi_dist$prob_Healthy_weight[overweight_mask] + 
                                                      pop_with_bmi_dist$prob_Overweight[overweight_mask])
      
      new_bmi_percentiles[obese_mask] <- runif(sum(obese_mask), 
                                               pop_with_bmi_dist$prob_Healthy_weight[obese_mask] + 
                                                 pop_with_bmi_dist$prob_Overweight[obese_mask], 1.0)
      
      # Update population with new BMI percentiles
      population$bmi_percentile_rank[eligible_indices[valid_bmi]] <- new_bmi_percentiles[valid_bmi]
    }
    
    # Merge with SBP distribution data
    pop_with_sbp_dist <- pop_subset %>%
      left_join(sbp_year_data, by = c("current_age_group" = "age_group", "sex_label"))
    
    # Vectorized SBP percentile rank updates
    valid_sbp <- !is.na(pop_with_sbp_dist$sbp) & !is.na(pop_with_sbp_dist$`prob__120_mmHg`)
    
    if (any(valid_sbp)) {
      # Initialize new percentile ranks
      new_sbp_percentiles <- numeric(nrow(pop_with_sbp_dist))
      
      # Vectorized categorization and percentile assignment
      normotensive_mask <- valid_sbp & pop_with_sbp_dist$sbp < 120
      prehypertensive_mask <- valid_sbp & pop_with_sbp_dist$sbp >= 120 & pop_with_sbp_dist$sbp < 140
      hypertensive_mask <- valid_sbp & pop_with_sbp_dist$sbp >= 140
      
      # Generate random percentiles within appropriate ranges
      new_sbp_percentiles[normotensive_mask] <- runif(sum(normotensive_mask), 
                                                      0, pop_with_sbp_dist$`prob__120_mmHg`[normotensive_mask])
      
      new_sbp_percentiles[prehypertensive_mask] <- runif(sum(prehypertensive_mask), 
                                                         pop_with_sbp_dist$`prob__120_mmHg`[prehypertensive_mask],
                                                         pop_with_sbp_dist$`prob__120_mmHg`[prehypertensive_mask] + 
                                                           pop_with_sbp_dist$`prob_120_140_mmHg`[prehypertensive_mask])
      
      new_sbp_percentiles[hypertensive_mask] <- runif(sum(hypertensive_mask), 
                                                      pop_with_sbp_dist$`prob__120_mmHg`[hypertensive_mask] + 
                                                        pop_with_sbp_dist$`prob_120_140_mmHg`[hypertensive_mask], 1.0)
      
      # Update population with new SBP percentiles
      population$sbp_percentile_rank[eligible_indices[valid_sbp]] <- new_sbp_percentiles[valid_sbp]
    }
  }
  
  return(population)
}


#' Decay NHS Health Check Effects Over Time
#'
#' Gradually reduces the persistent effects of NHS Health Check interventions
#' over time to model natural regression to baseline behaviors. 
#'
#' @param population Data frame containing population with intervention effects
#' @param current_year Current simulation year
#' @param longitudinal_hse_distributions Distribution data for percentile updates
#' @param decay_rate Annual decay rate for intervention effects (default: 0.1 = 10% per year)
#' @param min_years_before_decay Minimum years before decay begins (default: 2)
#' @param update_percentiles Whether to update percentile ranks after significant decay (default: TRUE)
#' @return Population with decayed intervention effects and updated percentiles
decay_nhs_effects <- function(population, 
                              current_year, 
                              longitudinal_hse_distributions = NULL,
                              decay_rate = 0.1, 
                              min_years_before_decay = 2,
                              update_percentiles = TRUE) {
  
  if (!"nhs_last_health_check_year" %in% names(population)) {
    return(population)  # No intervention effects to decay
  }
  
  # Calculate years since last health check vectorized
  years_since_check <- current_year - population$nhs_last_health_check_year
  
  # Apply decay only after minimum years have passed
  decay_mask <- !is.na(years_since_check) & years_since_check >= min_years_before_decay
  
  if (any(decay_mask)) {
    # Calculate decay factor vectorized
    excess_years <- pmax(0, years_since_check[decay_mask] - min_years_before_decay)
    decay_factor <- (1 - decay_rate) ^ excess_years
    
    # Track individuals with significant decay for percentile updates
    significant_decay_mask <- rep(FALSE, nrow(population))
    
    # Apply decay to BMI effects vectorized
    bmi_effect_mask <- decay_mask & !is.na(population$nhs_bmi_reduction) & population$nhs_bmi_reduction != 0
    if (any(bmi_effect_mask)) {
      original_effects <- population$nhs_bmi_reduction[bmi_effect_mask]
      decay_indices <- which(decay_mask)[bmi_effect_mask[decay_mask]]
      population$nhs_bmi_reduction[bmi_effect_mask] <- 
        population$nhs_bmi_reduction[bmi_effect_mask] * decay_factor[bmi_effect_mask[decay_mask]]
      
      # Mark for percentile update if decay is significant (> 20% change)
      significant_change <- abs(population$nhs_bmi_reduction[bmi_effect_mask] - original_effects) / abs(original_effects) > 0.2
      significant_decay_mask[decay_indices[significant_change]] <- TRUE
      
      # Remove very small effects vectorized
      small_effects_mask <- abs(population$nhs_bmi_reduction) < 0.01
      population$nhs_bmi_reduction[small_effects_mask] <- 0
    }
    
    # Apply decay to SBP effects vectorized
    sbp_effect_mask <- decay_mask & !is.na(population$nhs_sbp_reduction) & population$nhs_sbp_reduction != 0
    if (any(sbp_effect_mask)) {
      original_effects <- population$nhs_sbp_reduction[sbp_effect_mask]
      decay_indices <- which(decay_mask)[sbp_effect_mask[decay_mask]]
      population$nhs_sbp_reduction[sbp_effect_mask] <- 
        population$nhs_sbp_reduction[sbp_effect_mask] * decay_factor[sbp_effect_mask[decay_mask]]
      
      # Mark for percentile update if decay is significant (> 20% change)
      significant_change <- abs(population$nhs_sbp_reduction[sbp_effect_mask] - original_effects) / abs(original_effects) > 0.2
      significant_decay_mask[decay_indices[significant_change]] <- TRUE
      
      # Remove very small effects vectorized
      small_effects_mask <- abs(population$nhs_sbp_reduction) < 0.1
      population$nhs_sbp_reduction[small_effects_mask] <- 0
    }
    
    # Update percentile ranks for individuals with significant decay
    if (update_percentiles && any(significant_decay_mask) && !is.null(longitudinal_hse_distributions)) {
      population <- update_percentile_ranks_for_subset(
        population = population,
        indices = which(significant_decay_mask),
        longitudinal_hse_distributions = longitudinal_hse_distributions,
        current_year = current_year
      )
    }
    
    if (verbose) cat("Applied decay to intervention effects for", sum(decay_mask), "individuals\n")
  }
  
  return(population)
}

#' Update percentile ranks for a subset of individuals
#'
#' Helper function to update percentile ranks for specific individuals
update_percentile_ranks_for_subset <- function(population, 
                                               indices, 
                                               longitudinal_hse_distributions, 
                                               current_year) {
  
  if (length(indices) == 0) {
    return(population)
  }
  
  # Extract year-specific distributions
  bmi_year_data <- longitudinal_hse_distributions$projections$bmi %>%
    dplyr::filter(year == current_year)
  
  sbp_year_data <- longitudinal_hse_distributions$projections$sbp %>%
    dplyr::filter(year == current_year)
  
  # Get subset of population
  pop_subset <- population[indices, ]
  
  # Filter eligible individuals
  eligible_mask <- !is.na(pop_subset$current_age_group) & pop_subset$current_age_group != "0-16"
  eligible_indices <- indices[eligible_mask]
  
  if (length(eligible_indices) == 0) {
    return(population)
  }
  
  eligible_subset <- population[eligible_indices, ]
  
  # Update BMI percentile ranks vectorized
  pop_with_bmi_dist <- eligible_subset %>%
    left_join(bmi_year_data, by = c("current_age_group" = "age_group", "sex_label"))
  
  valid_bmi <- !is.na(pop_with_bmi_dist$bmi) & !is.na(pop_with_bmi_dist$prob_Healthy_weight)
  
  if (any(valid_bmi)) {
    new_bmi_percentiles <- numeric(nrow(pop_with_bmi_dist))
    
    healthy_weight_mask <- valid_bmi & pop_with_bmi_dist$bmi < 25
    overweight_mask <- valid_bmi & pop_with_bmi_dist$bmi >= 25 & pop_with_bmi_dist$bmi < 30
    obese_mask <- valid_bmi & pop_with_bmi_dist$bmi >= 30
    
    new_bmi_percentiles[healthy_weight_mask] <- runif(sum(healthy_weight_mask), 
                                                      0, pop_with_bmi_dist$prob_Healthy_weight[healthy_weight_mask])
    new_bmi_percentiles[overweight_mask] <- runif(sum(overweight_mask), 
                                                  pop_with_bmi_dist$prob_Healthy_weight[overweight_mask],
                                                  pop_with_bmi_dist$prob_Healthy_weight[overweight_mask] + 
                                                    pop_with_bmi_dist$prob_Overweight[overweight_mask])
    new_bmi_percentiles[obese_mask] <- runif(sum(obese_mask), 
                                             pop_with_bmi_dist$prob_Healthy_weight[obese_mask] + 
                                               pop_with_bmi_dist$prob_Overweight[obese_mask], 1.0)
    
    population$bmi_percentile_rank[eligible_indices[valid_bmi]] <- new_bmi_percentiles[valid_bmi]
  }
  
  # Update SBP percentile ranks vectorized
  pop_with_sbp_dist <- eligible_subset %>%
    left_join(sbp_year_data, by = c("current_age_group" = "age_group", "sex_label"))
  
  valid_sbp <- !is.na(pop_with_sbp_dist$sbp) & !is.na(pop_with_sbp_dist$`prob__120_mmHg`)
  
  if (any(valid_sbp)) {
    new_sbp_percentiles <- numeric(nrow(pop_with_sbp_dist))
    
    normotensive_mask <- valid_sbp & pop_with_sbp_dist$sbp < 120
    prehypertensive_mask <- valid_sbp & pop_with_sbp_dist$sbp >= 120 & pop_with_sbp_dist$sbp < 140
    hypertensive_mask <- valid_sbp & pop_with_sbp_dist$sbp >= 140
    
    new_sbp_percentiles[normotensive_mask] <- runif(sum(normotensive_mask), 
                                                    0, pop_with_sbp_dist$`prob__120_mmHg`[normotensive_mask])
    new_sbp_percentiles[prehypertensive_mask] <- runif(sum(prehypertensive_mask), 
                                                       pop_with_sbp_dist$`prob__120_mmHg`[prehypertensive_mask],
                                                       pop_with_sbp_dist$`prob__120_mmHg`[prehypertensive_mask] + 
                                                         pop_with_sbp_dist$`prob_120_140_mmHg`[prehypertensive_mask])
    new_sbp_percentiles[hypertensive_mask] <- runif(sum(hypertensive_mask), 
                                                    pop_with_sbp_dist$`prob__120_mmHg`[hypertensive_mask] + 
                                                      pop_with_sbp_dist$`prob_120_140_mmHg`[hypertensive_mask], 1.0)
    
    population$sbp_percentile_rank[eligible_indices[valid_sbp]] <- new_sbp_percentiles[valid_sbp]
  }
  
  return(population)
}


#' Clear Expired NHS Health Check Effects
#'
#' Removes intervention effects for individuals who have not had a health check
#' in more than 5 years (as per NHS Health Check guidelines).
#'
#' @param population Data frame containing population
#' @param current_year Current simulation year
#' @param longitudinal_hse_distributions Distribution data for percentile updates
#' @param max_effect_duration Maximum years for effects to persist (default: 5)
#' @param update_percentiles Whether to update percentile ranks after clearing effects (default: TRUE)
#' @return Population with expired effects cleared and percentiles updated
clear_expired_nhs_effects <- function(population, 
                                      current_year, 
                                      longitudinal_hse_distributions = NULL,
                                      max_effect_duration = 5,
                                      update_percentiles = TRUE) {
  
  if (!"nhs_last_health_check_year" %in% names(population)) {
    return(population)
  }
  
  # Identify individuals with expired effects vectorized
  years_since_check <- current_year - population$nhs_last_health_check_year
  expired_mask <- !is.na(years_since_check) & years_since_check > max_effect_duration
  
  if (any(expired_mask)) {
    # Clear expired effects vectorized
    population$nhs_bmi_reduction[expired_mask] <- 0
    population$nhs_sbp_reduction[expired_mask] <- 0
    population$nhs_last_health_check_year[expired_mask] <- NA_real_
    
    # Update percentile ranks for individuals whose effects expired
    if (update_percentiles && !is.null(longitudinal_hse_distributions)) {
      population <- update_percentile_ranks_for_subset(
        population = population,
        indices = which(expired_mask),
        longitudinal_hse_distributions = longitudinal_hse_distributions,
        current_year = current_year
      )
    }
    
    if (verbose) cat("Cleared expired effects for", sum(expired_mask), "individuals\n")
  }
  
  return(population)
}


#' Summarize NHS Health Check Effects in Population
#'
#' Provides summary statistics on active NHS Health Check intervention effects.
#'
#' @param population Data frame containing population
#' @param current_year Current simulation year
#' @return Summary statistics
summarize_nhs_effects <- function(population, current_year) {
  
  if (!"nhs_bmi_reduction" %in% names(population)) {
    return(list(message = "No NHS Health Check effects found in population"))
  }
  
  # Count active effects vectorized
  active_bmi_effects <- sum(!is.na(population$nhs_bmi_reduction) & population$nhs_bmi_reduction != 0)
  active_sbp_effects <- sum(!is.na(population$nhs_sbp_reduction) & population$nhs_sbp_reduction != 0)
  
  # Calculate mean effect sizes vectorized
  bmi_effects <- population$nhs_bmi_reduction[!is.na(population$nhs_bmi_reduction) & population$nhs_bmi_reduction != 0]
  sbp_effects <- population$nhs_sbp_reduction[!is.na(population$nhs_sbp_reduction) & population$nhs_sbp_reduction != 0]
  
  mean_bmi_reduction <- ifelse(length(bmi_effects) > 0, mean(bmi_effects), NA)
  mean_sbp_reduction <- ifelse(length(sbp_effects) > 0, mean(sbp_effects), NA)
  
  # Years since last health check distribution vectorized
  years_since_check <- current_year - population$nhs_last_health_check_year
  years_distribution <- table(years_since_check, useNA = "ifany")
  
  summary_stats <- list(
    current_year = current_year,
    total_population = nrow(population),
    active_bmi_effects = active_bmi_effects,
    active_sbp_effects = active_sbp_effects,
    pct_with_bmi_effects = round(100 * active_bmi_effects / nrow(population), 2),
    pct_with_sbp_effects = round(100 * active_sbp_effects / nrow(population), 2),
    mean_bmi_reduction = ifelse(is.finite(mean_bmi_reduction), round(mean_bmi_reduction, 3), NA),
    mean_sbp_reduction = ifelse(is.finite(mean_sbp_reduction), round(mean_sbp_reduction, 2), NA),
    years_since_check_distribution = years_distribution
  )
  
  return(summary_stats)
}

# Example usage:
# Update risk factors for all individuals based on current year distributions
# population <- update_risk_factors(population, longitudinal_hse_distributions, current_year)
#
# Optionally apply decay to intervention effects with percentile updates
# population <- decay_nhs_effects(population, current_year, longitudinal_hse_distributions, decay_rate = 0.1)
#
# Clear expired effects with percentile updates
# population <- clear_expired_nhs_effects(population, current_year, longitudinal_hse_distributions)
#
# Get summary of active effects
# effects_summary <- summarize_nhs_effects(population, current_year)