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
#'   current year distributions
#'
#' @export
update_risk_factors <- function(population, longitudinal_hse_distributions, current_year) {
  
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
  
  # Extract year-specific distributions for BMI and SBP
  bmi_year_data <- longitudinal_hse_distributions$projections$bmi %>%
    filter(year == current_year)
  
  sbp_year_data <- longitudinal_hse_distributions$projections$sbp %>%
    filter(year == current_year)
  
  cat("Updating risk factors for", nrow(population), "individuals based on", current_year, "distributions\n")
  
  # Update risk factors for ALL individuals based on current year distributions
  for (i in 1:nrow(population)) {
    
    individual_sex <- population$sex_label[i]
    current_age_group <- population$new_age_group[i]  # Use updated age group
    
    # Skip if age group is 0-16 (not in longitudinal data)
    if (current_age_group == "0-16") {
      next
    }
    
    # Get percentile ranks for this individual
    bmi_percentile <- population$bmi_percentile_rank[i]
    sbp_percentile <- population$sbp_percentile_rank[i]
    
    # Update BMI based on current age group distribution for current year
    bmi_dist <- bmi_year_data %>%
      filter(age_group == current_age_group & sex_label == individual_sex)
    
    if (nrow(bmi_dist) > 0) {
      # Sample from the categorical distribution based on probabilities
      # Use the percentile to determine which category they fall into
      
      prob_healthy <- bmi_dist$prob_Healthy_weight[1]
      prob_overweight <- bmi_dist$prob_Overweight[1] 
      prob_obese <- bmi_dist$prob_Obese[1]
      
      # Determine category based on percentile
      if (bmi_percentile <= prob_healthy) {
        new_bmi_category <- "Normal weight"
        # Sample from healthy weight distribution (assume mean ~22, sd ~2)
        new_bmi <- rnorm(1, mean = 22, sd = 2)
        new_bmi <- pmax(18.5, pmin(new_bmi, 24.99))  # Bound to healthy range
      } else if (bmi_percentile <= (prob_healthy + prob_overweight)) {
        new_bmi_category <- "Overweight"
        # Sample from overweight distribution (assume mean ~27, sd ~1.5)
        new_bmi <- rnorm(1, mean = 27, sd = 1.5)
        new_bmi <- pmax(25, pmin(new_bmi, 29.99))  # Bound to overweight range
      } else {
        new_bmi_category <- "Obese"
        # Sample from obese distribution (assume mean ~33, sd ~3)
        new_bmi <- rnorm(1, mean = 33, sd = 3)
        new_bmi <- pmax(30, new_bmi)  # Bound to obese range minimum
      }
      
      population$bmi[i] <- new_bmi
      population$bmi_category[i] <- new_bmi_category
    }
    
    # Update SBP based on current age group distribution for current year
    sbp_dist <- sbp_year_data %>%
      filter(age_group == current_age_group & sex_label == individual_sex)
    
    if (nrow(sbp_dist) > 0) {
      # Sample from the categorical distribution based on probabilities
      
      prob_low <- sbp_dist$`prob__120_mmHg`[1]
      prob_pre <- sbp_dist$`prob_120_140_mmHg`[1]
      prob_high <- sbp_dist$`prob__140_mmHg`[1]
      
      # Determine category based on percentile
      if (sbp_percentile <= prob_low) {
        new_sbp_category <- "Normotensive"
        # Sample from normotensive distribution (assume mean ~110, sd ~8)
        new_sbp <- rnorm(1, mean = 110, sd = 8)
        new_sbp <- pmax(90, pmin(new_sbp, 119))  # Bound to normotensive range
      } else if (sbp_percentile <= (prob_low + prob_pre)) {
        new_sbp_category <- "Prehypertensive"
        # Sample from prehypertensive distribution (assume mean ~130, sd ~6)
        new_sbp <- rnorm(1, mean = 130, sd = 6)
        new_sbp <- pmax(120, pmin(new_sbp, 139))  # Bound to prehypertensive range
      } else {
        new_sbp_category <- "Hypertensive"
        # Sample from hypertensive distribution (assume mean ~150, sd ~10)
        new_sbp <- rnorm(1, mean = 150, sd = 10)
        new_sbp <- pmax(140, new_sbp)  # Bound to hypertensive range minimum
      }
      
      population$sbp[i] <- new_sbp
      population$sbp_category[i] <- new_sbp_category
    }
  }
  
  # Update current_age_group to new_age_group for all individuals
  population$current_age_group <- population$new_age_group
  
  # Remove the temporary new_age_group column
  population$new_age_group <- NULL
  
  return(population)
}

# Example usage:
# Update risk factors for all individuals based on current year distributions
# population <- update_risk_factors(population, longitudinal_hse_distributions, current_year)