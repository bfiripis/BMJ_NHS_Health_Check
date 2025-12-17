#' Apply NHS Health Check Intervention
#'
#' Implements the NHS Health Check intervention for eligible individuals in the population.
#'
#' @param population Population dataframe with individual health data
#' @param current_year Current simulation year
#' @param longitudinal_hse_distributions Longitudinal HSE distributions for percentile recalculation
#' @param scenario_type Character: "baseline" or "intervention" 
#' @param male_attendance_rate Numeric: Proportion of eligible males who attend (0-1)
#' @param female_attendance_rate Numeric: Proportion of eligible females who attend (0-1)
#' @param intervention_cost Numeric: Cost per health check attendance (£)
#' @param bmi_reduction Numeric: BMI reduction in kg/m² for obese attendees (default: -0.3)
#' @param sbp_reduction Numeric: SBP reduction in mmHg for hypertensive attendees (default: -3.22)
#' @param smoking_cessation_rate Numeric: Probability of smoking cessation for current smokers (0-1, default: 0.0635)
#' @param seed Numeric: Random seed for reproducibility
#'
#' @return List containing:
#'   - population: Updated population dataframe with recalculated percentile ranks
#'   - intervention_costs: Total costs incurred this year
#'   - attendance_summary: Summary of attendance by demographics
#'
#' @export
apply_nhs_health_check <- function(population,
                                   current_year,
                                   longitudinal_hse_distributions,
                                   scenario_type = "baseline",
                                   male_attendance_rate = 0.3803,
                                   female_attendance_rate = 0.4396,
                                   intervention_cost = 150,
                                   bmi_reduction = -0.3,
                                   sbp_reduction = -3.22,
                                   smoking_cessation_rate = 0.0635,
                                   seed = NULL,
                                   verbose = TRUE) {
  
  # Set seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Initialize persistent effect columns
  if (!"nhs_bmi_reduction" %in% names(population)) {
    population$nhs_bmi_reduction <- 0
  }
  if (!"nhs_sbp_reduction" %in% names(population)) {
    population$nhs_sbp_reduction <- 0
  }
  if (!"nhs_last_health_check_year" %in% names(population)) {
    population$nhs_last_health_check_year <- NA_real_
  }
  
  if (!"last_health_check_year" %in% names(population)) {
    population$last_health_check_year <- population$nhs_last_health_check_year
  }
  
  # Determine eligibility for NHS Health Check
  eligible_individuals <- identify_eligible_individuals_compatible(population, current_year)
  
  if (verbose) cat("NHS Health Check", current_year, ":", nrow(eligible_individuals), "eligible individuals\n")
  
  if (nrow(eligible_individuals) == 0) {
    return(list(
      population = population,
      intervention_costs = 0,
      attendance_summary = data.frame(
        eligible_males = 0, attending_males = 0,
        eligible_females = 0, attending_females = 0,
        total_eligible = 0, total_attending = 0
      )
    ))
  }
  
  # Determine attendance
  attendance_result <- determine_attendance_compatible(eligible_individuals, 
                                                       male_attendance_rate, 
                                                       female_attendance_rate)
  attendees <- attendance_result$attendees
  summary <- attendance_result$summary
  
  if (verbose) cat("Attendance:", nrow(attendees), "of", nrow(eligible_individuals), "eligible individuals\n")
  
  # Calculate intervention costs
  total_cost <- nrow(attendees) * intervention_cost
  
  if (nrow(attendees) > 0) {
    # Apply risk factor modifications with persistent effects
    attendees_modified <- apply_risk_factor_modifications_with_percentile_update(
      attendees, 
      current_year,
      longitudinal_hse_distributions,
      bmi_reduction, 
      sbp_reduction, 
      smoking_cessation_rate,
      verbose
    )
    
    # Update the population with modified attendees
    population[population$id %in% attendees_modified$id, ] <- attendees_modified
    
    if (verbose) {
      cat("Applied interventions to", nrow(attendees_modified), "attendees\n")
      cat("BMI reductions:", sum(!is.na(attendees_modified$nhs_bmi_reduction) & attendees_modified$nhs_bmi_reduction != 0), "individuals\n")
      cat("SBP reductions:", sum(!is.na(attendees_modified$nhs_sbp_reduction) & attendees_modified$nhs_sbp_reduction != 0), "individuals\n")
    }
  }
  
  return(list(
    population = population,
    intervention_costs = total_cost,
    attendance_summary = summary
  ))
}

#' Identify eligible individuals
identify_eligible_individuals_compatible <- function(population, current_year) {
  
  # Start with basic age and disease eligibility
  eligible_mask <- (
    population$age >= 40 & 
      population$age <= 75
  )
  
  # Add alive filter if column exists (handle both TRUE/FALSE and alive/dead)
  if ("alive" %in% names(population)) {
    if (is.logical(population$alive)) {
      eligible_mask <- eligible_mask & (population$alive == TRUE)
    } else {
      # Handle character values like "alive"/"dead"
      eligible_mask <- eligible_mask & (population$alive == "alive" | population$alive == TRUE)
    }
  }
  
  # CHD filter (handle both simple boolean and incidence year)
  if ("chd_incidence_year" %in% names(population)) {
    eligible_mask <- eligible_mask & (
      (is.na(population$chd) | population$chd == FALSE | 
         is.na(population$chd_incidence_year) | population$chd_incidence_year > current_year)
    )
  } else {
    eligible_mask <- eligible_mask & (
      (is.na(population$chd) | population$chd == FALSE)
    )
  }
  
  # Stroke filter (handle both simple boolean and incidence year)  
  if ("stroke_incidence_year" %in% names(population)) {
    eligible_mask <- eligible_mask & (
      (is.na(population$stroke) | population$stroke == FALSE |
         is.na(population$stroke_incidence_year) | population$stroke_incidence_year > current_year)
    )
  } else {
    eligible_mask <- eligible_mask & (
      (is.na(population$stroke) | population$stroke == FALSE)
    )
  }
  
  # Health check timing filter (handle both column name variants)
  if ("last_health_check_year" %in% names(population)) {
    eligible_mask <- eligible_mask & (
      (is.na(population$last_health_check_year) | 
         (current_year - population$last_health_check_year) >= 5)
    )
  } else if ("nhs_last_health_check_year" %in% names(population)) {
    eligible_mask <- eligible_mask & (
      (is.na(population$nhs_last_health_check_year) | 
         (current_year - population$nhs_last_health_check_year) >= 5)
    )
  }
  
  eligible_individuals <- population[eligible_mask & !is.na(eligible_mask), ]
  
  return(eligible_individuals)
}

#' Determine attendance
determine_attendance_compatible <- function(eligible_individuals, 
                                            male_attendance_rate, 
                                            female_attendance_rate) {
  
  if (nrow(eligible_individuals) == 0) {
    return(list(
      attendees = data.frame(),
      summary = data.frame(
        eligible_males = 0, attending_males = 0,
        eligible_females = 0, attending_females = 0,
        total_eligible = 0, total_attending = 0
      )
    ))
  }
  
  # Handle both "Male"/"Female" and "Men"/"Women" sex labels
  male_labels <- c("Male", "Men", "male", "men", "M")
  female_labels <- c("Female", "Women", "female", "women", "F")
  
  # Count eligible individuals by sex
  eligible_males <- sum(eligible_individuals$sex_label %in% male_labels, na.rm = TRUE)
  eligible_females <- sum(eligible_individuals$sex_label %in% female_labels, na.rm = TRUE)
  
  # Get indices for each sex
  male_indices <- which(eligible_individuals$sex_label %in% male_labels)
  female_indices <- which(eligible_individuals$sex_label %in% female_labels)
  
  # Determine attendance using individual probability approach
  eligible_individuals$attends_health_check <- FALSE
  
  # Males - each individual has male_attendance_rate probability of attending
  if (length(male_indices) > 0) {
    male_attendance <- runif(length(male_indices)) < male_attendance_rate
    eligible_individuals$attends_health_check[male_indices] <- male_attendance
  }
  
  # Females - each individual has female_attendance_rate probability of attending
  if (length(female_indices) > 0) {
    female_attendance <- runif(length(female_indices)) < female_attendance_rate
    eligible_individuals$attends_health_check[female_indices] <- female_attendance
  }
  
  # Filter to attendees
  attendees <- eligible_individuals[eligible_individuals$attends_health_check == TRUE, ]
  
  # Remove the temporary attendance column
  if ("attends_health_check" %in% names(attendees)) {
    attendees$attends_health_check <- NULL
  }
  
  # Calculate actual attendance numbers
  attending_males <- sum(attendees$sex_label %in% male_labels, na.rm = TRUE)
  attending_females <- sum(attendees$sex_label %in% female_labels, na.rm = TRUE)
  
  summary <- data.frame(
    eligible_males = eligible_males,
    attending_males = attending_males,
    eligible_females = eligible_females, 
    attending_females = attending_females,
    total_eligible = nrow(eligible_individuals),
    total_attending = nrow(attendees)
  )
  
  return(list(
    attendees = attendees,
    summary = summary
  ))
}

#' Apply risk factor modifications with persistent effects and percentile rank recalculation
#'
#' This function applies the NHS Health Check interventions and then recalculates
#' percentile ranks to reflect the individuals' new position in the population
#' distribution after receiving the intervention.
apply_risk_factor_modifications_with_percentile_update <- function(attendees,
                                                                   current_year,
                                                                   longitudinal_hse_distributions,
                                                                   bmi_reduction,
                                                                   sbp_reduction,
                                                                   smoking_cessation_rate,
                                                                   verbose = TRUE) {
  
  attendees_modified <- attendees
  
  # Update both health check year columns
  attendees_modified$nhs_last_health_check_year <- current_year
  if ("last_health_check_year" %in% names(attendees_modified)) {
    attendees_modified$last_health_check_year <- current_year
  }
  
  # Track which individuals received interventions for percentile recalculation
  bmi_intervention_applied <- rep(FALSE, nrow(attendees_modified))
  sbp_intervention_applied <- rep(FALSE, nrow(attendees_modified))
  
  # BMI reduction for obese individuals (BMI >= 30)
  obese_mask <- !is.na(attendees_modified$bmi) & attendees_modified$bmi >= 30
  if (any(obese_mask)) {
    # Apply immediate BMI reduction
    attendees_modified$bmi[obese_mask] <- attendees_modified$bmi[obese_mask] + bmi_reduction
    
    # Set persistent effect flag
    attendees_modified$nhs_bmi_reduction[obese_mask] <- bmi_reduction
    
    # Update BMI category based on new BMI
    attendees_modified <- update_bmi_categories(attendees_modified)
    
    # Mark for percentile recalculation
    bmi_intervention_applied[obese_mask] <- TRUE
    
    if (verbose) cat("BMI reduction applied to", sum(obese_mask), "obese individuals\n")
  }
  
  # SBP reduction for hypertensive individuals  
  hypertensive_mask <- !is.na(attendees_modified$sbp) & attendees_modified$sbp >= 140
  if (any(hypertensive_mask)) {
    # Apply immediate SBP reduction
    attendees_modified$sbp[hypertensive_mask] <- attendees_modified$sbp[hypertensive_mask] + sbp_reduction
    
    # Set persistent effect flag
    attendees_modified$nhs_sbp_reduction[hypertensive_mask] <- sbp_reduction
    
    # Update SBP category based on new SBP
    attendees_modified <- update_sbp_categories(attendees_modified)
    
    # Mark for percentile recalculation
    sbp_intervention_applied[hypertensive_mask] <- TRUE
    
    if (verbose) cat("SBP reduction applied to", sum(hypertensive_mask), "hypertensive individuals\n")
  }
  
  # Smoking cessation for current smokers
  current_smokers <- which(!is.na(attendees_modified$smoking_status) & 
                             attendees_modified$smoking_status == "Current smoker")
  
  if (length(current_smokers) > 0) {
    # Determine who quits smoking
    quit_smoking <- runif(length(current_smokers)) < smoking_cessation_rate
    
    # Update smoking status for those who quit
    attendees_modified$smoking_status[current_smokers[quit_smoking]] <- "Former smoker"
    
    if (verbose && sum(quit_smoking) > 0) {
      cat("Smoking cessation applied to", sum(quit_smoking), "current smokers\n")
    }
  }
  
  # RECALCULATE PERCENTILE RANKS for individuals who received BMI/SBP interventions
  if (any(bmi_intervention_applied) || any(sbp_intervention_applied)) {
    attendees_modified <- recalculate_percentile_ranks_post_intervention(
      population = attendees_modified,
      longitudinal_hse_distributions = longitudinal_hse_distributions,
      current_year = current_year,
      bmi_intervention_mask = bmi_intervention_applied,
      sbp_intervention_mask = sbp_intervention_applied
    )
    
    if (verbose) {
      cat("Recalculated percentile ranks for", 
          sum(bmi_intervention_applied), "BMI and", 
          sum(sbp_intervention_applied), "SBP intervention recipients\n")
    }
  }
  
  return(attendees_modified)
}

#' Recalculate percentile ranks post-intervention
#'
#' Recalculates percentile ranks for individuals who received NHS Health Check
#' interventions, based on their new BMI/SBP values and the current year's
#' population distribution.
#'
#' @param population Population dataframe
#' @param longitudinal_hse_distributions Longitudinal HSE distributions
#' @param current_year Current simulation year
#' @param bmi_intervention_mask Logical vector indicating who received BMI intervention
#' @param sbp_intervention_mask Logical vector indicating who received SBP intervention
#' @return Population with updated percentile ranks
recalculate_percentile_ranks_post_intervention <- function(population,
                                                           longitudinal_hse_distributions,
                                                           current_year,
                                                           bmi_intervention_mask,
                                                           sbp_intervention_mask) {
  
  # Extract year-specific distributions for BMI and SBP
  bmi_year_data <- longitudinal_hse_distributions$projections$bmi %>%
    dplyr::filter(year == current_year)
  
  sbp_year_data <- longitudinal_hse_distributions$projections$sbp %>%
    dplyr::filter(year == current_year)
  
  # Process each individual who received an intervention
  for (i in 1:nrow(population)) {
    
    individual_sex <- population$sex_label[i]
    age_group <- population$current_age_group[i]
    
    # Skip if age group is 0-16 (not in longitudinal data)
    if (is.na(age_group) || age_group == "0-16") {
      next
    }
    
    # Recalculate BMI percentile rank if BMI intervention was applied
    if (bmi_intervention_mask[i]) {
      bmi_dist <- bmi_year_data %>%
        dplyr::filter(age_group == !!age_group & sex_label == individual_sex)
      
      if (nrow(bmi_dist) > 0 && !is.na(population$bmi[i])) {
        
        prob_healthy <- bmi_dist$prob_Healthy_weight[1]
        prob_overweight <- bmi_dist$prob_Overweight[1]
        
        current_bmi <- population$bmi[i]
        
        # Assign new percentile rank based on post-intervention BMI category
        if (current_bmi < 25) {
          # In healthy weight range - assign percentile within 0 to prob_healthy
          # Use a random value within this range to maintain variability
          population$bmi_percentile_rank[i] <- runif(1, 0, prob_healthy)
        } else if (current_bmi < 30) {
          # In overweight range
          population$bmi_percentile_rank[i] <- runif(1, prob_healthy, prob_healthy + prob_overweight)
        } else {
          # Still in obese range (reduced but not below threshold)
          population$bmi_percentile_rank[i] <- runif(1, prob_healthy + prob_overweight, 1.0)
        }
      }
    }
    
    # Recalculate SBP percentile rank if SBP intervention was applied
    if (sbp_intervention_mask[i]) {
      sbp_dist <- sbp_year_data %>%
        dplyr::filter(age_group == !!age_group & sex_label == individual_sex)
      
      if (nrow(sbp_dist) > 0 && !is.na(population$sbp[i])) {
        
        prob_low <- sbp_dist$`prob__120_mmHg`[1]
        prob_pre <- sbp_dist$`prob_120_140_mmHg`[1]
        
        current_sbp <- population$sbp[i]
        
        # Assign new percentile rank based on post-intervention SBP category
        if (current_sbp < 120) {
          # In normotensive range
          population$sbp_percentile_rank[i] <- runif(1, 0, prob_low)
        } else if (current_sbp < 140) {
          # In prehypertensive range  
          population$sbp_percentile_rank[i] <- runif(1, prob_low, prob_low + prob_pre)
        } else {
          # Still in hypertensive range (reduced but not below threshold)
          population$sbp_percentile_rank[i] <- runif(1, prob_low + prob_pre, 1.0)
        }
      }
    }
  }
  
  return(population)
}

#' Update BMI Categories
update_bmi_categories <- function(data) {
  if ("bmi_category" %in% names(data)) {
    data$bmi_category <- ifelse(is.na(data$bmi), NA_character_,
                                ifelse(data$bmi < 18.5, "Underweight", 
                                       ifelse(data$bmi < 25, "Normal weight",
                                              ifelse(data$bmi < 30, "Overweight", "Obese"))))
  }
  return(data)
}

#' Update SBP Categories  
update_sbp_categories <- function(data) {
  if ("sbp_category" %in% names(data)) {
    data$sbp_category <- ifelse(is.na(data$sbp), NA_character_,
                                ifelse(data$sbp < 120, "Normotensive",
                                       ifelse(data$sbp < 140, "Prehypertensive", "Hypertensive")))
  }
  return(data)
}