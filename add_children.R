#' Generate a new child individual with all microsimulation fields initialized
#'
#' @param maternal_age Age of the mother at birth
#' @param ons_distributions ONS population distributions
#' @param hse_distributions HSE health distributions
#' @param current_year Current simulation year
#' @return List representing a newborn individual
generate_child <- function(maternal_age, ons_distributions, hse_distributions, current_year, sim_end_year = 2040) {
  
  # Sample child's sex (approximately 51% male, 49% female births)
  child_gender <- sample(c("Men", "Women"), 1, prob = c(
    ons_distributions$gender_distributions$prop_sex["Men"],
    ons_distributions$gender_distributions$prop_sex["Women"]))
  
  # Child starts at age 0
  child_age <- 0
  age_group <- "0 - 1 years"
  
  # Sample BMI from HSE distributions for 0-1 years age group
  bmi_params <- hse_distributions$distributions$bmi %>%
    dplyr::filter(age_group == !!age_group, sex_label == child_gender)
  
  if (nrow(bmi_params) == 0 || is.na(bmi_params$mean) || is.na(bmi_params$sd)) {
    stop(paste("No BMI distribution found for:", age_group, child_gender))
  }
  
  bmi <- rnorm(1, bmi_params$mean, bmi_params$sd)
  if (!is.na(bmi_params$min_val)) bmi <- pmax(bmi, bmi_params$min_val)
  if (!is.na(bmi_params$max_val)) bmi <- pmin(bmi, bmi_params$max_val)
  
  # Calculate BMI percentile rank
  bmi_percentile_rank <- calculate_percentile_rank(bmi, bmi_params$mean, bmi_params$sd)
  
  # Assign BMI category based on sampled value
  bmi_category <- case_when(
    bmi < 18.5 ~ "Underweight",
    bmi >= 18.5 & bmi < 25 ~ "Normal weight",
    bmi >= 25 & bmi < 30 ~ "Overweight",
    bmi >= 30 ~ "Obese",
    TRUE ~ "Normal weight"
  )
  
  # Sample SBP from HSE distributions for 0-1 years age group
  sbp_params <- hse_distributions$distributions$sbp %>%
    dplyr::filter(age_group == !!age_group, sex_label == child_gender)
  
  if (nrow(sbp_params) == 0 || is.na(sbp_params$mean) || is.na(sbp_params$sd)) {
    stop(paste("No SBP distribution found for:", age_group, child_gender))
  }
  
  base_sbp <- rnorm(1, sbp_params$mean, sbp_params$sd)
  
  # Age adjustment for SBP
  age_adjustment <- pmax(0, (child_age - 25) * 0.35) # Will be 0 for newborns
  sbp <- base_sbp + age_adjustment
  
  if (!is.na(sbp_params$min_val)) sbp <- pmax(sbp, sbp_params$min_val)
  if (!is.na(sbp_params$max_val)) sbp <- pmin(sbp, sbp_params$max_val)
  
  # Calculate SBP percentile rank (using base SBP before age adjustment)
  sbp_percentile_rank <- calculate_percentile_rank(base_sbp, sbp_params$mean, sbp_params$sd)
  
  # Assign SBP category based on sampled value
  sbp_category <- case_when(
    sbp < 120 ~ "Normotensive",
    sbp >= 120 & sbp < 140 ~ "Prehypertensive",
    sbp >= 140 ~ "Hypertensive",
    TRUE ~ "Normotensive"
  )
  
  # Sample smoking status from proportions for 0-1 years age group
  smoking_props <- hse_distributions$proportions$smoking %>%
    dplyr::filter(age_group == !!age_group, sex_label == child_gender)
  
  if (nrow(smoking_props) == 0) {
    stop(paste("No smoking proportions found for:", age_group, child_gender))
  }
  
  smoking_status <- sample(
    c("Never smoker", "Former smoker", "Current smoker"),
    1,
    prob = c(smoking_props$prop_never, smoking_props$prop_ex,
             smoking_props$prop_current)
  )
  
  # Calculate smoking percentile rank
  smoking_percentile_rank <- calculate_smoking_percentile_rank(smoking_status, smoking_props)
  
  # Birth history (empty for newborns)
  birth_history <- list(n_births = 0, birth_ages = numeric(0))
  future_birth_years <- integer(0)
  
  # Generate future birth years if female
  if (child_gender == "Women") {
    future_birth_years <- generate_future_birth_years(child_age, ons_distributions, current_year, sim_end_year)
  }
  
  # Change age group to match HSE longitudinal dataset
  age_group <- "0 - 16 years"
  
  # Create child individual structure with ALL microsimulation fields
  child <- list(
    age = child_age,
    sex_label = child_gender,
    current_age_group = age_group,
    
    # Risk factors
    bmi = bmi,
    bmi_category = bmi_category,
    bmi_percentile_rank = bmi_percentile_rank,
    
    sbp = sbp,
    sbp_category = sbp_category,
    sbp_percentile_rank = sbp_percentile_rank,
    
    smoking_status = smoking_status,
    smoking_percentile_rank = smoking_percentile_rank,
    
    # Birth history
    n_births = birth_history$n_births,
    birth_ages = birth_history$birth_ages,
    birth_years = list(future_birth_years),
    
    # Alive flag
    alive = TRUE,
    
    nhs_last_health_check_year = NA,
    
    # Disease status (all FALSE for newborns)
    chd = FALSE,
    colorectal_cancer = FALSE,
    lung_cancer = FALSE,
    stroke = FALSE,
    copd = FALSE,
    
    # Disease years (all 0 for newborns)
    chd_years = 0,
    colorectal_cancer_years = 0,
    lung_cancer_years = 0,
    stroke_years = 0,
    copd_years = 0,
    
    # Disease incidence probability columns (initialized to 0)
    CHD_probability = 0,
    CHD_baseline_prob = 0,
    CHD_smoking_rr = 1,
    CHD_bp_rr = 1,
    CHD_bmi_rr = 1,
    CHD_combined_rr = 1,
    
    stroke_probability = 0,
    stroke_baseline_prob = 0,
    stroke_smoking_rr = 1,
    stroke_bp_rr = 1,
    stroke_bmi_rr = 1,
    stroke_combined_rr = 1,
    
    COPD_probability = 0,
    COPD_baseline_prob = 0,
    COPD_smoking_rr = 1,
    COPD_bp_rr = 1,
    COPD_bmi_rr = 1,
    COPD_combined_rr = 1,
    
    lung_cancer_probability = 0,
    lung_cancer_baseline_prob = 0,
    lung_cancer_smoking_rr = 1,
    lung_cancer_bp_rr = 1,
    lung_cancer_bmi_rr = 1,
    lung_cancer_combined_rr = 1,
    
    colorectal_cancer_probability = 0,
    colorectal_cancer_baseline_prob = 0,
    colorectal_cancer_smoking_rr = 1,
    colorectal_cancer_bp_rr = 1,
    colorectal_cancer_bmi_rr = 1,
    colorectal_cancer_combined_rr = 1,
    
    # Disease incidence year tracking (initialized to NA)
    chd_incidence_year = NA,
    stroke_incidence_year = NA,
    copd_incidence_year = NA,
    lung_cancer_incidence_year = NA,
    colorectal_cancer_incidence_year = NA,
    
    # Mortality probability columns (initialized to 0)
    CHD_mortality_prob = 0,
    stroke_mortality_prob = 0,
    COPD_mortality_prob = 0,
    lung_cancer_mortality_prob = 0,
    colorectal_cancer_mortality_prob = 0,
    other_cause_mortality_prob = 0,
    total_mortality_prob = 0,
    
    # Death tracking columns (initialized to NA)
    death_year = NA,
    cause_of_death = NA,
    
    # Additional metadata
    birth_year = current_year,
    maternal_age = maternal_age
  )
  
  return(child)
}

#' Check for births and generate children in current simulation year
#'
#' @param population Current population data frame
#' @param current_year Current simulation year
#' @param ons_distributions ONS population distributions
#' @param hse_distributions HSE health distributions
#' @return Updated population data frame with new children added
process_births_and_add_children <- function(population,
                                            current_year,
                                            ons_distributions,
                                            hse_distributions,
                                            sim_end_year = 2040,
                                            verbose = TRUE) {
  
  # Find women who are giving birth this year
  women_giving_birth <- population %>%
    dplyr::filter(sex_label == "Women") %>%
    rowwise() %>%
    mutate(
      giving_birth_this_year = if (length(birth_years) > 0 && !is.null(birth_years[[1]])) {
        current_year %in% birth_years[[1]]
      } else {
        FALSE
      }
    ) %>%
    ungroup() %>%
    dplyr::filter(giving_birth_this_year == TRUE)
  
  # If no births this year, return original population
  if (nrow(women_giving_birth) == 0) {
    if (verbose) cat("No births in year", current_year, "\n")
    return(population)
  }
  
  if (verbose) cat("Processing", nrow(women_giving_birth), "births in year", current_year, "\n")
  
  # Generate children for each birth
  new_children_list <- list()
  child_counter <- 1
  
  for (i in 1:nrow(women_giving_birth)) {
    mother <- women_giving_birth[i, ]
    maternal_age <- mother$age
    
    mother_birth_years_vector <- if (length(mother$birth_years[[1]]) > 0) {
      mother$birth_years[[1]]
    } else {
      integer(0)
    }
    
    # Count how many births this mother has this year
    n_births_this_year <- sum(mother_birth_years_vector == current_year)
    
    # Generate each child
    for (birth_num in 1:n_births_this_year) {
      child <- generate_child(
        maternal_age = maternal_age,
        ons_distributions = ons_distributions,
        hse_distributions = hse_distributions,
        current_year = current_year,
        sim_end_year = sim_end_year
      )
      
      new_children_list[[child_counter]] <- child
      child_counter <- child_counter + 1
    }
    
    # Update mother's birth history - remove this year's births from future births
    mother_idx <- which(population$id == mother$id)
    if (length(mother_idx) > 0) {
      
      original_birth_years_vector <- mother_birth_years_vector
      
      updated_birth_years <- original_birth_years_vector[original_birth_years_vector != current_year]
      population$birth_years[[mother_idx]] <- updated_birth_years
      
      # Update n_births count
      population$n_births[mother_idx] <- population$n_births[mother_idx] + n_births_this_year
      
      # Add current birth ages to birth_ages
      current_birth_ages <- population$birth_ages[[mother_idx]]
      new_birth_ages <- c(current_birth_ages, rep(maternal_age, n_births_this_year))
      population$birth_ages[[mother_idx]] <- new_birth_ages
    }
  }
  
  # If no children generated, return original population
  if (length(new_children_list) == 0) {
    return(population)
  }
  
  # Get the exact column structure from the existing population
  population_columns <- colnames(population)
  
  # Create a template row by copying the first row and modifying it
  template_row <- population[1, ]
  
  # Convert children list to data frame format
  children_df_list <- list()
  
  for (child_idx in seq_along(new_children_list)) {
    child <- new_children_list[[child_idx]]
    child_row <- template_row
    
    # Update all the values for this child
    child_row$id <- max(population$id) + child_idx
    child_row$age <- child$age
    child_row$sex_label <- child$sex_label
    child_row$current_age_group <- child$current_age_group
    
    # Risk factors
    child_row$bmi <- child$bmi
    child_row$bmi_category <- child$bmi_category
    child_row$bmi_percentile_rank <- child$bmi_percentile_rank
    
    child_row$sbp <- child$sbp
    child_row$sbp_category <- child$sbp_category
    child_row$sbp_percentile_rank <- child$sbp_percentile_rank
    
    child_row$smoking_status <- child$smoking_status
    child_row$smoking_percentile_rank <- child$smoking_percentile_rank
    
    # Birth history
    child_row$n_births <- child$n_births
    child_row$birth_ages <- list(child$birth_ages)
    child_row$birth_years <- list(child$birth_years[[1]])
    
    # Alive flag
    child_row$alive <- child$alive
    
    child_row$nhs_last_health_check_year <- child$nhs_last_health_check_year
    
    # Disease status
    child_row$chd <- child$chd
    child_row$colorectal_cancer <- child$colorectal_cancer
    child_row$lung_cancer <- child$lung_cancer
    child_row$stroke <- child$stroke
    child_row$copd <- child$copd
    
    # Disease years
    child_row$chd_years <- child$chd_years
    child_row$colorectal_cancer_years <- child$colorectal_cancer_years
    child_row$lung_cancer_years <- child$lung_cancer_years
    child_row$stroke_years <- child$stroke_years
    child_row$copd_years <- child$copd_years
    
    # Disease incidence probabilities and relative risks
    child_row$CHD_probability <- child$CHD_probability
    child_row$CHD_baseline_prob <- child$CHD_baseline_prob
    child_row$CHD_smoking_rr <- child$CHD_smoking_rr
    child_row$CHD_bp_rr <- child$CHD_bp_rr
    child_row$CHD_bmi_rr <- child$CHD_bmi_rr
    child_row$CHD_combined_rr <- child$CHD_combined_rr
    
    child_row$stroke_probability <- child$stroke_probability
    child_row$stroke_baseline_prob <- child$stroke_baseline_prob
    child_row$stroke_smoking_rr <- child$stroke_smoking_rr
    child_row$stroke_bp_rr <- child$stroke_bp_rr
    child_row$stroke_bmi_rr <- child$stroke_bmi_rr
    child_row$stroke_combined_rr <- child$stroke_combined_rr
    
    child_row$COPD_probability <- child$COPD_probability
    child_row$COPD_baseline_prob <- child$COPD_baseline_prob
    child_row$COPD_smoking_rr <- child$COPD_smoking_rr
    child_row$COPD_bp_rr <- child$COPD_bp_rr
    child_row$COPD_bmi_rr <- child$COPD_bmi_rr
    child_row$COPD_combined_rr <- child$COPD_combined_rr
    
    child_row$lung_cancer_probability <- child$lung_cancer_probability
    child_row$lung_cancer_baseline_prob <- child$lung_cancer_baseline_prob
    child_row$lung_cancer_smoking_rr <- child$lung_cancer_smoking_rr
    child_row$lung_cancer_bp_rr <- child$lung_cancer_bp_rr
    child_row$lung_cancer_bmi_rr <- child$lung_cancer_bmi_rr
    child_row$lung_cancer_combined_rr <- child$lung_cancer_combined_rr
    
    child_row$colorectal_cancer_probability <- child$colorectal_cancer_probability
    child_row$colorectal_cancer_baseline_prob <- child$colorectal_cancer_baseline_prob
    child_row$colorectal_cancer_smoking_rr <- child$colorectal_cancer_smoking_rr
    child_row$colorectal_cancer_bp_rr <- child$colorectal_cancer_bp_rr
    child_row$colorectal_cancer_bmi_rr <- child$colorectal_cancer_bmi_rr
    child_row$colorectal_cancer_combined_rr <- child$colorectal_cancer_combined_rr
    
    # Disease incidence year tracking
    child_row$chd_incidence_year <- child$chd_incidence_year
    child_row$stroke_incidence_year <- child$stroke_incidence_year
    child_row$copd_incidence_year <- child$copd_incidence_year
    child_row$lung_cancer_incidence_year <- child$lung_cancer_incidence_year
    child_row$colorectal_cancer_incidence_year <- child$colorectal_cancer_incidence_year
    
    # Mortality probabilities
    child_row$CHD_mortality_prob <- child$CHD_mortality_prob
    child_row$stroke_mortality_prob <- child$stroke_mortality_prob
    child_row$COPD_mortality_prob <- child$COPD_mortality_prob
    child_row$lung_cancer_mortality_prob <- child$lung_cancer_mortality_prob
    child_row$colorectal_cancer_mortality_prob <- child$colorectal_cancer_mortality_prob
    child_row$other_cause_mortality_prob <- child$other_cause_mortality_prob
    child_row$total_mortality_prob <- child$total_mortality_prob
    
    # Death tracking
    child_row$death_year <- child$death_year
    child_row$cause_of_death <- child$cause_of_death
    
    child_row$birth_year <- child$birth_year
    child_row$maternal_age <- child$maternal_age
    
    children_df_list[[child_idx]] <- child_row
  }
  
  # Combine all child rows into a single data frame
  children_df <- dplyr::bind_rows(children_df_list)
  if (verbose) {
    print(setdiff(colnames(population), colnames(children_df)))
    print(setdiff(colnames(children_df), colnames(population)))
  }
  # Ensure exact column structure match
  if (!identical(colnames(population), colnames(children_df))) {
    stop("Column mismatch between population and children_df")
  }
  
  # Combine original population with new children
  updated_population <- dplyr::bind_rows(population, children_df)
  
  if (verbose) {
    cat("Added", nrow(children_df), "new children to population\n")
    cat("Population size increased from", nrow(population), "to", nrow(updated_population), "\n")
  }
  
  return(updated_population)
}

# Example Usage
#
# # Process births for year 2025
# population <- process_births_and_add_children(
# population,
# current_year,
# ons_distributions,
# hse_distributions
# )