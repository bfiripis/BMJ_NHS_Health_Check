library(dplyr)

#' Sample from cumulative distributions using inverse transform sampling
sample_from_cumulative <- function(n, cumulative_dist, value_col, prob_col = "cumulative_prop") {
  u <- runif(n)
  indices <- findInterval(u, cumulative_dist[[prob_col]], left.open = FALSE) + 1
  indices[indices > nrow(cumulative_dist)] <- nrow(cumulative_dist)
  return(cumulative_dist[[value_col]][indices])
}

#' Convert continuous age to HSE 2021 age group (for initial risk factor sampling)
convert_age_to_hse_2021_group <- function(age) {
  case_when(
    age < 2 ~ "0 - 1 years",
    age < 5 ~ "2 - 4 years",
    age < 8 ~ "5 - 7 years",
    age < 11 ~ "8 - 10 years",
    age < 13 ~ "11 - 12 years",
    age < 16 ~ "13 - 15 years",
    age < 20 ~ "16 - 19 years",
    age < 25 ~ "20 - 24 years",
    age < 30 ~ "25 - 29 years",
    age < 35 ~ "30 - 34 years", 
    age < 40 ~ "35 - 39 years",
    age < 45 ~ "40 - 44 years",
    age < 50 ~ "45 - 49 years",
    age < 55 ~ "50 - 54 years",
    age < 60 ~ "55 - 59 years",
    age < 65 ~ "60 - 64 years",
    age < 70 ~ "65 - 69 years",
    age < 75 ~ "70 - 74 years", 
    age < 80 ~ "75 - 79 years",
    age < 85 ~ "80 - 84 years",
    age < 90 ~ "85 - 89 years",
    TRUE ~ "90+ years"
  )
}

#' Convert continuous age to HSE longitudinal age group (for microsimulation tracking)
convert_age_to_hse_longitudinal_group <- function(age) {
  case_when(
    age < 16 ~ "0 - 16 years",
    age < 25 ~ "16 - 24 years",
    age < 35 ~ "25 - 34 years",
    age < 45 ~ "35 - 44 years",
    age < 55 ~ "45 - 54 years",
    age < 65 ~ "55 - 64 years",
    age < 75 ~ "65 - 74 years",
    TRUE ~ "75+ years"
  )
}

#' Calculate percentile rank for a given value in a distribution
calculate_percentile_rank <- function(value, mean, sd) {
  pnorm(value, mean, sd)
}

#' Calculate percentile rank for smoking status
calculate_smoking_percentile_rank <- function(smoking_status, smoking_props) {
  cumulative_rank <- case_when(
    smoking_status == "Never smoker" ~ smoking_props$prop_never / 2,
    smoking_status == "Ex-smoker" ~ smoking_props$prop_never + smoking_props$prop_ex / 2,
    smoking_status == "Current smoker" ~ smoking_props$prop_never + smoking_props$prop_ex + smoking_props$prop_current / 2,
    TRUE ~ 0.5
  )
  return(cumulative_rank)
}

#' Generate future birth years for women
generate_future_birth_years <- function(age, ons_distributions, current_year) {
  # Use the actual fertility data structure
  fertility_data <- ons_distributions$fertility_parameters$fertility_data
  
  future_years <- (current_year + 1):2030
  future_birth_years <- c()
  
  for (year in future_years) {
    age_in_year <- age + (year - current_year)
    if (age_in_year >= 15 && age_in_year <= 49) {
      # Get fertility rate for this age
      age_fertility <- fertility_data %>% 
        dplyr::filter(age_numeric == age_in_year)
      
      if (nrow(age_fertility) > 0) {
        annual_birth_prob <- age_fertility$asfr[1]
        if (runif(1) < annual_birth_prob) {
          future_birth_years <- c(future_birth_years, year)
        }
      }
    }
  }
  return(future_birth_years)
}

#' Generate a single individual for the microsimulation
generate_individual <- function(ons_distributions, hse_distributions) {
  
  # Sample age from ONS age distribution
  age_combined <- ons_distributions$age_distributions$cumulative %>%
    group_by(age) %>%
    summarise(
      total_prop = sum(prop_age),
      .groups = 'drop'
    ) %>%
    arrange(age) %>%
    mutate(cumulative_prop = cumsum(total_prop))
  
  age <- sample_from_cumulative(1, age_combined, "age", "cumulative_prop")
  hse_age_group <- convert_age_to_hse_2021_group(age)
  longitudinal_age_group <- convert_age_to_hse_longitudinal_group(age)
  
  # Sample gender from ONS gender distribution
  gender_dist <- ons_distributions$gender_distributions$discrete
  gender <- sample(gender_dist$sex_label, 1, prob = gender_dist$prop_sex)
  
  # Sample BMI from HSE distributions
  bmi_params <- hse_distributions$distributions$bmi %>%
    dplyr::filter(as.character(age_group) == hse_age_group, sex_label == gender)
  
  if (nrow(bmi_params) == 0 || is.na(bmi_params$mean) || is.na(bmi_params$sd)) {
    stop(paste("No BMI distribution found for:", hse_age_group, gender))
  }
  
  bmi <- rnorm(1, bmi_params$mean, bmi_params$sd)
  if (!is.na(bmi_params$min_val)) bmi <- pmax(bmi, bmi_params$min_val)
  if (!is.na(bmi_params$max_val)) bmi <- pmin(bmi, bmi_params$max_val)
  
  bmi_percentile_rank <- calculate_percentile_rank(bmi, bmi_params$mean, bmi_params$sd)
  
  bmi_category <- case_when(
    bmi < 18.5 ~ "Underweight",
    bmi >= 18.5 & bmi < 25 ~ "Normal weight", 
    bmi >= 25 & bmi < 30 ~ "Overweight",
    bmi >= 30 ~ "Obese",
    TRUE ~ "Normal weight"
  )
  
  # Sample SBP from HSE distributions
  sbp_params <- hse_distributions$distributions$sbp %>%
    dplyr::filter(as.character(age_group) == hse_age_group, sex_label == gender)
  
  if (nrow(sbp_params) == 0 || is.na(sbp_params$mean) || is.na(sbp_params$sd)) {
    stop(paste("No SBP distribution found for:", hse_age_group, gender))
  }
  
  base_sbp <- rnorm(1, sbp_params$mean, sbp_params$sd)
  age_adjustment <- pmax(0, (age - 25) * 0.35)
  sbp <- base_sbp + age_adjustment
  
  if (!is.na(sbp_params$min_val)) sbp <- pmax(sbp, sbp_params$min_val)
  if (!is.na(sbp_params$max_val)) sbp <- pmin(sbp, sbp_params$max_val)
  
  sbp_percentile_rank <- calculate_percentile_rank(base_sbp, sbp_params$mean, sbp_params$sd)
  
  sbp_category <- case_when(
    sbp < 120 ~ "Normotensive",
    sbp >= 120 & sbp < 140 ~ "Prehypertensive",
    sbp >= 140 ~ "Hypertensive",
    TRUE ~ "Normotensive"
  )
  
  # Sample smoking status from HSE proportions
  smoking_props <- hse_distributions$proportions$smoking %>%
    dplyr::filter(as.character(age_group) == hse_age_group, sex_label == gender)
  
  if (nrow(smoking_props) == 0) {
    stop(paste("No smoking proportions found for:", hse_age_group, gender))
  }
  
  smoking_status <- sample(
    c("Never smoker", "Former smoker", "Current smoker"),
    1,
    prob = c(smoking_props$prop_never, smoking_props$prop_ex, smoking_props$prop_current)
  )
  
  smoking_percentile_rank <- calculate_smoking_percentile_rank(smoking_status, smoking_props)
  
  # Sample birth history for women over 15
  birth_history <- list(n_births = 0, birth_ages = numeric(0))
  if (gender == "Women" && age >= 15) {
    fertility_data <- ons_distributions$fertility_parameters$fertility_data
    
    if (!is.null(fertility_data) && nrow(fertility_data) > 0) {
      # Get age-specific fertility rate for this woman's age
      age_fertility <- fertility_data %>%
        dplyr::filter(age_numeric <= age) # Only consider completed fertility
      
      if (nrow(age_fertility) > 0) {
        # Simple approach: sample number of births based on cumulative fertility
        max_fertility <- max(age_fertility$asfr, na.rm = TRUE)
        # Convert fertility rate to probability of having had children
        fertility_prob <- min(age * max_fertility * 0.1, 0.8) # Simple scaling
        
        if (runif(1) < fertility_prob) {
          # Sample 1-3 children for women who have had children
          n_births <- sample(1:3, 1, prob = c(0.5, 0.3, 0.2))
          birth_ages <- sort(sample(18:min(age, 45), n_births, replace = TRUE))
          birth_history <- list(n_births = n_births, birth_ages = birth_ages)
        }
      }
    }
  }
  
  # Generate future birth years for women of reproductive age
  future_birth_years <- integer(0)
  if (gender == "Women" && age >= 15 && age <= 49) {
    future_birth_years <- generate_future_birth_years(age, ons_distributions, 2025)
  }
  
  # Sample disease status (prevalence-based)
  disease_status <- list(
    chd_status = FALSE,
    colorectal_cancer_status = FALSE,
    lung_cancer_status = FALSE,
    stroke_status = FALSE,
    copd_status = FALSE
  )
  
  diseases <- c("CHD", "colorectal_cancer", "COPD", "lung_cancer", "stroke")
  
  # Create individual structure with ALL microsimulation fields initialized
  individual <- list(
    age = age,
    sex_label = gender,
    current_age_group = longitudinal_age_group,
    
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
    
    # Disease status
    chd = FALSE,
    colorectal_cancer = FALSE, 
    lung_cancer = FALSE,
    stroke = FALSE,
    copd = FALSE,
    
    # Disease years
    chd_years = 0,
    colorectal_cancer_years = 0,
    lung_cancer_years = 0,
    stroke_years = 0,
    copd_years = 0,
    
    # Disease incidence probability columns (added by apply_disease_incidence)
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
    
    # Disease incidence year tracking
    chd_incidence_year = NA,
    stroke_incidence_year = NA,
    copd_incidence_year = NA,
    lung_cancer_incidence_year = NA,
    colorectal_cancer_incidence_year = NA,
    
    # Mortality probability columns (added by apply_mortality)
    CHD_mortality_prob = 0,
    stroke_mortality_prob = 0,
    COPD_mortality_prob = 0,
    lung_cancer_mortality_prob = 0,
    colorectal_cancer_mortality_prob = 0,
    other_cause_mortality_prob = 0,
    total_mortality_prob = 0,
    
    # Death tracking columns
    death_year = NA,
    cause_of_death = NA
  )
  
  # Override disease status with sampled values if they had pre-existing conditions
  for (disease_name in names(disease_status)) {
    clean_disease_name <- gsub("_status$", "", disease_name)
    if (clean_disease_name %in% names(individual)) {
      individual[[clean_disease_name]] <- disease_status[[disease_name]]
    }
  }
  
  return(individual)
}

#' Generate microsimulation population with all necessary fields pre-initialized
generate_population <- function(n_individuals, ons_distributions, hse_distributions, seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  
  cat("Generating", n_individuals, "individuals with full microsimulation structure...\n")
  
  individuals <- replicate(n_individuals, 
                           generate_individual(ons_distributions, hse_distributions), 
                           simplify = FALSE)
  
  # Convert to data frame with all required columns for microsimulation
  population_df <- data.frame(
    id = 1:n_individuals,
    age = sapply(individuals, function(x) x$age),
    sex_label = sapply(individuals, function(x) x$sex_label),
    current_age_group = sapply(individuals, function(x) x$current_age_group),
    
    # Risk factors
    bmi = sapply(individuals, function(x) x$bmi),
    bmi_category = sapply(individuals, function(x) x$bmi_category),
    bmi_percentile_rank = sapply(individuals, function(x) x$bmi_percentile_rank),
    
    sbp = sapply(individuals, function(x) x$sbp),
    sbp_category = sapply(individuals, function(x) x$sbp_category),
    sbp_percentile_rank = sapply(individuals, function(x) x$sbp_percentile_rank),
    
    smoking_status = sapply(individuals, function(x) x$smoking_status),
    smoking_percentile_rank = sapply(individuals, function(x) x$smoking_percentile_rank),
    
    # Birth history
    n_births = sapply(individuals, function(x) x$n_births),
    
    # Alive flag
    alive = sapply(individuals, function(x) x$alive),
    
    # Disease status
    chd = sapply(individuals, function(x) x$chd),
    colorectal_cancer = sapply(individuals, function(x) x$colorectal_cancer),
    lung_cancer = sapply(individuals, function(x) x$lung_cancer),
    stroke = sapply(individuals, function(x) x$stroke),
    copd = sapply(individuals, function(x) x$copd),
    
    # Disease years
    chd_years = sapply(individuals, function(x) x$chd_years),
    colorectal_cancer_years = sapply(individuals, function(x) x$colorectal_cancer_years),
    lung_cancer_years = sapply(individuals, function(x) x$lung_cancer_years),
    stroke_years = sapply(individuals, function(x) x$stroke_years),
    copd_years = sapply(individuals, function(x) x$copd_years),
    
    # Disease incidence probabilities and relative risks
    CHD_probability = sapply(individuals, function(x) x$CHD_probability),
    CHD_baseline_prob = sapply(individuals, function(x) x$CHD_baseline_prob),
    CHD_smoking_rr = sapply(individuals, function(x) x$CHD_smoking_rr),
    CHD_bp_rr = sapply(individuals, function(x) x$CHD_bp_rr),
    CHD_bmi_rr = sapply(individuals, function(x) x$CHD_bmi_rr),
    CHD_combined_rr = sapply(individuals, function(x) x$CHD_combined_rr),
    
    stroke_probability = sapply(individuals, function(x) x$stroke_probability),
    stroke_baseline_prob = sapply(individuals, function(x) x$stroke_baseline_prob),
    stroke_smoking_rr = sapply(individuals, function(x) x$stroke_smoking_rr),
    stroke_bp_rr = sapply(individuals, function(x) x$stroke_bp_rr),
    stroke_bmi_rr = sapply(individuals, function(x) x$stroke_bmi_rr),
    stroke_combined_rr = sapply(individuals, function(x) x$stroke_combined_rr),
    
    COPD_probability = sapply(individuals, function(x) x$COPD_probability),
    COPD_baseline_prob = sapply(individuals, function(x) x$COPD_baseline_prob),
    COPD_smoking_rr = sapply(individuals, function(x) x$COPD_smoking_rr),
    COPD_bp_rr = sapply(individuals, function(x) x$COPD_bp_rr),
    COPD_bmi_rr = sapply(individuals, function(x) x$COPD_bmi_rr),
    COPD_combined_rr = sapply(individuals, function(x) x$COPD_combined_rr),
    
    lung_cancer_probability = sapply(individuals, function(x) x$lung_cancer_probability),
    lung_cancer_baseline_prob = sapply(individuals, function(x) x$lung_cancer_baseline_prob),
    lung_cancer_smoking_rr = sapply(individuals, function(x) x$lung_cancer_smoking_rr),
    lung_cancer_bp_rr = sapply(individuals, function(x) x$lung_cancer_bp_rr),
    lung_cancer_bmi_rr = sapply(individuals, function(x) x$lung_cancer_bmi_rr),
    lung_cancer_combined_rr = sapply(individuals, function(x) x$lung_cancer_combined_rr),
    
    colorectal_cancer_probability = sapply(individuals, function(x) x$colorectal_cancer_probability),
    colorectal_cancer_baseline_prob = sapply(individuals, function(x) x$colorectal_cancer_baseline_prob),
    colorectal_cancer_smoking_rr = sapply(individuals, function(x) x$colorectal_cancer_smoking_rr),
    colorectal_cancer_bp_rr = sapply(individuals, function(x) x$colorectal_cancer_bp_rr),
    colorectal_cancer_bmi_rr = sapply(individuals, function(x) x$colorectal_cancer_bmi_rr),
    colorectal_cancer_combined_rr = sapply(individuals, function(x) x$colorectal_cancer_combined_rr),
    
    # Disease incidence year tracking
    chd_incidence_year = sapply(individuals, function(x) x$chd_incidence_year),
    stroke_incidence_year = sapply(individuals, function(x) x$stroke_incidence_year),
    copd_incidence_year = sapply(individuals, function(x) x$copd_incidence_year),
    lung_cancer_incidence_year = sapply(individuals, function(x) x$lung_cancer_incidence_year),
    colorectal_cancer_incidence_year = sapply(individuals, function(x) x$colorectal_cancer_incidence_year),
    
    # Mortality probabilities
    CHD_mortality_prob = sapply(individuals, function(x) x$CHD_mortality_prob),
    stroke_mortality_prob = sapply(individuals, function(x) x$stroke_mortality_prob),
    COPD_mortality_prob = sapply(individuals, function(x) x$COPD_mortality_prob),
    lung_cancer_mortality_prob = sapply(individuals, function(x) x$lung_cancer_mortality_prob),
    colorectal_cancer_mortality_prob = sapply(individuals, function(x) x$colorectal_cancer_mortality_prob),
    other_cause_mortality_prob = sapply(individuals, function(x) x$other_cause_mortality_prob),
    total_mortality_prob = sapply(individuals, function(x) x$total_mortality_prob),
    
    # Death tracking
    death_year = sapply(individuals, function(x) x$death_year),
    cause_of_death = sapply(individuals, function(x) x$cause_of_death),
    
    stringsAsFactors = FALSE
  )
  
  # Add list columns
  population_df$birth_ages <- lapply(individuals, function(x) x$birth_ages)
  population_df$birth_years <- lapply(individuals, function(x) x$birth_years[[1]])
  
  cat("Generated population with", ncol(population_df), "columns (all microsimulation fields pre-initialized)\n")
  
  return(population_df)
}

# Example Usage
# Generate microsimulation population
#
# initial_population <- generate_population(
#  n_individuals = 1000,
#  ons_distributions = ons_distributions,  
#  hse_distributions = hse_distributions,
#  seed = 9001
# )