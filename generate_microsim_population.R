library(dplyr)
library(data.table)
library(progress)

# Pre-computed lookup tables for age group conversion
.age_hse_2021_lookup <- data.frame(
  min_age = c(0, 2, 5, 8, 11, 13, 16, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90),
  max_age = c(1, 4, 7, 10, 12, 15, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
  group = c("0 - 1 years", "2 - 4 years", "5 - 7 years", "8 - 10 years", "11 - 12 years",
            "13 - 15 years", "16 - 19 years", "20 - 24 years", "25 - 29 years", "30 - 34 years",
            "35 - 39 years", "40 - 44 years", "45 - 49 years", "50 - 54 years", "55 - 59 years",
            "60 - 64 years", "65 - 69 years", "70 - 74 years", "75 - 79 years", "80 - 84 years",
            "85 - 89 years", "90+ years")
)

.age_longitudinal_lookup <- data.frame(
  min_age = c(0, 16, 25, 35, 45, 55, 65, 75),
  max_age = c(15, 24, 34, 44, 54, 64, 74, Inf),
  group = c("0 - 16 years", "16 - 24 years", "25 - 34 years", "35 - 44 years",
            "45 - 54 years", "55 - 64 years", "65 - 74 years", "75+ years")
)

# BMI category lookup for vectorized operations
.bmi_category_breaks <- c(-Inf, 18.5, 25, 30, Inf)
.bmi_category_labels <- c("Underweight", "Normal weight", "Overweight", "Obese")

# SBP category lookup
.sbp_category_breaks <- c(-Inf, 120, 140, Inf)
.sbp_category_labels <- c("Normotensive", "Prehypertensive", "Hypertensive")

#' Sample from cumulative distributions using inverse transform sampling
sample_from_cumulative <- function(n, cumulative_dist, value_col, prob_col = "cumulative_prop") {
  u <- runif(n)
  # Use findInterval for vectorized lookup
  indices <- findInterval(u, cumulative_dist[[prob_col]], left.open = FALSE) + 1L
  indices[indices > nrow(cumulative_dist)] <- nrow(cumulative_dist)
  return(cumulative_dist[[value_col]][indices])
}

#' Convert continuous age to HSE 2021 age group
convert_age_to_hse_2021_group <- function(age) {
  if (length(age) == 0) return(character(0))
  
  # Vectorized lookup using findInterval
  indices <- findInterval(age, .age_hse_2021_lookup$min_age, rightmost.closed = TRUE)
  indices[indices == 0] <- 1L  # Handle edge case for age 0
  
  return(.age_hse_2021_lookup$group[indices])
}

#' Convert continuous age to HSE longitudinal age group
convert_age_to_hse_longitudinal_group <- function(age) {
  if (length(age) == 0) return(character(0))
  
  indices <- findInterval(age, .age_longitudinal_lookup$min_age, rightmost.closed = TRUE)
  indices[indices == 0] <- 1L
  
  return(.age_longitudinal_lookup$group[indices])
}

#' Calculate percentile rank for a given value in a distribution
calculate_percentile_rank <- function(value, mean, sd) {
  pnorm(value, mean, sd)
}

#' Calculate percentile rank for smoking status
calculate_smoking_percentile_rank <- function(smoking_status, smoking_props) {
  status_levels <- c("Never smoker", "Ex-smoker", "Current smoker")
  status_indices <- match(smoking_status, status_levels)
  
  # Pre-calculate cumulative ranks for each status
  cumulative_ranks <- c(
    smoking_props$prop_never / 2,
    smoking_props$prop_never + smoking_props$prop_ex / 2,
    smoking_props$prop_never + smoking_props$prop_ex + smoking_props$prop_current / 2
  )
  
  # Use vectorized indexing
  result <- cumulative_ranks[status_indices]
  result[is.na(result)] <- 0.5  # Default for unknown status
  
  return(result)
}

#' Generate future birth years for women
generate_future_birth_years <- function(age, ons_distributions, current_year, sim_end_year = 2040) {
  fertility_data <- ons_distributions$fertility_parameters$fertility_data
  
  if (is.null(fertility_data) || nrow(fertility_data) == 0) {
    return(integer(0))
  }
  
  future_years <- (current_year + 1L):sim_end_year
  future_birth_years <- integer(0)
  
  # Pre-filter fertility data for efficiency
  fertility_lookup <- setNames(fertility_data$asfr, fertility_data$age_numeric)
  
  for (year in future_years) {
    age_in_year <- age + (year - current_year)
    if (age_in_year >= 15L && age_in_year <= 49L) {
      annual_birth_prob <- fertility_lookup[as.character(age_in_year)]
      if (!is.na(annual_birth_prob) && runif(1) < annual_birth_prob) {
        future_birth_years <- c(future_birth_years, year)
      }
    }
  }
  
  return(future_birth_years)
}

#' Generate a single individual for the microsimulation
generate_individual <- function(ons_distributions, hse_distributions, sim_end_year = 2040) {
  pop <- generate_population(1, ons_distributions, hse_distributions, sim_end_year = sim_end_year)
  
  individual <- list(
    age = pop$age[1],
    sex_label = pop$sex_label[1],
    current_age_group = pop$current_age_group[1],
    bmi = pop$bmi[1],
    bmi_category = pop$bmi_category[1],
    bmi_percentile_rank = pop$bmi_percentile_rank[1],
    sbp = pop$sbp[1],
    sbp_category = pop$sbp_category[1],
    sbp_percentile_rank = pop$sbp_percentile_rank[1],
    smoking_status = pop$smoking_status[1],
    smoking_percentile_rank = pop$smoking_percentile_rank[1],
    n_births = pop$n_births[1],
    birth_ages = pop$birth_ages[[1]],
    birth_years = pop$birth_years[[1]],
    alive = pop$alive[1],
    chd = pop$chd[1],
    colorectal_cancer = pop$colorectal_cancer[1],
    lung_cancer = pop$lung_cancer[1],
    stroke = pop$stroke[1],
    copd = pop$copd[1],
    chd_years = pop$chd_years[1],
    colorectal_cancer_years = pop$colorectal_cancer_years[1],
    lung_cancer_years = pop$lung_cancer_years[1],
    stroke_years = pop$stroke_years[1],
    copd_years = pop$copd_years[1],
    CHD_probability = pop$CHD_probability[1],
    CHD_baseline_prob = pop$CHD_baseline_prob[1],
    CHD_smoking_rr = pop$CHD_smoking_rr[1],
    CHD_bp_rr = pop$CHD_bp_rr[1],
    CHD_bmi_rr = pop$CHD_bmi_rr[1],
    CHD_combined_rr = pop$CHD_combined_rr[1],
    stroke_probability = pop$stroke_probability[1],
    stroke_baseline_prob = pop$stroke_baseline_prob[1],
    stroke_smoking_rr = pop$stroke_smoking_rr[1],
    stroke_bp_rr = pop$stroke_bp_rr[1],
    stroke_bmi_rr = pop$stroke_bmi_rr[1],
    stroke_combined_rr = pop$stroke_combined_rr[1],
    COPD_probability = pop$COPD_probability[1],
    COPD_baseline_prob = pop$COPD_baseline_prob[1],
    COPD_smoking_rr = pop$COPD_smoking_rr[1],
    COPD_bp_rr = pop$COPD_bp_rr[1],
    COPD_bmi_rr = pop$COPD_bmi_rr[1],
    COPD_combined_rr = pop$COPD_combined_rr[1],
    lung_cancer_probability = pop$lung_cancer_probability[1],
    lung_cancer_baseline_prob = pop$lung_cancer_baseline_prob[1],
    lung_cancer_smoking_rr = pop$lung_cancer_smoking_rr[1],
    lung_cancer_bp_rr = pop$lung_cancer_bp_rr[1],
    lung_cancer_bmi_rr = pop$lung_cancer_bmi_rr[1],
    lung_cancer_combined_rr = pop$lung_cancer_combined_rr[1],
    colorectal_cancer_probability = pop$colorectal_cancer_probability[1],
    colorectal_cancer_baseline_prob = pop$colorectal_cancer_baseline_prob[1],
    colorectal_cancer_smoking_rr = pop$colorectal_cancer_smoking_rr[1],
    colorectal_cancer_bp_rr = pop$colorectal_cancer_bp_rr[1],
    colorectal_cancer_bmi_rr = pop$colorectal_cancer_bmi_rr[1],
    colorectal_cancer_combined_rr = pop$colorectal_cancer_combined_rr[1],
    chd_incidence_year = pop$chd_incidence_year[1],
    stroke_incidence_year = pop$stroke_incidence_year[1],
    copd_incidence_year = pop$copd_incidence_year[1],
    lung_cancer_incidence_year = pop$lung_cancer_incidence_year[1],
    colorectal_cancer_incidence_year = pop$colorectal_cancer_incidence_year[1],
    CHD_mortality_prob = pop$CHD_mortality_prob[1],
    stroke_mortality_prob = pop$stroke_mortality_prob[1],
    COPD_mortality_prob = pop$COPD_mortality_prob[1],
    lung_cancer_mortality_prob = pop$lung_cancer_mortality_prob[1],
    colorectal_cancer_mortality_prob = pop$colorectal_cancer_mortality_prob[1],
    other_cause_mortality_prob = pop$other_cause_mortality_prob[1],
    total_mortality_prob = pop$total_mortality_prob[1],
    death_year = pop$death_year[1],
    cause_of_death = pop$cause_of_death[1]
  )
  
  return(individual)
}

#' Generate microsimulation population
generate_population <- function(n_individuals, ons_distributions, hse_distributions, seed = NULL, sim_end_year = 2040) {
  
  if (!is.null(seed)) set.seed(seed)
  
  # Initialize progress bar
  pb <- progress_bar$new(
    format = "Generating population [:bar] :percent :current/:total | :elapsed",
    total = 100,
    clear = FALSE,
    width = 80
  )
  
  cat("Generating", n_individuals, "individuals...\n")
  
  # Initialize data frame
  pb$tick(0, tokens = list(what = "Initializing"))
  
  # Pre-allocate output data frame
  population_df <- data.frame(
    id = seq_len(n_individuals),
    age = numeric(n_individuals),
    sex_label = character(n_individuals),
    current_age_group = character(n_individuals),
    
    # Risk factors
    bmi = numeric(n_individuals),
    bmi_category = character(n_individuals),
    bmi_percentile_rank = numeric(n_individuals),
    
    sbp = numeric(n_individuals),
    sbp_category = character(n_individuals),
    sbp_percentile_rank = numeric(n_individuals),
    
    smoking_status = character(n_individuals),
    smoking_percentile_rank = numeric(n_individuals),
    
    # Birth history
    n_births = integer(n_individuals),
    
    # Alive flag
    alive = rep(TRUE, n_individuals),
    
    # Disease status
    chd = rep(FALSE, n_individuals),
    colorectal_cancer = rep(FALSE, n_individuals),
    lung_cancer = rep(FALSE, n_individuals),
    stroke = rep(FALSE, n_individuals),
    copd = rep(FALSE, n_individuals),
    
    # Disease years
    chd_years = rep(0L, n_individuals),
    colorectal_cancer_years = rep(0L, n_individuals),
    lung_cancer_years = rep(0L, n_individuals),
    stroke_years = rep(0L, n_individuals),
    copd_years = rep(0L, n_individuals),
    
    # Disease incidence probabilities
    CHD_probability = rep(0.0, n_individuals),
    CHD_baseline_prob = rep(0.0, n_individuals),
    CHD_smoking_rr = rep(1.0, n_individuals),
    CHD_bp_rr = rep(1.0, n_individuals),
    CHD_bmi_rr = rep(1.0, n_individuals),
    CHD_combined_rr = rep(1.0, n_individuals),
    
    stroke_probability = rep(0.0, n_individuals),
    stroke_baseline_prob = rep(0.0, n_individuals),
    stroke_smoking_rr = rep(1.0, n_individuals),
    stroke_bp_rr = rep(1.0, n_individuals),
    stroke_bmi_rr = rep(1.0, n_individuals),
    stroke_combined_rr = rep(1.0, n_individuals),
    
    COPD_probability = rep(0.0, n_individuals),
    COPD_baseline_prob = rep(0.0, n_individuals),
    COPD_smoking_rr = rep(1.0, n_individuals),
    COPD_bp_rr = rep(1.0, n_individuals),
    COPD_bmi_rr = rep(1.0, n_individuals),
    COPD_combined_rr = rep(1.0, n_individuals),
    
    lung_cancer_probability = rep(0.0, n_individuals),
    lung_cancer_baseline_prob = rep(0.0, n_individuals),
    lung_cancer_smoking_rr = rep(1.0, n_individuals),
    lung_cancer_bp_rr = rep(1.0, n_individuals),
    lung_cancer_bmi_rr = rep(1.0, n_individuals),
    lung_cancer_combined_rr = rep(1.0, n_individuals),
    
    colorectal_cancer_probability = rep(0.0, n_individuals),
    colorectal_cancer_baseline_prob = rep(0.0, n_individuals),
    colorectal_cancer_smoking_rr = rep(1.0, n_individuals),
    colorectal_cancer_bp_rr = rep(1.0, n_individuals),
    colorectal_cancer_bmi_rr = rep(1.0, n_individuals),
    colorectal_cancer_combined_rr = rep(1.0, n_individuals),
    
    # Disease incidence year tracking
    chd_incidence_year = rep(NA_integer_, n_individuals),
    stroke_incidence_year = rep(NA_integer_, n_individuals),
    copd_incidence_year = rep(NA_integer_, n_individuals),
    lung_cancer_incidence_year = rep(NA_integer_, n_individuals),
    colorectal_cancer_incidence_year = rep(NA_integer_, n_individuals),
    
    # Mortality probabilities
    CHD_mortality_prob = rep(0.0, n_individuals),
    stroke_mortality_prob = rep(0.0, n_individuals),
    COPD_mortality_prob = rep(0.0, n_individuals),
    lung_cancer_mortality_prob = rep(0.0, n_individuals),
    colorectal_cancer_mortality_prob = rep(0.0, n_individuals),
    other_cause_mortality_prob = rep(0.0, n_individuals),
    total_mortality_prob = rep(0.0, n_individuals),
    
    # Death tracking
    death_year = rep(NA_integer_, n_individuals),
    cause_of_death = rep(NA_character_, n_individuals),
    
    nhs_last_check_year = rep(NA_integer_, n_individuals),
    birth_year = rep(NA_integer_, n_individuals),
    maternal_age = rep(NA_integer_, n_individuals),
    
    stringsAsFactors = FALSE
  )
  
  pb$tick(5, tokens = list(what = "Data frame initialized"))
  
  # Sample ages and genders
  age_combined <- ons_distributions$age_distributions$cumulative %>%
    group_by(age) %>%
    summarise(total_prop = sum(prop_age), .groups = 'drop') %>%
    arrange(age) %>%
    mutate(cumulative_prop = cumsum(total_prop))
  
  population_df$age <- sample_from_cumulative(n_individuals, age_combined, "age", "cumulative_prop")
  
  # Sample genders
  gender_dist <- ons_distributions$gender_distributions$discrete
  population_df$sex_label <- sample(gender_dist$sex_label, n_individuals, 
                                    prob = gender_dist$prop_sex, replace = TRUE)
  
  pb$tick(10, tokens = list(what = "Ages and genders sampled"))
  
  # Age group conversion
  hse_age_groups <- convert_age_to_hse_2021_group(population_df$age)
  population_df$current_age_group <- convert_age_to_hse_longitudinal_group(population_df$age)
  
  # Create lookup key for parameter matching
  lookup_key <- paste(hse_age_groups, population_df$sex_label, sep = "_")
  
  pb$tick(15, tokens = list(what = "Age groups processed"))
  
  # BMI generation
  bmi_params <- hse_distributions$distributions$bmi %>%
    mutate(lookup_key = paste(as.character(age_group), sex_label, sep = "_")) %>%
    filter(!is.na(mean), !is.na(sd))
  
  bmi_lookup <- setNames(list(
    mean = setNames(bmi_params$mean, bmi_params$lookup_key),
    sd = setNames(bmi_params$sd, bmi_params$lookup_key),
    min_val = setNames(bmi_params$min_val, bmi_params$lookup_key),
    max_val = setNames(bmi_params$max_val, bmi_params$lookup_key)
  ), c("mean", "sd", "min_val", "max_val"))
  
  # BMI sampling
  bmi_means <- bmi_lookup$mean[lookup_key]
  bmi_sds <- bmi_lookup$sd[lookup_key]
  valid_bmi <- !is.na(bmi_means) & !is.na(bmi_sds)
  
  population_df$bmi[valid_bmi] <- rnorm(sum(valid_bmi), bmi_means[valid_bmi], bmi_sds[valid_bmi])
  
  # Apply constraints
  min_vals <- bmi_lookup$min_val[lookup_key]
  max_vals <- bmi_lookup$max_val[lookup_key]
  
  population_df$bmi <- pmax(population_df$bmi, min_vals, na.rm = TRUE)
  population_df$bmi <- pmin(population_df$bmi, max_vals, na.rm = TRUE)
  
  # Handle missing BMI cases
  population_df$bmi[!valid_bmi] <- 25.0  # Default healthy BMI
  
  # BMI percentile rank and category calculation
  population_df$bmi_percentile_rank[valid_bmi] <- calculate_percentile_rank(
    population_df$bmi[valid_bmi], bmi_means[valid_bmi], bmi_sds[valid_bmi])
  population_df$bmi_percentile_rank[!valid_bmi] <- 0.5
  
  # BMI categorization using cut()
  population_df$bmi_category <- cut(population_df$bmi, 
                                    breaks = .bmi_category_breaks,
                                    labels = .bmi_category_labels,
                                    right = FALSE, include.lowest = TRUE)
  
  pb$tick(15, tokens = list(what = "BMI generated"))
  
  # SBP generation
  sbp_params <- hse_distributions$distributions$sbp %>%
    mutate(lookup_key = paste(as.character(age_group), sex_label, sep = "_")) %>%
    filter(!is.na(mean), !is.na(sd))
  
  sbp_lookup <- setNames(list(
    mean = setNames(sbp_params$mean, sbp_params$lookup_key),
    sd = setNames(sbp_params$sd, sbp_params$lookup_key),
    min_val = setNames(sbp_params$min_val, sbp_params$lookup_key),
    max_val = setNames(sbp_params$max_val, sbp_params$lookup_key)
  ), c("mean", "sd", "min_val", "max_val"))
  
  # SBP sampling
  sbp_means <- sbp_lookup$mean[lookup_key]
  sbp_sds <- sbp_lookup$sd[lookup_key]
  valid_sbp <- !is.na(sbp_means) & !is.na(sbp_sds)
  
  base_sbp <- numeric(n_individuals)
  base_sbp[valid_sbp] <- rnorm(sum(valid_sbp), sbp_means[valid_sbp], sbp_sds[valid_sbp])
  
  # age adjustment
  age_adjustment <- pmax(0, (population_df$age - 25) * 0.35)
  population_df$sbp <- base_sbp + age_adjustment
  
  # Apply constraints
  sbp_min_vals <- sbp_lookup$min_val[lookup_key]
  sbp_max_vals <- sbp_lookup$max_val[lookup_key]
  
  population_df$sbp <- pmax(population_df$sbp, sbp_min_vals, na.rm = TRUE)
  population_df$sbp <- pmin(population_df$sbp, sbp_max_vals, na.rm = TRUE)
  
  # Handle missing SBP cases
  population_df$sbp[!valid_sbp] <- 120.0  # Default normal SBP
  base_sbp[!valid_sbp] <- 120.0
  
  # SBP percentile rank and category
  population_df$sbp_percentile_rank[valid_sbp] <- calculate_percentile_rank(
    base_sbp[valid_sbp], sbp_means[valid_sbp], sbp_sds[valid_sbp])
  population_df$sbp_percentile_rank[!valid_sbp] <- 0.5
  
  population_df$sbp_category <- cut(population_df$sbp,
                                    breaks = .sbp_category_breaks,
                                    labels = .sbp_category_labels,
                                    right = FALSE, include.lowest = TRUE)
  
  pb$tick(15, tokens = list(what = "Blood pressure generated"))
  
  # Sample smoking status
  smoking_params <- hse_distributions$proportions$smoking %>%
    mutate(lookup_key = paste(as.character(age_group), sex_label, sep = "_"))
  
  smoking_lookup <- setNames(list(
    prop_never = setNames(smoking_params$prop_never, smoking_params$lookup_key),
    prop_ex = setNames(smoking_params$prop_ex, smoking_params$lookup_key),
    prop_current = setNames(smoking_params$prop_current, smoking_params$lookup_key)
  ), c("prop_never", "prop_ex", "prop_current"))
  
  # Smoking status sampling
  never_props <- smoking_lookup$prop_never[lookup_key]
  ex_props <- smoking_lookup$prop_ex[lookup_key]
  current_props <- smoking_lookup$prop_current[lookup_key]
  
  # Handle missing data
  valid_smoking <- !is.na(never_props) & !is.na(ex_props) & !is.na(current_props)
  never_props[!valid_smoking] <- 0.5
  ex_props[!valid_smoking] <- 0.3
  current_props[!valid_smoking] <- 0.2
  
  # Multinomial sampling using cumulative probabilities
  cumulative_never <- never_props
  cumulative_ex <- never_props + ex_props
  
  u_smoking <- runif(n_individuals)
  population_df$smoking_status <- ifelse(u_smoking <= cumulative_never, "Never smoker",
                                         ifelse(u_smoking <= cumulative_ex, "Former smoker", "Current smoker"))
  
  # Smoking percentile rank calculation
  status_ranks <- data.frame(
    never = never_props / 2,
    ex = never_props + ex_props / 2,
    current = never_props + ex_props + current_props / 2
  )
  
  population_df$smoking_percentile_rank <- ifelse(
    population_df$smoking_status == "Never smoker", status_ranks$never,
    ifelse(population_df$smoking_status == "Former smoker", status_ranks$ex, status_ranks$current)
  )
  
  pb$tick(15, tokens = list(what = "Smoking status generated"))
  
  # Birth history processing
  # Only process women of reproductive age
  women_indices <- which(population_df$sex_label == "Women" & population_df$age >= 15)
  
  # Initialize list columns
  population_df$birth_ages <- vector("list", n_individuals)
  population_df$birth_years <- vector("list", n_individuals)
  
  if (length(women_indices) > 0 && !is.null(ons_distributions$fertility_parameters$fertility_data)) {
    fertility_data <- ons_distributions$fertility_parameters$fertility_data
    
    if (nrow(fertility_data) > 0) {
      # Process birth history for eligible women
      for (i in women_indices) {
        age <- population_df$age[i]
        
        max_fertility <- max(fertility_data$asfr, na.rm = TRUE)
        fertility_prob <- min(age * max_fertility * 0.1, 0.8)
        
        if (runif(1) < fertility_prob) {
          n_births <- sample(1:3, 1, prob = c(0.5, 0.3, 0.2))
          birth_ages <- sort(sample(18:min(age, 45), n_births, replace = TRUE))
          population_df$n_births[i] <- n_births
          population_df$birth_ages[[i]] <- birth_ages
        } else {
          population_df$birth_ages[[i]] <- numeric(0)
        }
        
        # Generate future births for women of reproductive age
        if (age <= 49) {
          future_births <- generate_future_birth_years(age, ons_distributions, 2024, sim_end_year)
          population_df$birth_years[[i]] <- future_births
        } else {
          population_df$birth_years[[i]] <- integer(0)
        }
      }
    }
  }
  
  # Initialize empty birth histories for men and non-eligible women
  non_eligible <- setdiff(1:n_individuals, women_indices)
  population_df$birth_ages[non_eligible] <- lapply(non_eligible, function(x) numeric(0))
  population_df$birth_years[non_eligible] <- lapply(non_eligible, function(x) integer(0))
  
  pb$tick(20, tokens = list(what = "Birth histories processed"))
  
  # Complete progress bar
  pb$tick(5, tokens = list(what = "Complete"))
  
  cat("Generated population with", ncol(population_df), "columns\n")
  
  return(population_df)
}

# Example Usage
# Generate microsimulation population
#
# initial_population <- generate_population(
#   n_individuals = 10000,
#   ons_distributions = ons_distributions,  
#   hse_distributions = hse_distributions,
#   seed = 1234
# )