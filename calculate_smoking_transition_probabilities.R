#' Calculate Smoking Transition Probabilities from Longitudinal Data
#' 
#' This function calculates realistic smoking transition probabilities using
#'longitudinal smoking survey data
#' 
#' @param longitudinal_file Path to longitudinal smoking CSV file
#' @return Data frame with transition probabilities by demographic group
#' 
#' @details
#' Uses observed year-over-year changes in smoking prevalence from survey data
#' to estimate individual-level transition rates.
#'

calculate_smoking_transitions_final <- function(longitudinal_file = "longitudinal_smoking.csv") {
  
  # Load longitudinal data
  cat("Loading longitudinal smoking data...\n")
  longitudinal_data <- read_csv(longitudinal_file, show_col_types = FALSE)
  
  cat("Data loaded:", nrow(longitudinal_data), "rows\n")
  cat("Years available:", paste(range(longitudinal_data$year), collapse = "-"), "\n")
  cat("Demographic groups:", length(unique(paste(longitudinal_data$sex, longitudinal_data$age_group))), "\n\n")
  
  # Convert to wide format
  smoking_wide <- longitudinal_data %>%
    pivot_wider(names_from = smoking_category, values_from = rate, names_prefix = "prob_") %>%
    rename(
      prob_Current_smoker = `prob_Current smoker`,
      prob_Former_smoker = `prob_Former smoker`, 
      prob_Never_smoker = `prob_Never smoker`
    ) %>%
    # Normalize to handle rounding errors
    mutate(
      total = prob_Current_smoker + prob_Former_smoker + prob_Never_smoker,
      prob_Current_smoker = prob_Current_smoker / total,
      prob_Former_smoker = prob_Former_smoker / total,
      prob_Never_smoker = prob_Never_smoker / total
    ) %>%
    dplyr::select(-total) %>%
    arrange(sex, age_group, year)
  
  # Process each demographic group
  demo_groups <- smoking_wide %>%
    distinct(sex, age_group)
  
  all_results <- tibble()
  
  cat("Processing demographic groups:\n")
  
  for (i in 1:nrow(demo_groups)) {
    sex_grp <- demo_groups$sex[i]
    age_grp <- demo_groups$age_group[i]
    
    cat("  ", sex_grp, age_grp, "... ")
    
    # Get data for this group
    group_data <- smoking_wide %>%
      dplyr::filter(sex == sex_grp, age_group == age_grp) %>%
      arrange(year)
    
    # Focus on recent years (2014+) for better relevance
    recent_data <- group_data %>%
      dplyr::filter(year >= 2014)
    
    if (nrow(recent_data) < 2) {
      cat("insufficient data\n")
      next
    }
    
    # Calculate year-over-year transitions
    transitions <- calculate_demographic_transitions(recent_data)
    
    if (nrow(transitions) > 0) {
      # Average transition rates
      avg_rates <- transitions %>%
        summarise(
          n_transitions = n(),
          min_year = min(from_year),
          max_year = max(to_year),
          initiation_rate = mean(initiation_rate, na.rm = TRUE),
          cessation_rate = mean(cessation_rate, na.rm = TRUE),
          relapse_rate = mean(relapse_rate, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Create transition matrix
      q1 <- avg_rates$initiation_rate  # never -> current
      q2 <- avg_rates$relapse_rate     # former -> current
      q3 <- avg_rates$cessation_rate   # current -> former
      
      transition_matrix <- matrix(c(
        1-q1,    0,   q1,     # Never -> {never, former, current}
        0,    1-q2,   q2,     # Former -> {never, former, current}
        0,       q3, 1-q3     # Current -> {never, former, current}
      ), nrow = 3, byrow = TRUE)
      
      # Store results
      result <- tibble(
        sex = sex_grp,
        age_group = age_grp,
        n_transitions = avg_rates$n_transitions,
        data_period = paste0(avg_rates$min_year, "-", avg_rates$max_year),
        
        # Primary transition rates
        initiation_rate = q1,
        cessation_rate = q3,
        relapse_rate = q2,
        
        # Annual probabilities (same as rates for annual transitions)
        annual_initiation_prob = q1,
        annual_cessation_prob = q3,
        annual_relapse_prob = q2,
        
        # Transition matrix elements
        trans_never_to_never = transition_matrix[1,1],
        trans_never_to_former = transition_matrix[1,2],
        trans_never_to_current = transition_matrix[1,3],
        trans_former_to_never = transition_matrix[2,1], 
        trans_former_to_former = transition_matrix[2,2],
        trans_former_to_current = transition_matrix[2,3],
        trans_current_to_never = transition_matrix[3,1],
        trans_current_to_former = transition_matrix[3,2],
        trans_current_to_current = transition_matrix[3,3]
      )
      
      all_results <- bind_rows(all_results, result)
      cat("✓ (", avg_rates$n_transitions, "transitions )\n")
      
    } else {
      cat("no valid transitions\n")
    }
  }
  
  cat("\nCalculation complete!\n")
  cat("Total demographic groups processed:", nrow(all_results), "\n\n")
  
  # Print summary
  if (nrow(all_results) > 0) {
    cat("Summary of Transition Rates:\n")
    cat("============================\n")
    
    summary_stats <- all_results %>%
      summarise(
        mean_initiation = mean(initiation_rate),
        mean_cessation = mean(cessation_rate),
        mean_relapse = mean(relapse_rate),
        median_initiation = median(initiation_rate),
        median_cessation = median(cessation_rate),
        median_relapse = median(relapse_rate),
        range_initiation = paste0(round(min(initiation_rate), 3), " - ", round(max(initiation_rate), 3)),
        range_cessation = paste0(round(min(cessation_rate), 3), " - ", round(max(cessation_rate), 3)),
        range_relapse = paste0(round(min(relapse_rate), 3), " - ", round(max(relapse_rate), 3))
      )
    
    cat("Mean rates: Initiation =", sprintf("%.1f%%", summary_stats$mean_initiation * 100), 
        "| Cessation =", sprintf("%.1f%%", summary_stats$mean_cessation * 100),
        "| Relapse =", sprintf("%.1f%%", summary_stats$mean_relapse * 100), "\n")
    cat("Ranges:     Initiation =", summary_stats$range_initiation,
        "| Cessation =", summary_stats$range_cessation, 
        "| Relapse =", summary_stats$range_relapse, "\n\n")
  }
  
  return(all_results)
}

#' Calculate Demographic-Specific Transitions
#'
#' Helper function to calculate transitions for a single demographic group
calculate_demographic_transitions <- function(group_data) {
  
  transitions <- tibble()
  
  for (j in 1:(nrow(group_data) - 1)) {
    current_year <- group_data[j, ]
    next_year <- group_data[j + 1, ]
    
    # Skip large time gaps (different surveys/cohorts)
    year_gap <- next_year$year - current_year$year
    if (year_gap > 2) next
    
    # Current and next distributions 
    pi_never_t <- current_year$prob_Never_smoker
    pi_former_t <- current_year$prob_Former_smoker
    pi_current_t <- current_year$prob_Current_smoker
    
    pi_never_t1 <- next_year$prob_Never_smoker
    pi_former_t1 <- next_year$prob_Former_smoker
    pi_current_t1 <- next_year$prob_Current_smoker
    
    # Calculate changes
    delta_never <- pi_never_t1 - pi_never_t
    delta_former <- pi_former_t1 - pi_former_t
    delta_current <- pi_current_t1 - pi_current_t
    
    # Estimate individual transition rates
    rates <- estimate_rates_from_changes(
      pi_never_t, pi_former_t, pi_current_t,
      delta_never, delta_former, delta_current
    )
    
    if (!is.null(rates)) {
      transition_row <- tibble(
        from_year = current_year$year,
        to_year = next_year$year,
        year_gap = year_gap,
        initiation_rate = rates$initiation,
        cessation_rate = rates$cessation,
        relapse_rate = rates$relapse
      )
      
      transitions <- bind_rows(transitions, transition_row)
    }
  }
  
  return(transitions)
}

#' Estimate Rates from Observed Changes
#'
#' Estimates transition rates that would produce the observed changes
estimate_rates_from_changes <- function(pi_never_t, pi_former_t, pi_current_t,
                                        delta_never, delta_former, delta_current) {
  
  # Use heuristic approach based on observed changes and demographic patterns
  
  # Cessation rate: primary driver of former smoker increase
  cessation_rate <- 0.06  # baseline 6%
  if (pi_current_t > 0.01 && delta_former > 0) {
    cessation_rate <- min(0.25, max(0.01, delta_former / pi_current_t))
  }
  
  # Initiation rate: driver of never smoker decrease  
  initiation_rate <- 0.02  # baseline 2%
  if (pi_never_t > 0.01) {
    if (delta_never < 0) {
      # Never smokers decreased - estimate initiation
      initiation_rate <- min(0.15, max(0.005, -delta_never / pi_never_t))
    } else {
      # Never smokers increased - lower initiation
      initiation_rate <- 0.01
    }
  }
  
  # Relapse rate: contributes to former smoker decrease and current increase
  relapse_rate <- 0.03  # baseline 3%
  if (pi_former_t > 0.01 && delta_former < 0) {
    # Former smokers decreased - might indicate relapse
    relapse_rate <- min(0.1, max(0.01, -delta_former * 0.5 / pi_former_t))
  }
  
  # Ensure reasonable bounds
  initiation_rate <- max(0.005, min(0.15, initiation_rate))
  cessation_rate <- max(0.01, min(0.25, cessation_rate))
  relapse_rate <- max(0.01, min(0.1, relapse_rate))
  
  return(list(
    initiation = initiation_rate,
    cessation = cessation_rate,
    relapse = relapse_rate
  ))
}

# Usage and example
if (FALSE) {
  # Run the calculation 
  smoking_transitions <- calculate_smoking_transitions_final("longitudinal_smoking.csv")
  
  # View results
  print(smoking_transitions)
  
  # Save results
  write_csv(smoking_transitions, "smoking_transition_probabilities_longitudinal.csv")
  
  # Example: Get transition matrix for specific group
  example_group <- smoking_transitions %>%
    dplyr::filter(sex == "Men", age_group == "35-44 years")
  
  if (nrow(example_group) > 0) {
    cat("Transition matrix for Men 35-44 years:\n")
    trans_matrix <- matrix(c(
      example_group$trans_never_to_never, example_group$trans_never_to_former, example_group$trans_never_to_current,
      example_group$trans_former_to_never, example_group$trans_former_to_former, example_group$trans_former_to_current, 
      example_group$trans_current_to_never, example_group$trans_current_to_former, example_group$trans_current_to_current
    ), nrow = 3, byrow = TRUE)
    
    rownames(trans_matrix) <- c("Never", "Former", "Current")
    colnames(trans_matrix) <- c("Never", "Former", "Current")
    print(round(trans_matrix, 4))
  }
}