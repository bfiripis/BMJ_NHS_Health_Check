#' Run Multiple Scenario Comparisons for Confidence Intervals
#'
#' This function runs the scenario comparison multiple times with different random seeds
#' to generate confidence intervals around the key outcomes including ICERs.
#' Uses pre-generated initial population for consistency across replications.
#'
#' @param initial_population Pre-generated population dataframe
#' @param n_replications Number of simulation replications to run (default: 100)
#' @param base_seed Base random seed (each replication gets base_seed + i)
#' @param verbose Whether to print progress and results (default: TRUE)
#' @param scenarios Character vector: scenarios to run (default: c("baseline", "intervention", "equalized_attendance"))
#' @param start_year Starting year for simulation (default: 2025)
#' @param end_year Ending year for simulation (default: 2040)
#' @param baseline_params List of NHS Health Check parameters for baseline scenario
#' @param intervention_params List of NHS Health Check parameters for intervention scenario
#' @param equalized_attendance_params List of NHS Health Check parameters for equalized attendance scenario
#' @param longitudinal_hse_distributions Longitudinal HSE distributions
#' @param incidence_probabilities Disease incidence probability tables
#' @param smoking_relative_risks Smoking relative risks
#' @param blood_pressure_relative_risks Blood pressure relative risks
#' @param bmi_relative_risks BMI relative risks
#' @param mortality_probabilities Mortality probability tables
#' @param age_sex_utilities Age/sex utility values
#' @param disease_utilities Disease utility decrements
#' @param costs Disease cost estimates
#' @return List containing replicated results and summary statistics with confidence intervals
#'
#' @examples
#' \dontrun{
#' # Generate initial population first
#' initial_population <- generate_population(
#'   n_individuals = 1000,
#'   ons_distributions = ons_distributions,
#'   hse_distributions = hse_distributions,
#'   seed = 1234
#' )
#'
#' # Run 100 replications of scenario comparison
#' monte_carlo_results <- run_monte_carlo(
#'   initial_population = initial_population,
#'   n_replications = 100,
#'   start_year = 2025,
#'   end_year = 2040,
#'   baseline_params = baseline_params,
#'   intervention_params = intervention_params,
#'   equalized_attendance_params = equalized_attendance_params,
#'   # ... other standard parameters
#'   base_seed = 1234,
#'   verbose = TRUE
#' )
#'
#' # Get confidence intervals
#' ci_results <- calculate_scenario_confidence_intervals(monte_carlo_results)
#' print_scenario_confidence_intervals(ci_results)
#' }
#'
#' @export
run_monte_carlo <- function(initial_population,
                            n_replications = 100,
                            base_seed = 1234,
                            verbose = FALSE,
                            scenarios = c("baseline", "intervention", "equalized_attendance"),
                            start_year = 2025,
                            end_year = 2040,
                            baseline_params = NULL,
                            intervention_params = NULL,
                            equalized_attendance_params = NULL,
                            ons_distributions,
                            hse_distributions,
                            longitudinal_hse_distributions,
                            incidence_probabilities,
                            smoking_relative_risks,
                            blood_pressure_relative_risks,
                            bmi_relative_risks,
                            mortality_probabilities,
                            age_sex_utilities,
                            disease_utilities,
                            costs) {
  if (verbose) {
    cat("=== MONTE CARLO SCENARIO COMPARISON ===\n")
    cat(sprintf("Running %d replications consecutively for confidence intervals...\n", n_replications))
    cat(sprintf("Using pre-generated population: %d individuals\n", nrow(initial_population)))
    cat(sprintf("Scenarios: %s\n", paste(scenarios, collapse = ", ")))
    cat("===========================================\n\n")
  }
  
  # Get number of individuals from population
  n_individuals <- nrow(initial_population)
  
  # Generate seeds for each replication
  replication_seeds <- base_seed + (1:n_replications)
  
  # Initialize results storage
  results_list <- list()
  n_successful <- 0
  n_failed <- 0
  
  start_time <- Sys.time()
  
  # Run replications consecutively
  for (i in 1:n_replications) {
    # Progress reporting - only when verbose = TRUE and at 10% intervals
    if (verbose) {
      if (i %% max(1, floor(n_replications / 10)) == 0 || i == 1) {
        cat(sprintf(
          "Starting replication %d/%d (%.1f%%)...\n",
          i, n_replications, 100 * i / n_replications
        ))
      }
    } else {
      # Simple progress for verbose = FALSE - just show every 20%
      if (i %% max(1, floor(n_replications / 5)) == 0 || i == 1) {
        cat(sprintf("Replication %d/%d\n", i, n_replications))
      }
    }
    
    # Run the scenario comparison with this seed and the pre-generated population
    result <- tryCatch(
      {
        run_scenario_comparison(
          initial_population = initial_population,
          scenarios = scenarios,
          start_year = start_year,
          end_year = end_year,
          baseline_params = baseline_params,
          intervention_params = intervention_params,
          equalized_attendance_params = equalized_attendance_params,
          longitudinal_hse_distributions = longitudinal_hse_distributions,
          incidence_probabilities = incidence_probabilities,
          smoking_relative_risks = smoking_relative_risks,
          blood_pressure_relative_risks = blood_pressure_relative_risks,
          bmi_relative_risks = bmi_relative_risks,
          mortality_probabilities = mortality_probabilities,
          age_sex_utilities = age_sex_utilities,
          disease_utilities = disease_utilities,
          costs = costs,
          seed = replication_seeds[i],
          verbose = FALSE
        )
      },
      error = function(e) {
        if (verbose) {
          cat(sprintf("Replication %d failed with error: %s\n", i, e$message))
          cat(sprintf("Full error: %s\n", paste(deparse(e$call), collapse = " ")))
        } else {
          cat(sprintf("Replication %d failed\n", i))
        }
        return(NULL)
      }
    )
    
    if (is.null(result)) {
      n_failed <- n_failed + 1
      results_list[[i]] <- list(
        replication = i,
        seed = replication_seeds[i],
        success = FALSE,
        error = TRUE
      )
      next
    }
    
    n_successful <- n_successful + 1
    
    # Extract key outcomes from the scenario results
    comparison_summary <- result$comparison_summary
    icer_analysis <- result$icer_analysis
    
    # Initialize outcome list
    outcomes <- list(
      replication = i,
      seed = replication_seeds[i],
      success = TRUE,
      error = FALSE
    )
    
    # Extract baseline scenario outcomes if available
    if ("baseline" %in% comparison_summary$scenario) {
      baseline_row <- comparison_summary[comparison_summary$scenario == "baseline", ]
      
      outcomes$baseline_total_qalys <- baseline_row$total_qalys
      outcomes$baseline_qalys_per_person <- baseline_row$qalys_per_person
      outcomes$baseline_total_costs <- baseline_row$total_costs
      outcomes$baseline_costs_per_person <- baseline_row$total_costs_per_person
      outcomes$baseline_intervention_costs <- baseline_row$total_intervention_costs
      outcomes$baseline_disease_costs <- baseline_row$total_disease_costs
      outcomes$baseline_total_deaths <- baseline_row$total_deaths
      outcomes$baseline_final_alive <- baseline_row$final_alive
      outcomes$baseline_chd_cases <- baseline_row$total_chd_cases
      outcomes$baseline_stroke_cases <- baseline_row$total_stroke_cases
      outcomes$baseline_copd_cases <- baseline_row$total_copd_cases
      outcomes$baseline_lung_cancer_cases <- baseline_row$total_lung_cancer_cases
      outcomes$baseline_colorectal_cancer_cases <- baseline_row$total_colorectal_cancer_cases
    }
    
    # Extract intervention scenario outcomes if available
    if ("intervention" %in% comparison_summary$scenario) {
      intervention_row <- comparison_summary[comparison_summary$scenario == "intervention", ]
      
      outcomes$intervention_total_qalys <- intervention_row$total_qalys
      outcomes$intervention_qalys_per_person <- intervention_row$qalys_per_person
      outcomes$intervention_total_costs <- intervention_row$total_costs
      outcomes$intervention_costs_per_person <- intervention_row$total_costs_per_person
      outcomes$intervention_intervention_costs <- intervention_row$total_intervention_costs
      outcomes$intervention_disease_costs <- intervention_row$total_disease_costs
      outcomes$intervention_total_deaths <- intervention_row$total_deaths
      outcomes$intervention_final_alive <- intervention_row$final_alive
      outcomes$intervention_chd_cases <- intervention_row$total_chd_cases
      outcomes$intervention_stroke_cases <- intervention_row$total_stroke_cases
      outcomes$intervention_copd_cases <- intervention_row$total_copd_cases
      outcomes$intervention_lung_cancer_cases <- intervention_row$total_lung_cancer_cases
      outcomes$intervention_colorectal_cancer_cases <- intervention_row$total_colorectal_cancer_cases
    }
    
    # Extract equalized attendance scenario outcomes if available
    if ("equalized_attendance" %in% comparison_summary$scenario) {
      equalized_row <- comparison_summary[comparison_summary$scenario == "equalized_attendance", ]
      
      outcomes$equalized_total_qalys <- equalized_row$total_qalys
      outcomes$equalized_qalys_per_person <- equalized_row$qalys_per_person
      outcomes$equalized_total_costs <- equalized_row$total_costs
      outcomes$equalized_costs_per_person <- equalized_row$total_costs_per_person
      outcomes$equalized_intervention_costs <- equalized_row$total_intervention_costs
      outcomes$equalized_disease_costs <- equalized_row$total_disease_costs
      outcomes$equalized_total_deaths <- equalized_row$total_deaths
      outcomes$equalized_final_alive <- equalized_row$final_alive
      outcomes$equalized_chd_cases <- equalized_row$total_chd_cases
      outcomes$equalized_stroke_cases <- equalized_row$total_stroke_cases
      outcomes$equalized_copd_cases <- equalized_row$total_copd_cases
      outcomes$equalized_lung_cancer_cases <- equalized_row$total_lung_cancer_cases
      outcomes$equalized_colorectal_cancer_cases <- equalized_row$total_colorectal_cancer_cases
    }
    
    # Extract ICER results if available
    if (!is.null(icer_analysis)) {
      # ICER for intervention vs baseline
      if (!is.null(icer_analysis$intervention_vs_baseline)) {
        icer_int <- icer_analysis$intervention_vs_baseline
        outcomes$int_vs_base_delta_qalys <- icer_int$delta_qalys
        outcomes$int_vs_base_delta_costs <- icer_int$delta_costs
        outcomes$int_vs_base_icer <- icer_int$icer
        outcomes$int_vs_base_cost_effective_20k <- icer_int$cost_effective_20k
        outcomes$int_vs_base_cost_effective_30k <- icer_int$cost_effective_30k
        outcomes$int_vs_base_qalys_gained_per_person <- icer_int$qalys_gained_per_person
        outcomes$int_vs_base_additional_cost_per_person <- icer_int$intervention_cost_per_person
        
        # Calculate disease cases prevented (intervention vs baseline)
        if ("baseline" %in% comparison_summary$scenario && "intervention" %in% comparison_summary$scenario) {
          baseline_row <- comparison_summary[comparison_summary$scenario == "baseline", ]
          intervention_row <- comparison_summary[comparison_summary$scenario == "intervention", ]
          
          outcomes$int_vs_base_chd_cases_prevented <- baseline_row$total_chd_cases - intervention_row$total_chd_cases
          outcomes$int_vs_base_stroke_cases_prevented <- baseline_row$total_stroke_cases - intervention_row$total_stroke_cases
          outcomes$int_vs_base_copd_cases_prevented <- baseline_row$total_copd_cases - intervention_row$total_copd_cases
          outcomes$int_vs_base_lung_cancer_cases_prevented <- baseline_row$total_lung_cancer_cases - intervention_row$total_lung_cancer_cases
          outcomes$int_vs_base_colorectal_cancer_cases_prevented <- baseline_row$total_colorectal_cancer_cases - intervention_row$total_colorectal_cancer_cases
          outcomes$int_vs_base_deaths_prevented <- baseline_row$total_deaths - intervention_row$total_deaths
        }
      }
      
      # ICER for equalized vs baseline
      if (!is.null(icer_analysis$equalized_vs_baseline)) {
        icer_eq <- icer_analysis$equalized_vs_baseline
        outcomes$eq_vs_base_delta_qalys <- icer_eq$delta_qalys
        outcomes$eq_vs_base_delta_costs <- icer_eq$delta_costs
        outcomes$eq_vs_base_icer <- icer_eq$icer
        outcomes$eq_vs_base_cost_effective_20k <- icer_eq$cost_effective_20k
        outcomes$eq_vs_base_cost_effective_30k <- icer_eq$cost_effective_30k
        outcomes$eq_vs_base_qalys_gained_per_person <- icer_eq$qalys_gained_per_person
        outcomes$eq_vs_base_additional_cost_per_person <- icer_eq$intervention_cost_per_person
        
        # Calculate disease cases prevented (equalized vs baseline)
        if ("baseline" %in% comparison_summary$scenario && "equalized_attendance" %in% comparison_summary$scenario) {
          baseline_row <- comparison_summary[comparison_summary$scenario == "baseline", ]
          equalized_row <- comparison_summary[comparison_summary$scenario == "equalized_attendance", ]
          
          outcomes$eq_vs_base_chd_cases_prevented <- baseline_row$total_chd_cases - equalized_row$total_chd_cases
          outcomes$eq_vs_base_stroke_cases_prevented <- baseline_row$total_stroke_cases - equalized_row$total_stroke_cases
          outcomes$eq_vs_base_copd_cases_prevented <- baseline_row$total_copd_cases - equalized_row$total_copd_cases
          outcomes$eq_vs_base_lung_cancer_cases_prevented <- baseline_row$total_lung_cancer_cases - equalized_row$total_lung_cancer_cases
          outcomes$eq_vs_base_colorectal_cancer_cases_prevented <- baseline_row$total_colorectal_cancer_cases - equalized_row$total_colorectal_cancer_cases
          outcomes$eq_vs_base_deaths_prevented <- baseline_row$total_deaths - equalized_row$total_deaths
        }
      }
      
      # ICER for intervention vs equalized
      if (!is.null(icer_analysis$intervention_vs_equalized)) {
        icer_int_eq <- icer_analysis$intervention_vs_equalized
        outcomes$int_vs_eq_delta_qalys <- icer_int_eq$delta_qalys
        outcomes$int_vs_eq_delta_costs <- icer_int_eq$delta_costs
        outcomes$int_vs_eq_icer <- icer_int_eq$icer
        outcomes$int_vs_eq_cost_effective_20k <- icer_int_eq$cost_effective_20k
        outcomes$int_vs_eq_cost_effective_30k <- icer_int_eq$cost_effective_30k
        outcomes$int_vs_eq_qalys_gained_per_person <- icer_int_eq$qalys_gained_per_person
        outcomes$int_vs_eq_additional_cost_per_person <- icer_int_eq$intervention_cost_per_person
        
        # Calculate disease cases prevented (intervention vs equalized)
        if ("intervention" %in% comparison_summary$scenario && "equalized_attendance" %in% comparison_summary$scenario) {
          intervention_row <- comparison_summary[comparison_summary$scenario == "intervention", ]
          equalized_row <- comparison_summary[comparison_summary$scenario == "equalized_attendance", ]
          
          outcomes$int_vs_eq_chd_cases_prevented <- equalized_row$total_chd_cases - intervention_row$total_chd_cases
          outcomes$int_vs_eq_stroke_cases_prevented <- equalized_row$total_stroke_cases - intervention_row$total_stroke_cases
          outcomes$int_vs_eq_copd_cases_prevented <- equalized_row$total_copd_cases - intervention_row$total_copd_cases
          outcomes$int_vs_eq_lung_cancer_cases_prevented <- equalized_row$total_lung_cancer_cases - intervention_row$total_lung_cancer_cases
          outcomes$int_vs_eq_colorectal_cancer_cases_prevented <- equalized_row$total_colorectal_cancer_cases - intervention_row$total_colorectal_cancer_cases
          outcomes$int_vs_eq_deaths_prevented <- equalized_row$total_deaths - intervention_row$total_deaths
        }
      }
    }
    
    results_list[[i]] <- outcomes
    
    # Progress update for completed replications
    if (verbose && (i %% max(1, floor(n_replications / 10)) == 0 || i == n_replications)) {
      cat(sprintf(
        "Completed replication %d/%d (%.1f%%) - Success: %s\n",
        i, n_replications, 100 * i / n_replications,
        ifelse(outcomes$success, "YES", "NO")
      ))
    }
  }
  
  end_time <- Sys.time()
  runtime_minutes <- as.numeric(difftime(end_time, start_time, units = "mins"))
  
  if (verbose) {
    cat(sprintf(
      "\nCompleted %d replications in %.2f minutes\n",
      n_replications, runtime_minutes
    ))
    cat(sprintf(
      "Successful replications: %d/%d (%.1f%%)\n",
      n_successful, n_replications, 100 * n_successful / n_replications
    ))
    
    if (n_failed > 0) {
      cat(sprintf(
        "Failed replications: %d/%d (%.1f%%)\n",
        n_failed, n_replications, 100 * n_failed / n_replications
      ))
    }
  }
  
  return(list(
    results = results_list,
    n_successful = n_successful,
    n_failed = n_failed,
    runtime_minutes = runtime_minutes,
    scenarios = scenarios,
    parameters = list(
      n_replications = n_replications,
      base_seed = base_seed,
      n_individuals = n_individuals,
      start_year = start_year,
      end_year = end_year
    )
  ))
}


#' Calculate Confidence Intervals for Scenario Comparison
#'
#' Calculate confidence intervals and summary statistics for Monte Carlo scenario results
#'
#' @param monte_carlo_results Output from run_monte_carlo()
#' @param confidence_level Confidence level for intervals (default: 0.95)
#'
#' @export
calculate_scenario_confidence_intervals <- function(monte_carlo_results, confidence_level = 0.95) {
  # Calculate percentiles for confidence interval
  alpha <- 1 - confidence_level
  lower_percentile <- alpha / 2
  upper_percentile <- 1 - alpha / 2
  
  # Extract successful results
  successful_results <- monte_carlo_results$results[sapply(monte_carlo_results$results, function(x) x$success)]
  
  if (length(successful_results) == 0) {
    warning("No successful replications found")
    return(data.frame())
  }
  
  # Get all outcome names from successful results
  all_outcomes <- unique(unlist(lapply(successful_results, names)))
  outcome_names <- all_outcomes[!all_outcomes %in% c("replication", "seed", "success", "error")]
  
  # Initialize results dataframe
  ci_results <- data.frame()
  
  # Calculate confidence intervals for each outcome
  for (outcome in outcome_names) {
    # Extract values for this outcome
    values <- sapply(successful_results, function(x) {
      if (outcome %in% names(x)) {
        return(x[[outcome]])
      } else {
        return(NA)
      }
    })
    
    # Remove NA values
    values <- values[!is.na(values)]
    
    if (length(values) == 0) {
      warning(sprintf("No valid values for outcome: %s", outcome))
      next
    }
    
    outcome_mean <- mean(values)
    outcome_median <- median(values)
    outcome_sd <- sd(values)
    outcome_min <- min(values)
    outcome_max <- max(values)
    
    # Calculate confidence interval
    ci_lower <- quantile(values, lower_percentile, na.rm = TRUE)
    ci_upper <- quantile(values, upper_percentile, na.rm = TRUE)
    ci_width <- ci_upper - ci_lower
    
    # Coefficient of variation (CV) as percentage
    cv_percent <- ifelse(outcome_mean != 0, (outcome_sd / abs(outcome_mean)) * 100, NA)
    
    # Add to results
    ci_results <- rbind(ci_results, data.frame(
      outcome = outcome,
      n_replications = length(values),
      mean = outcome_mean,
      median = outcome_median,
      std_dev = outcome_sd,
      min = outcome_min,
      max = outcome_max,
      lower_ci = ci_lower,
      upper_ci = ci_upper,
      ci_width = ci_width,
      cv_percent = cv_percent,
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by outcome importance (ICERs first, then deltas, then totals)
  priority_order <- c(
    # Intervention vs Baseline
    "int_vs_base_icer", "int_vs_base_delta_qalys", "int_vs_base_delta_costs",
    "int_vs_base_qalys_gained_per_person", "int_vs_base_additional_cost_per_person",
    "int_vs_base_deaths_prevented",
    
    # Equalized vs Baseline
    "eq_vs_base_icer", "eq_vs_base_delta_qalys", "eq_vs_base_delta_costs",
    "eq_vs_base_qalys_gained_per_person", "eq_vs_base_additional_cost_per_person",
    "eq_vs_base_deaths_prevented",
    
    # Intervention vs Equalized
    "int_vs_eq_icer", "int_vs_eq_delta_qalys", "int_vs_eq_delta_costs",
    "int_vs_eq_qalys_gained_per_person", "int_vs_eq_additional_cost_per_person",
    "int_vs_eq_deaths_prevented"
  )
  
  ci_results$priority <- match(ci_results$outcome, priority_order)
  ci_results$priority[is.na(ci_results$priority)] <- 999
  ci_results <- ci_results[order(ci_results$priority, ci_results$outcome), ]
  ci_results$priority <- NULL
  
  rownames(ci_results) <- NULL
  
  return(ci_results)
}


#' Print Confidence Intervals for Scenario Comparison
#'
#' Pretty print confidence interval results with appropriate formatting
#' Updated for 3-scenario analysis
#'
#' @param ci_results Output from calculate_scenario_confidence_intervals()
#' @param show_cost_effectiveness Whether to show cost-effectiveness thresholds for ICER
#'
#' @export
print_scenario_confidence_intervals <- function(ci_results, show_cost_effectiveness = TRUE) {
  if (nrow(ci_results) == 0) {
    cat("No results to display\n")
    return()
  }
  

  # Helper function to format and print a group of outcomes
  print_outcome_group <- function(outcomes_subset, group_title) {
    if (nrow(outcomes_subset) > 0) {
      cat(sprintf("%s:\n", group_title))
      for (i in 1:nrow(outcomes_subset)) {
        row <- outcomes_subset[i, ]
        
        # Custom formatting based on outcome type
        if (grepl("icer", row$outcome)) {
          if (is.finite(row$mean) && row$mean > 0) {
            mean_str <- sprintf("£%.0f per QALY", row$mean)
            ci_str <- sprintf("£%.0f - £%.0f", row$lower_ci, row$upper_ci)
            
            if (show_cost_effectiveness) {
              ce_status <- ""
              if (row$upper_ci <= 20000) {
                ce_status <- " (Highly cost-effective)"
              } else if (row$upper_ci <= 30000) {
                ce_status <- " (Cost-effective)"
              } else if (row$lower_ci > 30000) {
                ce_status <- " (Not cost-effective)"
              } else {
                ce_status <- " (Uncertain cost-effectiveness)"
              }
              ci_str <- paste0(ci_str, ce_status)
            }
          } else {
            mean_str <- "Dominant/Invalid"
            ci_str <- "Cannot calculate"
          }
        } else if (grepl("cost", row$outcome)) {
          mean_str <- sprintf("£%.0f", row$mean)
          ci_str <- sprintf("£%.0f - £%.0f", row$lower_ci, row$upper_ci)
        } else if (grepl("delta_qalys|qalys_gained", row$outcome)) {
          mean_str <- sprintf("%.3f QALYs", row$mean)
          ci_str <- sprintf("%.3f - %.3f", row$lower_ci, row$upper_ci)
        } else {
          mean_str <- sprintf("%.1f", row$mean)
          ci_str <- sprintf("%.1f - %.1f", row$lower_ci, row$upper_ci)
        }
        
        # Create readable outcome label
        outcome_label <- row$outcome
        outcome_label <- gsub("int_vs_base_", "", outcome_label)
        outcome_label <- gsub("eq_vs_base_", "", outcome_label)
        outcome_label <- gsub("int_vs_eq_", "", outcome_label)
        outcome_label <- gsub("_", " ", outcome_label)
        outcome_label <- tools::toTitleCase(outcome_label)
        
        cat(sprintf("  %-28s: %s [%s]\n", outcome_label, mean_str, ci_str))
      }
      cat("\n")
    }
  }
  
  # Intervention vs Baseline outcomes
  int_vs_base_outcomes <- c(
    "int_vs_base_icer", "int_vs_base_delta_qalys", "int_vs_base_delta_costs",
    "int_vs_base_qalys_gained_per_person", "int_vs_base_additional_cost_per_person",
    "int_vs_base_deaths_prevented"
  )
  int_vs_base_results <- ci_results[ci_results$outcome %in% int_vs_base_outcomes, ]
  print_outcome_group(int_vs_base_results, "INTERVENTION (75% attendance) vs BASELINE")
  
  # Equalized vs Baseline outcomes
  eq_vs_base_outcomes <- c(
    "eq_vs_base_icer", "eq_vs_base_delta_qalys", "eq_vs_base_delta_costs",
    "eq_vs_base_qalys_gained_per_person", "eq_vs_base_additional_cost_per_person",
    "eq_vs_base_deaths_prevented"
  )
  eq_vs_base_results <- ci_results[ci_results$outcome %in% eq_vs_base_outcomes, ]
  print_outcome_group(eq_vs_base_results, "EQUALIZED ATTENDANCE (both 43.96%) vs BASELINE")
  
  # Intervention vs Equalized outcomes
  int_vs_eq_outcomes <- c(
    "int_vs_eq_icer", "int_vs_eq_delta_qalys", "int_vs_eq_delta_costs",
    "int_vs_eq_qalys_gained_per_person", "int_vs_eq_additional_cost_per_person",
    "int_vs_eq_deaths_prevented"
  )
  int_vs_eq_results <- ci_results[ci_results$outcome %in% int_vs_eq_outcomes, ]
  print_outcome_group(int_vs_eq_results, "INTERVENTION (75%) vs EQUALIZED ATTENDANCE (43.96%)")
  
  # Disease prevention outcomes (for intervention vs baseline)
  disease_outcomes <- c(
    "int_vs_base_chd_cases_prevented", "int_vs_base_stroke_cases_prevented",
    "int_vs_base_copd_cases_prevented", "int_vs_base_lung_cancer_cases_prevented",
    "int_vs_base_colorectal_cancer_cases_prevented"
  )
  disease_results <- ci_results[ci_results$outcome %in% disease_outcomes, ]
  if (nrow(disease_results) > 0) {
    cat("DISEASE PREVENTION (Intervention vs Baseline):\n")
    for (i in 1:nrow(disease_results)) {
      row <- disease_results[i, ]
      mean_str <- sprintf("%.1f cases", row$mean)
      ci_str <- sprintf("%.1f - %.1f", row$lower_ci, row$upper_ci)
      
      outcome_label <- gsub("int_vs_base_", "", row$outcome)
      outcome_label <- gsub("_", " ", outcome_label)
      outcome_label <- gsub("cases prevented", "", outcome_label)
      outcome_label <- paste(tools::toTitleCase(outcome_label), "cases prevented")
      
      cat(sprintf("  %-28s: %s [%s]\n", outcome_label, mean_str, ci_str))
    }
    cat("\n")
  }
  
  cat(sprintf("Based on %d successful replications\n", ci_results$n_replications[1]))
  cat("Values shown as: Mean [95% Confidence Interval]\n")
  
  if (show_cost_effectiveness) {
    cat("\nCost-effectiveness thresholds: £20,000 (highly cost-effective), £30,000 (cost-effective)\n")
  }
}


#' Save Scenario Confidence Intervals to CSV
#'
#' Save confidence interval results to CSV file
#'
#' @param ci_results Output from calculate_scenario_confidence_intervals()
#' @param filename Output filename
#'
#' @export
save_scenario_confidence_intervals <- function(ci_results, filename = NULL) {
  if (is.null(filename)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    filename <- sprintf("three_scenario_confidence_intervals_%s.csv", timestamp)
  }
  
  # Create formatted version for reporting
  formatted_results <- ci_results
  formatted_results$confidence_interval <- sprintf("%.2f - %.2f", ci_results$lower_ci, ci_results$upper_ci)
  
  # Add formatted mean column
  formatted_results$mean_formatted <- ifelse(
    grepl("cost|icer", formatted_results$outcome),
    sprintf("£%.0f", formatted_results$mean),
    sprintf("%.3f", formatted_results$mean)
  )
  
  # Reorder columns
  formatted_results <- formatted_results[, c(
    "outcome", "n_replications", "mean", "mean_formatted",
    "confidence_interval", "std_dev", "cv_percent",
    "lower_ci", "upper_ci", "ci_width"
  )]
  
  write.csv(formatted_results, filename, row.names = FALSE)
  cat(sprintf("Three-scenario confidence interval results saved to: %s\n", filename))
  
  return(filename)
}