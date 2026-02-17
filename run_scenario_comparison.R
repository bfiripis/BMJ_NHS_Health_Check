#' Run Scenario Comparison for NHS Health Check Intervention
#' 
#' This function wraps the existing run_microsimulation function to compare
#' baseline vs intervention scenarios for the NHS Health Check program.
#' It runs multiple scenarios using the same initial population for fair comparison.
#'
#' @param scenarios Character vector: scenarios to run (default: c("baseline", "intervention", "equalized_attendance"))
#' @param n_individuals Number of individuals to generate for simulation
#' @param start_year Starting year for simulation (default: 2025)
#' @param end_year Ending year for simulation (default: 2040)
#' @param baseline_params List of NHS Health Check parameters for baseline scenario
#' @param intervention_params List of NHS Health Check parameters for intervention scenario
#' @param equalized_attendance_params List of NHS Health Check parameters for equalized attendance scenario
#' @param ons_distributions ONS age/gender distributions
#' @param hse_distributions HSE risk factor distributions
#' @param longitudinal_hse_distributions Longitudinal HSE distributions
#' @param incidence_probabilities Disease incidence probability tables
#' @param smoking_relative_risks Smoking relative risks
#' @param blood_pressure_relative_risks Blood pressure relative risks
#' @param bmi_relative_risks BMI relative risks
#' @param mortality_probabilities Mortality probability tables
#' @param age_sex_utilities Age/sex utility values
#' @param disease_utilities Disease utility decrements
#' @param costs Disease cost estimates
#' @param seed Random seed (default: 1234)
#' @param verbose Logical: print detailed monitoring output (default: TRUE)
#' 
#' @return List with results for each scenario and comparison analysis
#' 
#' @examples
#' \dontrun{
#' # Set up parameter lists
#' baseline_params <- list(
#'   male_attendance_rate = 0.3803,
#'   female_attendance_rate = 0.4396,
#'   intervention_cost = 150,
#'   bmi_reduction = -0.3,
#'   sbp_reduction = -3.22,
#'   smoking_cessation_rate = 0.0635
#' )
#' 
#' intervention_params <- list(
#'   male_attendance_rate = 0.75,
#'   female_attendance_rate = 0.75,
#'   intervention_cost = 150,
#'   bmi_reduction = -0.3,
#'   sbp_reduction = -3.22,
#'   smoking_cessation_rate = 0.0635
#' )
#' 
#' equalized_attendance_params <- list(
#'   male_attendance_rate = 0.4396,
#'   female_attendance_rate = 0.4396,
#'   intervention_cost = 150,
#'   bmi_reduction = -0.3,
#'   sbp_reduction = -3.22,
#'   smoking_cessation_rate = 0.0635
#' )
#' 
#' # Run comparison
#' results <- run_scenario_comparison(
#'   scenarios = c("baseline", "intervention", "equalized_attendance"),
#'   n_individuals = 1000,
#'   baseline_params = baseline_params,
#'   intervention_params = intervention_params,
#'   equalized_attendance_params = equalized_attendance_params,
#'   # ... other standard parameters
#' )
#' 
#' # Access results
#' baseline_results <- results$scenario_results$baseline
#' intervention_results <- results$scenario_results$intervention
#' equalized_results <- results$scenario_results$equalized_attendance
#' comparison <- results$comparison_summary
#' }
#' 
#' @export
# run_scenario_comparison <- function(scenarios = c("baseline", "intervention", "equalized_attendance"),
#                                    n_individuals,
#                                    start_year = 2025,
#                                    end_year = 2040,
#                                    baseline_params = NULL,
#                                    intervention_params = NULL,
#                                    equalized_attendance_params = NULL,
#                                    ons_distributions,
#                                    hse_distributions,
#                                    longitudinal_hse_distributions,
#                                    incidence_probabilities,
#                                    smoking_relative_risks,
#                                    blood_pressure_relative_risks,
#                                    bmi_relative_risks,
#                                    mortality_probabilities,
#                                    age_sex_utilities,
#                                    disease_utilities,
#                                    costs,
#                                    seed = 1234,
#                                    verbose = TRUE) {
#  
#  # Set default parameters if not provided
#  if (is.null(baseline_params)) {
#    baseline_params <- get_nhs_health_check_defaults("baseline_2018")
#  }
#  
#  if (is.null(intervention_params)) {
#    intervention_params <- get_nhs_health_check_defaults("intervention_75_target")
#  }
#  
#  if (is.null(equalized_attendance_params)) {
#    equalized_attendance_params <- list(
#      male_attendance_rate = 0.4396,
#      female_attendance_rate = 0.4396,
#      intervention_cost = 150,
#      bmi_reduction = -0.3,
#      sbp_reduction = -3.22,
#      smoking_cessation_rate = 0.0635
#    )
#  }
#  
#  # Initialize results list
#  scenario_results <- list()
#  
#  if (verbose) {
#    cat("=== NHS HEALTH CHECK SCENARIO COMPARISON ===\n")
#    cat(sprintf("%d individuals | %d-%d | Scenarios: %s\n", 
#                n_individuals, start_year, end_year, paste(scenarios, collapse = ", ")))
#    cat("===========================================\n\n")
#  }
#  
#  # Store initial population to ensure same starting point for all scenarios
#  set.seed(seed)
#  initial_population <- generate_population(
#    n_individuals = n_individuals,
#    ons_distributions = ons_distributions,
#    hse_distributions = hse_distributions,
#    seed = seed
#  )
#  
#  # Loop through each scenario
#  for (scenario in scenarios) {
#    
#    if (verbose) {
#      cat(sprintf("=== %s SCENARIO ===\n", toupper(scenario)))
#    }
#    
#    # Get parameters for this scenario
#    if (scenario == "baseline") {
#      scenario_params <- baseline_params
#    } else if (scenario == "intervention") {
#      scenario_params <- intervention_params
#    } else if (scenario == "equalized_attendance") {
#      scenario_params <- equalized_attendance_params
#    } else {
#      stop(sprintf("Unknown scenario: %s. Use 'baseline', 'intervention', or 'equalized_attendance'", scenario))
#    }
#    
#    if (verbose) {
#      cat(sprintf("NHS Health Check: M/F attendance %.1f%%/%.1f%% | £%.0f per check\n",
#                  scenario_params$male_attendance_rate * 100,
#                  scenario_params$female_attendance_rate * 100,
#                  scenario_params$intervention_cost))
#    }
#    
#    # Run microsimulation for this scenario
#    scenario_results[[scenario]] <- run_microsimulation(
#      initial_population = initial_population,
#      scenario_type = scenario,
#      nhs_params = scenario_params,
#      start_year = start_year,
#      end_year = end_year,
#      ons_distributions = ons_distributions,
#      hse_distributions = hse_distributions,
#      longitudinal_hse_distributions = longitudinal_hse_distributions,
#      incidence_probabilities = incidence_probabilities,
#      smoking_relative_risks = smoking_relative_risks,
#      blood_pressure_relative_risks = blood_pressure_relative_risks,
#      bmi_relative_risks = bmi_relative_risks,
#      mortality_probabilities = mortality_probabilities,
#      age_sex_utilities = age_sex_utilities,
#      disease_utilities = disease_utilities,
#      costs = costs,
#      seed = seed,
#      verbose = verbose
#    )
#    
#    # Print scenario summary
#    if (verbose) {
#      results <- scenario_results[[scenario]]
#      alive_count <- sum(results$final_population$alive, na.rm = TRUE)
#      total_qalys <- sum(results$model_outputs$total_qalys, na.rm = TRUE)
#      total_costs <- sum(results$model_outputs$total_costs, na.rm = TRUE)
#      total_intervention_costs <- sum(results$model_outputs$intervention_costs, na.rm = TRUE)
#      
#      cat(sprintf("Complete: %d→%d alive | %.0f QALYs | £%.0fk total (£%.0fk intervention)\n\n", 
#                  n_individuals, alive_count, total_qalys, total_costs/1000, total_intervention_costs/1000))
#    }
#  }
#  
#  # Generate comparison analysis
#  comparison_summary <- compare_scenario_outcomes(scenario_results)
#  
#  # Calculate ICER comparisons (baseline vs others)
#  icer_analysis <- list()
#  
#  if ("baseline" %in% names(scenario_results)) {
#    baseline_results <- scenario_results$baseline
#    
#    # ICER for intervention vs baseline
#    if ("intervention" %in% names(scenario_results)) {
#      icer_analysis$intervention_vs_baseline <- calculate_icer(baseline_results, scenario_results$intervention)
#      
#      if (verbose) {
#        cat("=== INTERVENTION VS BASELINE COST-EFFECTIVENESS ===\n")
#        icer <- icer_analysis$intervention_vs_baseline
#        cat(sprintf("Additional QALYs: %.3f per person (%.1f total)\n",
#                    icer$qalys_gained_per_person, icer$delta_qalys))
#        cat(sprintf("Intervention costs: £%.2f per person (£%.0fk total)\n",
#                    icer$intervention_cost_per_person, icer$delta_intervention_costs/1000))
#        cat(sprintf("ICER: £%.0f per QALY gained\n",
#                    ifelse(is.finite(icer$icer), icer$icer, 0)))
#        cat(sprintf("Cost-effective at £20k threshold: %s\n",
#                    ifelse(icer$cost_effective_20k, "Yes", "No")))
#        cat(sprintf("Cost-effective at £30k threshold: %s\n\n",
#                    ifelse(icer$cost_effective_30k, "Yes", "No")))
#      }
#    }
#    
#    # ICER for equalized_attendance vs baseline
#    if ("equalized_attendance" %in% names(scenario_results)) {
#      icer_analysis$equalized_vs_baseline <- calculate_icer(baseline_results, scenario_results$equalized_attendance)
#      
#      if (verbose) {
#        cat("=== EQUALIZED ATTENDANCE VS BASELINE COST-EFFECTIVENESS ===\n")
#        icer <- icer_analysis$equalized_vs_baseline
#        cat(sprintf("Additional QALYs: %.3f per person (%.1f total)\n",
#                    icer$qalys_gained_per_person, icer$delta_qalys))
#        cat(sprintf("Intervention costs: £%.2f per person (£%.0fk total)\n",
#                    icer$intervention_cost_per_person, icer$delta_intervention_costs/1000))
#        cat(sprintf("ICER: £%.0f per QALY gained\n",
#                    ifelse(is.finite(icer$icer), icer$icer, 0)))
#        cat(sprintf("Cost-effective at £20k threshold: %s\n",
#                    ifelse(icer$cost_effective_20k, "Yes", "No")))
#        cat(sprintf("Cost-effective at £30k threshold: %s\n\n",
#                    ifelse(icer$cost_effective_30k, "Yes", "No")))
#      }
#    }
#  }
#  
#  # Calculate ICER for intervention vs equalized_attendance if both exist
#  if ("intervention" %in% names(scenario_results) && "equalized_attendance" %in% names(scenario_results)) {
#    icer_analysis$intervention_vs_equalized <- calculate_icer(scenario_results$equalized_attendance, scenario_results$intervention)
#    
#    if (verbose) {
#      cat("=== INTERVENTION VS EQUALIZED ATTENDANCE COST-EFFECTIVENESS ===\n")
#      icer <- icer_analysis$intervention_vs_equalized
#      cat(sprintf("Additional QALYs: %.3f per person (%.1f total)\n",
#                  icer$qalys_gained_per_person, icer$delta_qalys))
#      cat(sprintf("Intervention costs: £%.2f per person (£%.0fk total)\n",
#                  icer$intervention_cost_per_person, icer$delta_intervention_costs/1000))
#      cat(sprintf("ICER: £%.0f per QALY gained\n",
#                  ifelse(is.finite(icer$icer), icer$icer, 0)))
#      cat(sprintf("Cost-effective at £20k threshold: %s\n",
#                  ifelse(icer$cost_effective_20k, "Yes", "No")))
#      cat(sprintf("Cost-effective at £30k threshold: %s\n\n",
#                  ifelse(icer$cost_effective_30k, "Yes", "No")))
#    }
#  }
#  
#  return(list(
#    scenario_results = scenario_results,
#    comparison_summary = comparison_summary,
#    icer_analysis = icer_analysis
#  ))
#}


#' Alternative function that accepts a pre-generated population
#' 
#' This version is used by the Monte Carlo analysis to ensure 
#' consistent populations across replications
#'
#' @param initial_population Pre-generated population dataframe
#' @param scenarios Character vector of scenarios to run
#' @param ... Other parameters passed to run_scenario_comparison
#'
#' @export
run_scenario_comparison <- function(initial_population,
                                    scenarios = c("baseline", "intervention", "equalized_attendance"),
                                    start_year = 2025,
                                    end_year = 2040,
                                    baseline_params = NULL,
                                    intervention_params = NULL,
                                    equalized_attendance_params = NULL,
                                    longitudinal_hse_distributions,
                                    incidence_probabilities,
                                    smoking_relative_risks,
                                    blood_pressure_relative_risks,
                                    bmi_relative_risks,
                                    mortality_probabilities,
                                    age_sex_utilities,
                                    disease_utilities,
                                    costs,
                                    seed = 1234,
                                    verbose = TRUE) {
  
  # Set default parameters if not provided
  if (is.null(baseline_params)) {
    baseline_params <- get_nhs_health_check_defaults("baseline_2018")
  }
  
  if (is.null(intervention_params)) {
    intervention_params <- get_nhs_health_check_defaults("intervention_75_target")
  }
  
  if (is.null(equalized_attendance_params)) {
    equalized_attendance_params <- list(
      male_attendance_rate = 0.4396,
      female_attendance_rate = 0.4396,
      intervention_cost = 150,
      bmi_reduction = -0.3,
      sbp_reduction = -3.22,
      smoking_cessation_rate = 0.0635
    )
  }
  
  # Initialize results list
  scenario_results <- list()
  n_individuals <- nrow(initial_population)
  
  if (verbose) {
    cat("=== NHS HEALTH CHECK SCENARIO COMPARISON ===\n")
    cat(sprintf("%d individuals | %d-%d | Scenarios: %s\n", 
                n_individuals, start_year, end_year, paste(scenarios, collapse = ", ")))
    cat("===========================================\n\n")
  }
  
  # Loop through each scenario
  for (scenario in scenarios) {
    
    if (verbose) {
      cat(sprintf("=== %s SCENARIO ===\n", toupper(scenario)))
    }
    
    # Get parameters for this scenario
    if (scenario == "baseline") {
      scenario_params <- baseline_params
    } else if (scenario == "intervention") {
      scenario_params <- intervention_params
    } else if (scenario == "equalized_attendance") {
      scenario_params <- equalized_attendance_params
    } else {
      stop(sprintf("Unknown scenario: %s. Use 'baseline', 'intervention', or 'equalized_attendance'", scenario))
    }
    
    if (verbose) {
      cat(sprintf("NHS Health Check: M/F attendance %.1f%%/%.1f%% | £%.0f per check\n",
                  scenario_params$male_attendance_rate * 100,
                  scenario_params$female_attendance_rate * 100,
                  scenario_params$intervention_cost))
    }
    
    # Run microsimulation for this scenario with the pre-generated population
    scenario_results[[scenario]] <- run_microsimulation(
      initial_population = initial_population,
      scenario_type = scenario,
      nhs_params = scenario_params,
      start_year = start_year,
      end_year = end_year,
      ons_distributions = ons_distributions,
      hse_distributions = hse_distributions,
      longitudinal_hse_distributions = longitudinal_hse_distributions,
      incidence_probabilities = incidence_probabilities,
      smoking_relative_risks = smoking_relative_risks,
      blood_pressure_relative_risks = blood_pressure_relative_risks,
      bmi_relative_risks = bmi_relative_risks,
      mortality_probabilities = mortality_probabilities,
      age_sex_utilities = age_sex_utilities,
      disease_utilities = disease_utilities,
      costs = costs,
      seed = seed,
      verbose = verbose
    )
    
    # Print scenario summary
    if (verbose) {
      results <- scenario_results[[scenario]]
      alive_count <- sum(results$final_population$alive, na.rm = TRUE)
      total_qalys <- sum(results$model_outputs$total_qalys, na.rm = TRUE)
      total_costs <- sum(results$model_outputs$total_costs, na.rm = TRUE)
      total_intervention_costs <- sum(results$model_outputs$intervention_costs, na.rm = TRUE)
      
      cat(sprintf("Complete: %d→%d alive | %.0f QALYs | £%.0fk total (£%.0fk intervention)\n\n", 
                  n_individuals, alive_count, total_qalys, total_costs/1000, total_intervention_costs/1000))
    }
  }
  
  # Generate comparison analysis
  comparison_summary <- compare_scenario_outcomes(scenario_results)
  
  # Calculate ICER comparisons (baseline vs others)
  icer_analysis <- list()
  
  if ("baseline" %in% names(scenario_results)) {
    baseline_results <- scenario_results$baseline
    
    # ICER for intervention vs baseline
    if ("intervention" %in% names(scenario_results)) {
      icer_analysis$intervention_vs_baseline <- calculate_icer(baseline_results, scenario_results$intervention)
    }
    
    # ICER for equalized_attendance vs baseline
    if ("equalized_attendance" %in% names(scenario_results)) {
      icer_analysis$equalized_vs_baseline <- calculate_icer(baseline_results, scenario_results$equalized_attendance)
    }
  }
  
  # Calculate ICER for intervention vs equalized_attendance if both exist
  if ("intervention" %in% names(scenario_results) && "equalized_attendance" %in% names(scenario_results)) {
    icer_analysis$intervention_vs_equalized <- calculate_icer(scenario_results$equalized_attendance, scenario_results$intervention)
  }
  
  return(list(
    scenario_results = scenario_results,
    comparison_summary = comparison_summary,
    icer_analysis = icer_analysis
  ))
}


#' Compare Outcomes Across Scenarios
#'
#' Creates a summary comparison of key outcomes across scenarios
#'
#' @param scenario_results List of scenario results
#' @return Dataframe with scenario comparisons
compare_scenario_outcomes <- function(scenario_results) {
  
  comparison_list <- list()
  
  for (scenario_name in names(scenario_results)) {
    scenario_data <- scenario_results[[scenario_name]]
    model_outputs <- scenario_data$model_outputs
    params <- scenario_data$simulation_parameters
    
    # Calculate totals
    total_qalys <- sum(model_outputs$total_qalys, na.rm = TRUE)
    total_intervention_costs <- sum(model_outputs$intervention_costs, na.rm = TRUE)
    total_disease_costs <- sum(model_outputs$total_costs, na.rm = TRUE) - total_intervention_costs
    total_costs <- sum(model_outputs$total_costs, na.rm = TRUE)
    
    # Calculate per-person metrics
    n_individuals <- params$n_individuals
    qalys_per_person <- total_qalys / n_individuals
    disease_costs_per_person <- total_disease_costs / n_individuals
    intervention_costs_per_person <- total_intervention_costs / n_individuals
    total_costs_per_person <- total_costs / n_individuals
    
    # Calculate final population metrics
    final_alive <- sum(scenario_data$final_population$alive, na.rm = TRUE)
    total_deaths <- sum(model_outputs$deaths_total, na.rm = TRUE)
    
    # Calculate disease totals
    total_chd <- sum(model_outputs$chd_incidence_count, na.rm = TRUE)
    total_stroke <- sum(model_outputs$stroke_incidence_count, na.rm = TRUE)
    total_copd <- sum(model_outputs$copd_incidence_count, na.rm = TRUE)
    total_lung_cancer <- sum(model_outputs$lung_cancer_incidence_count, na.rm = TRUE)
    total_colorectal_cancer <- sum(model_outputs$colorectal_cancer_incidence_count, na.rm = TRUE)
    
    comparison_list[[scenario_name]] <- data.frame(
      scenario = scenario_name,
      n_individuals = n_individuals,
      final_alive = final_alive,
      total_deaths = total_deaths,
      total_qalys = round(total_qalys, 1),
      qalys_per_person = round(qalys_per_person, 3),
      total_disease_costs = round(total_disease_costs, 0),
      disease_costs_per_person = round(disease_costs_per_person, 0),
      total_intervention_costs = round(total_intervention_costs, 0),
      intervention_costs_per_person = round(intervention_costs_per_person, 0),
      total_costs = round(total_costs, 0),
      total_costs_per_person = round(total_costs_per_person, 0),
      cost_per_qaly = round(ifelse(total_qalys > 0, total_costs / total_qalys, NA), 0),
      total_chd_cases = total_chd,
      total_stroke_cases = total_stroke,
      total_copd_cases = total_copd,
      total_lung_cancer_cases = total_lung_cancer,
      total_colorectal_cancer_cases = total_colorectal_cancer,
      male_attendance_rate = round(params$nhs_params$male_attendance_rate, 3),
      female_attendance_rate = round(params$nhs_params$female_attendance_rate, 3)
    )
  }
  
  # Combine all scenarios
  comparison_df <- do.call(rbind, comparison_list)
  rownames(comparison_df) <- NULL
  
  return(comparison_df)
}


#' Calculate Incremental Cost-Effectiveness Ratio (ICER)
#'
#' This function calculates ICER using only the NHS Health Check intervention costs
#' rather than total healthcare system costs. This provides a policy-relevant answer
#' to: "Is spending money on NHS Health Checks cost-effective?"
#'
#' @param baseline_results Results from baseline scenario
#' @param intervention_results Results from intervention scenario
#' @return List with ICER calculation details
calculate_icer <- function(baseline_results, intervention_results) {
  
  # Extract QALYs for each scenario
  baseline_qalys <- sum(baseline_results$model_outputs$total_qalys, na.rm = TRUE)
  intervention_qalys <- sum(intervention_results$model_outputs$total_qalys, na.rm = TRUE)
  
  # Extract INTERVENTION costs only (not total healthcare costs)
  baseline_intervention_costs <- sum(baseline_results$model_outputs$intervention_costs, na.rm = TRUE)
  intervention_intervention_costs <- sum(intervention_results$model_outputs$intervention_costs, na.rm = TRUE)
  
  # Extract total healthcare costs (for reference and comparison)
  baseline_total_costs <- sum(baseline_results$model_outputs$total_costs, na.rm = TRUE)
  intervention_total_costs <- sum(intervention_results$model_outputs$total_costs, na.rm = TRUE)
  
  # Calculate differences
  delta_qalys <- intervention_qalys - baseline_qalys
  delta_intervention_costs <- intervention_intervention_costs - baseline_intervention_costs
  delta_total_costs <- intervention_total_costs - baseline_total_costs
  
  # Calculate ICER using intervention costs only (primary metric)
  icer <- ifelse(delta_qalys != 0, delta_intervention_costs / delta_qalys, NA)
  
  # Calculate ICER using total costs (for comparison/reference)
  icer_total_costs <- ifelse(delta_qalys != 0, delta_total_costs / delta_qalys, NA)
  
  # Determine cost-effectiveness
  cost_effective_20k <- !is.na(icer) && icer <= 20000
  cost_effective_30k <- !is.na(icer) && icer <= 30000
  
  # Calculate per-person metrics for easier interpretation
  n_individuals <- baseline_results$simulation_parameters$n_individuals
  intervention_cost_per_person <- delta_intervention_costs / n_individuals
  qalys_gained_per_person <- delta_qalys / n_individuals
  healthcare_cost_savings <- -delta_total_costs  # Positive = savings
  healthcare_savings_per_person <- healthcare_cost_savings / n_individuals
  
  result <- list(
    # QALY outcomes
    baseline_qalys = baseline_qalys,
    intervention_qalys = intervention_qalys,
    delta_qalys = delta_qalys,
    qalys_gained_per_person = qalys_gained_per_person,
    
    # Intervention costs (primary for ICER)
    baseline_costs = baseline_intervention_costs,
    intervention_costs = intervention_intervention_costs,
    delta_costs = delta_intervention_costs,
    
    # Additional intervention cost fields
    baseline_intervention_costs = baseline_intervention_costs,
    intervention_intervention_costs = intervention_intervention_costs,
    delta_intervention_costs = delta_intervention_costs,
    intervention_cost_per_person = intervention_cost_per_person,
    
    # Total healthcare costs (for reference)
    baseline_total_costs = baseline_total_costs,
    intervention_total_costs = intervention_total_costs,
    delta_total_costs = delta_total_costs,
    healthcare_cost_savings = healthcare_cost_savings,
    healthcare_savings_per_person = healthcare_savings_per_person,
    
    # ICERs
    icer = icer,  # Primary ICER using intervention costs
    icer_total_costs = icer_total_costs,  # Reference ICER using total costs
    
    # Cost-effectiveness decisions
    cost_effective_20k = cost_effective_20k,
    cost_effective_30k = cost_effective_30k,
    
    # Additional context
    n_individuals = n_individuals
  )
  
  return(result)
}