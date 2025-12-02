#' Run Scenario Comparison for NHS Health Check Intervention
#' 
#' This function wraps the existing run_microsimulation function to compare
#' baseline vs intervention scenarios for the NHS Health Check program.
#' It runs multiple scenarios using the same initial population for fair comparison.
#'
#' @param scenarios Character vector: scenarios to run (default: c("baseline", "intervention"))
#' @param n_individuals Number of individuals to generate for simulation
#' @param start_year Starting year for simulation (default: 2025)
#' @param end_year Ending year for simulation (default: 2040)
#' @param baseline_params List of NHS Health Check parameters for baseline scenario
#' @param intervention_params List of NHS Health Check parameters for intervention scenario
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
#' @param seed Random seed (default: 9001)
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
#' # Run comparison
#' results <- run_scenario_comparison(
#'   scenarios = c("baseline", "intervention"),
#'   n_individuals = 1000,
#'   baseline_params = baseline_params,
#'   intervention_params = intervention_params,
#'   # ... other standard parameters
#' )
#' 
#' # Access results
#' baseline_results <- results$scenario_results$baseline
#' intervention_results <- results$scenario_results$intervention
#' comparison <- results$comparison_summary
#' icer <- results$icer_analysis
#' }
#' 
#' @export
run_scenario_comparison <- function(scenarios = c("baseline", "intervention"),
                                    n_individuals,
                                    start_year = 2025,
                                    end_year = 2040,
                                    baseline_params = NULL,
                                    intervention_params = NULL,
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
                                    costs,
                                    seed = 9001,
                                    verbose = TRUE) {
  
  # Set default parameters if not provided
  if (is.null(baseline_params)) {
    baseline_params <- get_nhs_health_check_defaults("baseline_2018")
  }
  
  if (is.null(intervention_params)) {
    intervention_params <- get_nhs_health_check_defaults("intervention_75_target")
  }
  
  # Initialize results list
  scenario_results <- list()
  
  if (verbose) {
    cat("=== NHS HEALTH CHECK SCENARIO COMPARISON ===\n")
    cat(sprintf("%d individuals | %d-%d | Scenarios: %s\n", 
                n_individuals, start_year, end_year, paste(scenarios, collapse = ", ")))
    cat("===========================================\n\n")
  }
  
  # Store initial population to ensure same starting point for all scenarios
  set.seed(seed)
  initial_population <- generate_population(
    n_individuals = n_individuals,
    ons_distributions = ons_distributions,
    hse_distributions = hse_distributions,
    seed = seed
  )
  
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
    } else {
      stop(sprintf("Unknown scenario: %s. Use 'baseline' or 'intervention'", scenario))
    }
    
    if (verbose) {
      cat(sprintf("NHS Health Check: M/F attendance %.1f%%/%.1f%% | £%.0f per check\n",
                  scenario_params$male_attendance_rate * 100,
                  scenario_params$female_attendance_rate * 100,
                  scenario_params$intervention_cost))
    }
    
    # Run microsimulation for this scenario
    scenario_results[[scenario]] <- run_microsimulation_with_nhs_health_check(
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
  
  # Calculate ICER if both baseline and intervention scenarios exist
  icer_analysis <- NULL
  if ("baseline" %in% names(scenario_results) && "intervention" %in% names(scenario_results)) {
    icer_analysis <- calculate_icer(scenario_results$baseline, scenario_results$intervention)
    
    if (verbose) {
      cat("=== COST-EFFECTIVENESS ANALYSIS ===\n")
      cat(sprintf("Additional QALYs: %.1f | Additional costs: £%.0fk\n",
                  icer_analysis$delta_qalys, icer_analysis$delta_costs/1000))
      
      if (!is.na(icer_analysis$icer)) {
        cat(sprintf("ICER: £%.0f per QALY | Cost-effective (£30k): %s\n",
                    icer_analysis$icer,
                    ifelse(icer_analysis$cost_effective_30k, "YES", "NO")))
      } else {
        cat("ICER: Cannot calculate (no QALY difference)\n")
      }
      cat("==================================\n\n")
    }
  }
  
  # Save results
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Save comparison summary
  comparison_filename <- sprintf("nhs_health_check_comparison_%s.csv", timestamp)
  write_csv(comparison_summary, comparison_filename)
  
  # Save individual scenario results
  for (scenario in names(scenario_results)) {
    outputs_filename <- sprintf("microsim_outputs_%s_%s.csv", scenario, timestamp)
    population_filename <- sprintf("microsim_population_%s_%s.csv", scenario, timestamp)
    
    write_csv(scenario_results[[scenario]]$model_outputs, outputs_filename)
    write_csv(scenario_results[[scenario]]$final_population, population_filename)
  }
  
  if (verbose) {
    cat(sprintf("Results saved with timestamp: %s\n", timestamp))
  }
  
  # Return comprehensive results
  return(list(
    scenario_results = scenario_results,
    comparison_summary = comparison_summary,
    icer_analysis = icer_analysis,
    parameters = list(
      scenarios = scenarios,
      n_individuals = n_individuals,
      start_year = start_year,
      end_year = end_year,
      baseline_params = baseline_params,
      intervention_params = intervention_params,
      seed = seed
    )
  ))
}


#' Modified Run Microsimulation with NHS Health Check Integration
#'
#' This is a modified version of your existing run_microsimulation function
#' that includes the NHS Health Check intervention
#'
#' @param initial_population Pre-generated initial population
#' @param scenario_type Character: "baseline" or "intervention"
#' @param nhs_params List of NHS Health Check parameters
#' @param verbose Logical: print monitoring output (default: FALSE for scenario comparison)
#' @param ... Other parameters passed to original run_microsimulation
#' @return Same structure as original run_microsimulation
run_microsimulation_with_nhs_health_check <- function(initial_population,
                                                      scenario_type,
                                                      nhs_params,
                                                      start_year = 2025,
                                                      end_year = 2040,
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
                                                      costs,
                                                      seed = 9001,
                                                      verbose = FALSE) {
  
  # Create sequence of simulation years
  n_years <- (end_year - start_year)
  simulation_years <- seq(1:n_years)
  
  # Use provided initial population
  population <- initial_population
  n_individuals <- nrow(initial_population)
  
  # Define list of diseases
  diseases = c("CHD", "stroke", "COPD", "lung_cancer", "colorectal_cancer")
  
  # Initialize model_outputs df 
  model_outputs <- initialize_model_outputs(diseases)
  
  # For loop - each cycle represents 1 year in the microsimulation
  for (year in simulation_years) {
    
    current_year <- start_year + year - 1L
    
    # Store current population before changes for comparison
    previous_population <- population
    
    # Filter out dead individuals from previous cycle
    population <- population %>% filter(alive == TRUE)
    
    # Process births and add children
    population <- process_births_and_add_children(
      population = population,
      current_year = current_year,
      ons_distributions = ons_distributions,
      hse_distributions = hse_distributions
    )
    
    # Increment population age by 1
    population$age <- population$age + 1L
    
    # Update individuals risk factors
    population <- update_risk_factors(population = population,
                                      longitudinal_hse_distributions = longitudinal_hse_distributions,
                                      current_year = current_year)
    
    # Apply Smoking State Transitions
    population <- apply_smoking_transitions(
      population = population,
      seed = seed + year
    )
    
    # Apply NHS Health Check intervention
    nhs_health_check_result <- apply_nhs_health_check(
      population = population,
      current_year = current_year,
      scenario_type = scenario_type,
      male_attendance_rate = nhs_params$male_attendance_rate,
      female_attendance_rate = nhs_params$female_attendance_rate,
      intervention_cost = nhs_params$intervention_cost,
      bmi_reduction = nhs_params$bmi_reduction,
      sbp_reduction = nhs_params$sbp_reduction,
      smoking_cessation_rate = nhs_params$smoking_cessation_rate,
      seed = seed + year
    )
    
    population <- nhs_health_check_result$population
    annual_intervention_costs <- nhs_health_check_result$intervention_costs
    
    # Apply disease incidence probabilities and simulate disease occurrence
    population <- apply_disease_incidence(
      population = population,
      current_year = current_year,
      incidence_probabilities = incidence_probabilities,
      smoking_relative_risks = smoking_relative_risks,
      blood_pressure_relative_risks = blood_pressure_relative_risks,
      bmi_relative_risks = bmi_relative_risks,
      diseases = diseases,
      seed = seed
    )
    
    # Apply mortality probabilities and simulate death occurrence
    population <- apply_mortality(
      population = population,
      mortality_probabilities = mortality_probabilities,
      diseases = diseases,
      current_year = current_year,
      seed = seed
    )
    
    # Calculate aggregate outcomes for this year
    annual_outcomes <- calculate_aggregate_outcomes(
      population = population,
      current_year = current_year,
      age_sex_utilities = age_sex_utilities,
      disease_utilities = disease_utilities,
      costs = costs,
      previous_population = previous_population,
      utility_method = "minimum",
      intervention_costs = annual_intervention_costs
    )
    
    # Add annual outcomes to model_outputs dataframe
    model_outputs <- add_annual_outcomes(model_outputs, annual_outcomes)
    
    # Update previous population for next iteration
    previous_population <- population
    
    # Concise year-by-year monitoring (only if verbose)
    if (verbose) {
      cat(sprintf("Year %d: %d alive | Deaths: %d | QALYs: %.0f | Costs: £%.0fk\n",
                  current_year,
                  annual_outcomes$alive_population,
                  annual_outcomes$deaths_total,
                  annual_outcomes$total_qalys,
                  annual_outcomes$total_costs/1000))
    }
  }
  
  # Return results in same format as original function
  results <- list(
    final_population = population,
    model_outputs = model_outputs,
    simulation_parameters = list(
      scenario_type = scenario_type,
      nhs_params = nhs_params,
      n_individuals = n_individuals,
      start_year = start_year,
      end_year = end_year,
      n_years = n_years,
      seed = seed
    )
  )
  
  return(results)
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
#' @param baseline_results Results from baseline scenario
#' @param intervention_results Results from intervention scenario
#' @return List with ICER calculation details
calculate_icer <- function(baseline_results, intervention_results) {
  
  # Extract totals for each scenario
  baseline_qalys <- sum(baseline_results$model_outputs$total_qalys, na.rm = TRUE)
  baseline_costs <- sum(baseline_results$model_outputs$total_costs, na.rm = TRUE)
  
  intervention_qalys <- sum(intervention_results$model_outputs$total_qalys, na.rm = TRUE)
  intervention_costs <- sum(intervention_results$model_outputs$total_costs, na.rm = TRUE)
  
  # Calculate differences
  delta_qalys <- intervention_qalys - baseline_qalys
  delta_costs <- intervention_costs - baseline_costs
  
  # Calculate ICER
  icer <- ifelse(delta_qalys != 0, delta_costs / delta_qalys, NA)
  
  # Determine cost-effectiveness
  cost_effective_20k <- !is.na(icer) && icer <= 20000
  cost_effective_30k <- !is.na(icer) && icer <= 30000
  
  result <- list(
    baseline_qalys = baseline_qalys,
    intervention_qalys = intervention_qalys,
    delta_qalys = delta_qalys,
    baseline_costs = baseline_costs,
    intervention_costs = intervention_costs,
    delta_costs = delta_costs,
    icer = icer,
    cost_effective_20k = cost_effective_20k,
    cost_effective_30k = cost_effective_30k
  )
  
  return(result)
}


# Example USage
#
# # Load all required data
# library(dplyr)
# library(readr)
# 
# # Source required functions
# source("R/apply_nhs_health_check.R")  # NHS Health Check function
# source("R/run_scenario_comparison.R") # This file
# # ... source all your other required functions
# 
# # Load data distributions and parameters
# ons_distributions <- generate_ons_distributions()
# hse_distributions <- generate_hse_distributions()
# # ... load all your standard data files
# 
# # Method 1: Use default parameters
# baseline_params <- get_nhs_health_check_defaults("baseline_2018")
# intervention_params <- get_nhs_health_check_defaults("intervention_75_target")
# 
# results <- run_scenario_comparison(
#   scenarios = c("baseline", "intervention"),
#   n_individuals = 1000,
#   start_year = 2025,
#   end_year = 2035,  # Shorter for faster testing
#   baseline_params = baseline_params,
#   intervention_params = intervention_params,
#   ons_distributions = ons_distributions,
#   hse_distributions = hse_distributions,
#   longitudinal_hse_distributions = longitudinal_hse_distributions,
#   incidence_probabilities = incidence_probabilities,
#   smoking_relative_risks = smoking_relative_risks,
#   blood_pressure_relative_risks = blood_pressure_relative_risks,
#   bmi_relative_risks = bmi_relative_risks,
#   mortality_probabilities = mortality_probabilities,
#   age_sex_utilities = age_sex_utilities,
#   disease_utilities = disease_utilities,
#   costs = costs,
#   seed = 9001
# )
# 
# # Method 2: Custom parameters for sensitivity analysis
# custom_intervention_params <- list(
#   male_attendance_rate = 0.80,        # Higher than standard 75%
#   female_attendance_rate = 0.80,      # Higher than standard 75%
#   intervention_cost = 120,             # Lower cost scenario
#   bmi_reduction = -0.5,                # Stronger BMI effect
#   sbp_reduction = -4.0,                # Stronger BP effect
#   smoking_cessation_rate = 0.10       # Higher smoking cessation rate
# )
# 
# sensitivity_results <- run_scenario_comparison(
#   scenarios = c("baseline", "intervention"),
#   n_individuals = 500,  # Smaller for speed
#   baseline_params = baseline_params,
#   intervention_params = custom_intervention_params,
#   # ... same other parameters
#   seed = 9002
# )
# 
# # Access and analyze results
# baseline_results <- results$scenario_results$baseline
# intervention_results <- results$scenario_results$intervention
# comparison_summary <- results$comparison_summary
# icer_analysis <- results$icer_analysis
# 
# # Print key findings
# print("=== SCENARIO COMPARISON ===")
# print(comparison_summary)
# 
# if (!is.null(icer_analysis)) {
#   cat(sprintf("\nICER: £%.0f per QALY gained\n", icer_analysis$icer))
#   cat(sprintf("Cost-effective at £30k threshold: %s\n", 
#               ifelse(icer_analysis$cost_effective_30k, "YES", "NO")))
# }