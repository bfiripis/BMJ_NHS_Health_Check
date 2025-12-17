#' Run Microsimulation
#'
#'
#' @param initial_population Pre-generated population (from scenario comparison)
#' @param scenario_type Character: "baseline" or "intervention"
#' @param nhs_params List of NHS Health Check parameters
#' @param start_year Starting year for simulation (default: 2025)
#' @param end_year Ending year for simulation (default: 2040)
#' @param ons_distributions ONS age/gender distributions for population generation
#' @param hse_distributions HSE risk factor distributions for new births
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
#' @param show_progress Logical: show detailed progress bars (default: TRUE)
#'
#' @return List with final_population, model_outputs, and simulation_parameters

library(dplyr)
library(readr)

run_microsimulation <- function(initial_population,
                                scenario_type = "baseline",
                                nhs_params = NULL,
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
                                seed = NULL,
                                verbose = TRUE,
                                show_progress = TRUE) {
  # Create sequence of simulation years
  n_years <- (end_year - start_year)
  simulation_years <- seq(1:n_years)
  
  # Use provided initial population
  population <- initial_population
  n_individuals <- nrow(population)
  
  # Set default NHS parameters if not provided
  if (is.null(nhs_params)) {
    nhs_params <- list(
      male_attendance_rate = 0.3803,
      female_attendance_rate = 0.4396,
      intervention_cost = 150,
      bmi_reduction = -0.3,
      sbp_reduction = -3.22,
      smoking_cessation_rate = 0.0635
    )
  }
  
  # Define list of diseases
  diseases <- c("CHD", "stroke", "COPD", "lung_cancer", "colorectal_cancer")
  
  # Initialize model_outputs df
  model_outputs <- initialize_model_outputs(diseases)
  
  # Setup progress tracking
  main_pb <- NULL
  step_pb <- NULL
  
  # Initialize main progress bar and header
  if (verbose && show_progress) {
    cat(sprintf("\n=== %s MICROSIMULATION ===\n", toupper(scenario_type)))
    cat(sprintf(
      "Population: %s | Years: %d-%d (%d years) | NHS attendance: M/F %.1f%%/%.1f%%\n",
      format(n_individuals, big.mark = ","),
      start_year, end_year, n_years,
      nhs_params$male_attendance_rate * 100,
      nhs_params$female_attendance_rate * 100
    ))
    cat("Enhanced with percentile rank recalculation post-intervention\n\n")
    
    # Main simulation progress bar
    main_pb <- txtProgressBar(
      min = 0,
      max = n_years,
      style = 3,
      width = 60,
      char = "="
    )
    
    cat("Simulation Progress:\n")
  }
  
  # Print simulation start info (only if verbose and not showing progress)
  if (verbose && !show_progress) {
    cat(sprintf(
      "Running %s microsimulation: %d individuals, %d-%d (%d years)\n",
      toupper(scenario_type), n_individuals, start_year, end_year, n_years
    ))
    cat(sprintf(
      "NHS Health Check - M/F attendance: %.1f%%/%.1f%%, Cost: £%.0f\n",
      nhs_params$male_attendance_rate * 100,
      nhs_params$female_attendance_rate * 100,
      nhs_params$intervention_cost
    ))
    cat("Enhanced with percentile rank recalculation post-intervention\n")
  }
  
  # Helper function to create step progress bar
  create_step_progress <- function(label, n_steps) {
    if (verbose && show_progress) {
      cat(sprintf("\n  %s", label))
      return(txtProgressBar(min = 0, max = n_steps, style = 1, width = 30, char = "."))
    }
    return(NULL)
  }
  
  # Helper function to update step progress
  update_step_progress <- function(pb, current) {
    if (!is.null(pb)) {
      setTxtProgressBar(pb, current)
    }
  }
  
  # Helper function to close step progress
  close_step_progress <- function(pb) {
    if (!is.null(pb)) {
      close(pb)
      cat(" ✓\n")
    }
  }
  
  # For loop - each cycle represents 1 year in the microsimulation
  for (year in simulation_years) {
    current_year <- start_year + year - 1L
    year_start_time <- Sys.time()
    
    # Store current population before changes for comparison
    previous_population <- population
    
    # === STEP 1: POPULATION MAINTENANCE ===
    if (verbose && show_progress) {
      step_pb <- create_step_progress(sprintf("Year %d - Handling aging, births & deaths", current_year), 3)
    }
    
    # Filter out dead individuals from previous cycle
    population <- population %>%
      dplyr::filter(alive == TRUE)
    
    update_step_progress(step_pb, 1)
    
    # Age the population - increase age of those still alive
    population <- population %>%
      mutate(age = age + 1)
    
    update_step_progress(step_pb, 2)
    
    # Process births and add children
    population <- process_births_and_add_children(
      population = population,
      current_year = current_year,
      ons_distributions = ons_distributions,
      hse_distributions = hse_distributions,
      verbose = verbose
    )
    
    update_step_progress(step_pb, 3)
    close_step_progress(step_pb)
    
    # === STEP 2: NHS HEALTH CHECK INTERVENTION ===
    if (verbose && show_progress) {
      step_pb <- create_step_progress("Applying NHS Health Check intervention", 2)
    }
    
    # Apply NHS Health Check based on scenario type
    nhs_health_check_result <- apply_nhs_health_check(
      population = population,
      scenario_type = scenario_type,
      current_year = current_year,
      longitudinal_hse_distributions = longitudinal_hse_distributions,
      male_attendance_rate = nhs_params$male_attendance_rate,
      female_attendance_rate = nhs_params$female_attendance_rate,
      intervention_cost = nhs_params$intervention_cost,
      bmi_reduction = nhs_params$bmi_reduction,
      sbp_reduction = nhs_params$sbp_reduction,
      smoking_cessation_rate = nhs_params$smoking_cessation_rate,
      seed = seed,
      verbose = verbose
    )
    
    population <- nhs_health_check_result$population
    annual_intervention_costs <- nhs_health_check_result$intervention_costs
    
    # Store attendance summary for model outputs
    health_check_eligible <- nhs_health_check_result$attendance_summary$total_eligible
    health_check_attendees <- nhs_health_check_result$attendance_summary$total_attending
    
    update_step_progress(step_pb, 1)
    
    # Calculate intervention costs for this year
    annual_intervention_costs <- calculate_annual_intervention_costs(
      population = population,
      current_year = current_year,
      nhs_params = nhs_params
    )
    
    update_step_progress(step_pb, 2)
    close_step_progress(step_pb)
    
    # === STEP 3: RISK FACTOR UPDATES ===
    if (verbose && show_progress) {
      step_pb <- create_step_progress("Updating risk factors", 1)
    }
    
    # Update risk factors - handles natural aging and smoking transitions
    population <- update_risk_factors(
      population = population,
      current_year = current_year,
      longitudinal_hse_distributions = longitudinal_hse_distributions,
      verbose = verbose
    )
    
    update_step_progress(step_pb, 1)
    close_step_progress(step_pb)
    
    # === STEP 4: DISEASE INCIDENCE ===
    if (verbose && show_progress) {
      step_pb <- create_step_progress("Applying disease incidence", length(diseases))
    }
    
    # Apply disease incidence for each disease type
    for (disease_idx in 1:length(diseases)) {
      disease <- diseases[disease_idx]
      
      population <- apply_disease_incidence(
        population = population,
        current_year = current_year,
        incidence_probabilities = incidence_probabilities,
        smoking_relative_risks = smoking_relative_risks,
        blood_pressure_relative_risks = blood_pressure_relative_risks,
        bmi_relative_risks = bmi_relative_risks,
        diseases = disease,
        seed = seed
      )
      
      update_step_progress(step_pb, disease_idx)
    }
    close_step_progress(step_pb)
    
    # === STEP 5: MORTALITY ===
    if (verbose && show_progress) {
      step_pb <- create_step_progress("Applying mortality", 1)
    }
    
    # Apply mortality probabilities and simulate death occurrence
    population <- apply_mortality(
      population = population,
      mortality_probabilities = mortality_probabilities,
      diseases = diseases,
      current_year = current_year,
      seed = seed
    )
    
    update_step_progress(step_pb, 1)
    close_step_progress(step_pb)
    
    # === STEP 6: OUTCOME CALCULATION ===
    if (verbose && show_progress) {
      step_pb <- create_step_progress("Calculating annual outcomes", 1)
    }
    
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
    
    update_step_progress(step_pb, 1)
    close_step_progress(step_pb)
    
    # Update main progress bar
    if (verbose && show_progress) {
      setTxtProgressBar(main_pb, year)
    }
    
    # Concise year-by-year monitoring (only if verbose and not showing progress)
    if (verbose && !show_progress) {
      cat(sprintf(
        "Year %d: %d alive | Deaths: %d | QALYs: %.0f | Costs: £%.0fk\n",
        current_year,
        annual_outcomes$alive_population,
        annual_outcomes$deaths_total,
        annual_outcomes$total_qalys,
        annual_outcomes$total_costs / 1000
      ))
    }
  }
  
  # Close main progress bar
  if (verbose && show_progress) {
    close(main_pb)
    cat("\n\nSimulation complete! ✓\n")
  }
  
  # Return results
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