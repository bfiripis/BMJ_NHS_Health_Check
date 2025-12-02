library(dplyr)
library(readr)

source("R/generate_risk_factor_distributions_from_2021_HSE_data.R")
source("R/generate_age_gender_fertility_distributions_from_2022_ONS_data.R")
source("R/calculate_incidence_probabilities.R")
source("R/calculate_mortality_probabilities.R")
source("R/generate_longitudinal_risk_factor_distributions.R")
source("R/generate_microsim_population.R")
source("R/add_children.R")
source("R/update_risk_factors.R")
source("R/apply_disease_incidence.R")
source("R/apply_mortality.R")
source("R/calculate_aggregate_outcomes.R")

# source("R/calculate_smoking_transition_probabilities.R")
source("R/draft_apply_smoking_transitions.R")
source("R/draft_monte_carlo.R")
source("R/apply_nhs_health_check.R")

# Generate age, gender fertility distributions from ONS data 
ons_distributions <- generate_ons_distributions()

# Generate initial year risk factor distributions from HSE data
hse_distributions <- generate_hse_distributions()

# Generate risk factor projections using multinomial logistic regression from HSE data
longitudinal_hse_distributions <- generate_longitudinal_risk_factor_distributions(
  projection_years = 2025:2040, n_samples = 1000)

# Load data files
incidence_data <- read_csv("data/incidence.csv", show_col_types = FALSE)
mortality_data <- read_csv("data/mortality.csv", show_col_types = FALSE)
prevalence_data <- read_csv("data/prevalence.csv", show_col_types = FALSE)

# Load relative risks
smoking_relative_risks <- read.csv("data/smoking_relative_risks.csv")
blood_pressure_relative_risks <- read.csv("data/blood_pressure_relative_risks.csv") 
bmi_relative_risks <- read.csv("data/BMI_relative_risks.csv")

# Calculate incidence probabilities
incidence_probabilities <- calculate_baseline_incidence_probabilities(
  hse_distributions = hse_distributions,
  incidence_data = incidence_data,
  smoking_relative_risks = smoking_relative_risks,
  bmi_relative_risks = bmi_relative_risks,
  bp_relative_risks = blood_pressure_relative_risks
)

# Calculate mortality probabilities
mortality_probabilities <- calculate_post_incidence_mortality(
  mortality_data = mortality_data,
  prevalence_data = prevalence_data
)

#' Calculate Annual Smoking Transition Probabilities
#smoking_transition_probabilities <- calculate_smoking_transition_probabilities(
#  longitudinal_hse_distributions = longitudinal_hse_distributions,
#  current_year = current_year
#  )

# Load utilities and costs
age_sex_utilities <- read.csv("data/age_sex_utilities.csv")
disease_utilities <- read.csv("data/disease_utilities.csv")
costs <- read.csv("data/costs.csv")


#' Run Microsimulation Model
#' 
#' Simulates individual life trajectories over multiple years, tracking disease 
#' incidence, mortality, and health economic outcomes.
#'
#' @param n_individuals Number of individuals to generate for simulation
#' @param start_year Starting year for simulation (default: 2025)
#' @param end_year Ending year for simulation (default: 2040)
#' @param ons_distributions ONS age/gender distributions
#' @param hse_distributions HSE risk factor distributions
#' @param incidence_probabilities Disease incidence probability tables
#' @param smoking_relative_risks Smoking relative risks
#' @param blood_pressure_relative_risks Blood pressure relative risks
#' @param bmi_relative_risks BMI relative risks
#' @param mortality_probabilities Mortality probability tables
#' @param age_sex_utilities Age/sex utility values
#' @param disease_utilities Disease utility decrements
#' @param costs Disease cost estimates
#' @param seed Random seed (default: 9001)
#' @param verbose Logical: print detailed year-by-year monitoring (default: TRUE)
#' 
#' @return List with final_population, model_outputs, and simulation_parameters
#' 
#' @examples
#' \dontrun{
#' # Run baseline microsimulation
#' results <- run_microsimulation(
#'   initial_population = NULL,
#'   start_year = 2025,
#'   end_year = 2040,
#'   ons_distributions = ons_distributions,
#'   hse_distributions = hse_distributions,
#'   incidence_probabilities = incidence_probabilities,
#'   smoking_relative_risks = smoking_relative_risks,
#'   blood_pressure_relative_risks = blood_pressure_relative_risks,
#'   bmi_relative_risks = bmi_relative_risks,
#'   mortality_probabilities = mortality_probabilities,
#'   age_sex_utilities = age_sex_utilities,
#'   disease_utilities = disease_utilities,
#'   costs = costs,
#'   seed = 9001
#' )
#' 
#' # Access results
#' final_population <- results$final_population
#' annual_outcomes <- results$model_outputs
#' }
#' 
#' @export
run_microsimulation <- function(n_individuals,
                                start_year = 2025,
                                end_year = 2040,
                                ons_distributions,
                                hse_distributions,
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
  
  # Create sequence of simulation years
  n_years <- (end_year - start_year)
  simulation_years <- seq(1:n_years)
  
  # Generate microsimulation population 
  initial_population <- generate_population(
    n_individuals = n_individuals,
    ons_distributions = ons_distributions,  
    hse_distributions = hse_distributions,
    seed = seed
  )
  
  population <- initial_population
  
  # Define list of disease
  diseases = c("CHD", "stroke", "COPD", "lung_cancer", "colorectal_cancer")
  
  # initialize model_outputs df 
  model_outputs <- initialize_model_outputs(diseases)
  
  # Print simulation start info (only if verbose)
  if (verbose) {
    cat(sprintf("Running microsimulation: %d individuals, %d-%d (%d years)\n", 
                n_individuals, start_year, end_year, n_years))
  }
  
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
    
    # Increment population age by 1 (need to move this to later in the loop?)
    population$age <- population$age + 1L
    
    # Update individuals risk factors
    # - based on their initial percentile rank and the new year distribution 
    population <- update_risk_factors(population = population,
                                      longitudinal_hse_distributions = longitudinal_hse_distributions,
                                      current_year = current_year)
    
    # # Calculate and Apply Smoking State Transitions ******UNFINISHED******
    # population <- calculate_and_apply_smoking_transitions(
    #  population = population,
    #  longitudinal_hse_distributions = longitudinal_hse_distributions,
    #  current_year = current_year,
    #  seed = seed
    #  )
    
    # Apply Smoking State Transitions (static transition probabilities)
    population <- apply_smoking_transitions(
      population = population,
      seed = seed + year  # Use different seed each year for variety
    )
    
    # Apply NHS Health Check intervention (with default baseline parameters)
    nhs_health_check_result <- apply_nhs_health_check(
      population = population,
      current_year = current_year,
      scenario_type = "baseline",
      male_attendance_rate = 0.3803,
      female_attendance_rate = 0.4396,
      intervention_cost = 150,
      seed = seed + year
    )
    
    
    population <- nhs_health_check_result$population
    annual_intervention_costs <- nhs_health_check_result$intervention_costs
    
    # Store attendance summary for model outputs
    health_check_eligible <- nhs_health_check_result$attendance_summary$total_eligible
    health_check_attendees <- nhs_health_check_result$attendance_summary$total_attending
    
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
  
  # Return results
  results <- list(
    final_population = population,
    model_outputs = model_outputs,
    simulation_parameters = list(
      n_individuals = n_individuals,
      start_year = start_year,
      end_year = end_year,
      n_years = n_years,
      seed = seed
    )
  )
  
  # Final summary (only if verbose)
  if (verbose) {
    alive_count <- sum(population$alive, na.rm = TRUE)
    total_qalys <- sum(model_outputs$total_qalys, na.rm = TRUE)
    total_costs <- sum(model_outputs$total_costs, na.rm = TRUE)
    
    cat(sprintf("\nSimulation complete: %d→%d alive | %.0f QALYs | £%.0fk total costs\n", 
                n_individuals, alive_count, total_qalys, total_costs/1000))
  }
  
  # Save results to CSV files (only if verbose to avoid clutter in scenario comparison)
  if (verbose) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    
    # Save annual model outputs
    outputs_filename <- sprintf("microsim_outputs_%s.csv", timestamp)
    write_csv(model_outputs, outputs_filename)
    
    # Save final population
    population_filename <- sprintf("microsim_population_%s.csv", timestamp)
    write_csv(population, population_filename)
    
    cat(sprintf("Results saved: %s, %s\n", outputs_filename, population_filename))
  }
  
  return(results)
}

# Example Usage
# results <- run_microsimulation(
#   n_individuals = 1000,
#   start_year = 2025,
#   end_year = 2040,
#   ons_distributions = ons_distributions,
#   hse_distributions = hse_distributions,
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