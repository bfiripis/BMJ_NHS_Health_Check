library(dplyr)
library(readr)

# Source all functions
source("R/calculate_incidence_probabilities.R")
source("R/calculate_mortality_probabilities.R") 
source("R/generate_age_gender_fertility_distributions_from_2022_ONS_data.R")
source("R/generate_risk_factor_distributions_from_2021_HSE_data.R")
source("R/generate_longitudinal_risk_factor_distributions.R")
source("R/generate_microsim_population.R")
source("R/add_children.R")
source("R/update_risk_factors.R")
source("R/apply_disease_incidence.R")
source("R/apply_mortality.R")
source("R/calculate_aggregate_outcomes.R")
source("R/draft_apply_smoking_transitions.R")
source("R/apply_nhs_health_check.R")
source("R/run_scenario_comparison.R")
source("R/run_microsimulation.R")
source("R/get_nhs_health_check_defaults.R")
source("R/run_monte_carlo.R")

# Load data
incidence_data <- read_csv("data/incidence.csv", show_col_types = FALSE)
mortality_data <- read_csv("data/mortality.csv", show_col_types = FALSE)
prevalence_data <- read_csv("data/prevalence.csv", show_col_types = FALSE)
smoking_relative_risks <- read.csv("data/smoking_relative_risks.csv")
blood_pressure_relative_risks <- read.csv("data/blood_pressure_relative_risks.csv")
bmi_relative_risks <- read.csv("data/BMI_relative_risks.csv")
age_sex_utilities <- read.csv("data/age_sex_utilities.csv")
disease_utilities <- read.csv("data/disease_utilities.csv")
costs <- read.csv("data/costs.csv")

# Generate distributions
ons_distributions <- generate_ons_distributions()
hse_distributions <- generate_hse_distributions()
longitudinal_hse_distributions <- generate_longitudinal_risk_factor_distributions(projection_years = 2025:2040, n_samples = 1000)

# Generate incidence/mortality probabilities
incidence_probabilities <- calculate_baseline_incidence_probabilities(
  hse_distributions = hse_distributions,
  incidence_data = incidence_data,
  smoking_relative_risks = smoking_relative_risks,
  bmi_relative_risks = bmi_relative_risks,
  bp_relative_risks = blood_pressure_relative_risks
)

mortality_probabilities <- calculate_post_incidence_mortality(
  mortality_data = mortality_data,
  prevalence_data = prevalence_data
)

# Generate initial population
cat("Generating initial population...\n")
initial_population <- generate_population(
  n_individuals = 8000000,
  ons_distributions = ons_distributions,
  hse_distributions = hse_distributions,
  seed = 1234
)
cat(sprintf("Initial population generated: %d individuals\n\n", nrow(initial_population)))

# Set parameters for 3 scenarios
baseline_params <- list(male_attendance_rate = 0.3803,
                        female_attendance_rate = 0.4396,
                        intervention_cost = 150,
                        bmi_reduction = -0.3,
                        sbp_reduction = -3.22,
                        smoking_cessation_rate = 0.0635)

intervention_params <- list(male_attendance_rate = 0.75, 
                            female_attendance_rate = 0.75, 
                            intervention_cost = 150, 
                            bmi_reduction = -0.3, 
                            sbp_reduction = -3.22, 
                            smoking_cessation_rate = 0.0635)

equalized_attendance_params <- list(male_attendance_rate = 0.4396,
                                    female_attendance_rate = 0.4396,
                                    intervention_cost = 150,
                                    bmi_reduction = -0.3,
                                    sbp_reduction = -3.22,
                                    smoking_cessation_rate = 0.0635)

# Run the simulation with 3 scenarios
monte_carlo_results <- run_monte_carlo(
  initial_population = initial_population,
  n_replications = 100,
  scenarios = c("baseline", "intervention", "equalized_attendance"),
  start_year = 2024,
  end_year = 2041,
  baseline_params = baseline_params,
  intervention_params = intervention_params,
  equalized_attendance_params = equalized_attendance_params,
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
  base_seed = 1234,
  verbose = FALSE
)

# Calculate and view results with confidence intervals
cat("\n=== CALCULATING CONFIDENCE INTERVALS ===\n")
ci_results <- calculate_scenario_confidence_intervals(monte_carlo_results)

# Print formatted results
print_scenario_confidence_intervals(ci_results)

# Save results to CSV
output_file <- save_scenario_confidence_intervals(ci_results)