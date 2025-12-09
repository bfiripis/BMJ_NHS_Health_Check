library(dplyr)
library(readr)
library(readxl)
library(stringr)

#' Calculate annual post-incidence mortality rates by disease, age and sex
#' 
#' @param mortality_data Data frame with mortality data (default: NULL, will read from file)
#' @param prevalence_data Data frame with prevalence data (default: NULL, will read from file)
#' @param ons_file Path to ONS population projections Excel file (default: "data/raw/ONS_Zipped_population_projections_data_files_England/en_ppp_machine_readable.xlsx")
#' @param reference_year Year for ONS mortality data (default "2022")
#' @return Data frame with annual case fatality rates and competing mortality
calculate_post_incidence_mortality <- function(mortality_data = NULL,
                                               prevalence_data = NULL,
                                               ons_file = "data/raw/ONS_Zipped_population_projections_data_files_England/en_ppp_machine_readable.xlsx",
                                               reference_year = "2022") {
  
  # Load or use mortality data
  if (is.null(mortality_data)) {
    mortality_data <- read_csv("data/mortality.csv", show_col_types = FALSE)
  }
  
  mortality_data <- mortality_data %>%
    dplyr::filter(measure == "mortality") %>%
    mutate(sex_label = str_to_title(sex)) %>%
    dplyr::select(disease, age, sex_label, mortality_per_100k = rate_per_100k)
  
  # Load or use prevalence data
  if (is.null(prevalence_data)) {
    prevalence_data <- read_csv("data/prevalence.csv", show_col_types = FALSE)
  }
  
  prevalence_data <- prevalence_data %>%
    dplyr::filter(measure == "prevalence") %>%
    mutate(sex_label = str_to_title(sex)) %>%
    dplyr::select(disease, age, sex_label, prevalence_per_100k = rate_per_100k)
  
  # Validate data completeness
  cat("Mortality data diseases:", unique(mortality_data$disease), "\n")
  cat("Prevalence data diseases:", unique(prevalence_data$disease), "\n")
  cat("Age groups in mortality:", length(unique(mortality_data$age)), "\n")
  cat("Age groups in prevalence:", length(unique(prevalence_data$age)), "\n")
  
  # Calculate disease-specific annual case fatality rates: λ = M / P
  results <- mortality_data %>%
    inner_join(prevalence_data, by = c("disease", "age", "sex_label")) %>%
    mutate(
      annual_disease_mortality_prob = ifelse(
        prevalence_per_100k == 0,
        NA,  # Return NA when prevalence is 0 to avoid Inf
        mortality_per_100k / prevalence_per_100k
      ),
      annual_disease_mortality_prob_percent = annual_disease_mortality_prob * 100
    )
  
  # Add all-cause mortality from ONS data
  # Read ONS all-cause mortality data
  ons_mortality <- read_excel(ons_file, sheet = "Mortality_assumptions") %>%
    dplyr::select(Sex, Age, all_of(paste0(reference_year, " - ", as.numeric(reference_year) + 1))) %>%
    rename(
      sex_label = Sex,
      age_numeric = Age,
      all_cause_mortality_per_100k = paste0(reference_year, " - ", as.numeric(reference_year) + 1)
    ) %>%
    mutate(
      sex_label = case_when(
        sex_label == "Females" ~ "Female",
        sex_label == "Males" ~ "Male",
        TRUE ~ sex_label
      ),
      # Convert continuous age to numeric and then to age groups
      age_numeric = as.numeric(ifelse(age_numeric == "Birth", 0, age_numeric)),
      age_group = case_when(
        age_numeric == 0 ~ "0 years",
        age_numeric >= 1 & age_numeric <= 4 ~ "1 - 4 years",
        age_numeric >= 5 & age_numeric <= 9 ~ "5 - 9 years",
        age_numeric >= 10 & age_numeric <= 14 ~ "10 - 14 years",
        age_numeric >= 15 & age_numeric <= 19 ~ "15 - 19 years",
        age_numeric >= 20 & age_numeric <= 24 ~ "20 - 24 years",
        age_numeric >= 25 & age_numeric <= 29 ~ "25 - 29 years",
        age_numeric >= 30 & age_numeric <= 34 ~ "30 - 34 years",
        age_numeric >= 35 & age_numeric <= 39 ~ "35 - 39 years",
        age_numeric >= 40 & age_numeric <= 44 ~ "40 - 44 years",
        age_numeric >= 45 & age_numeric <= 49 ~ "45 - 49 years",
        age_numeric >= 50 & age_numeric <= 54 ~ "50 - 54 years",
        age_numeric >= 55 & age_numeric <= 59 ~ "55 - 59 years",
        age_numeric >= 60 & age_numeric <= 64 ~ "60 - 64 years",
        age_numeric >= 65 & age_numeric <= 69 ~ "65 - 69 years",
        age_numeric >= 70 & age_numeric <= 74 ~ "70 - 74 years",
        age_numeric >= 75 & age_numeric <= 79 ~ "75 - 79 years",
        age_numeric >= 80 & age_numeric <= 84 ~ "80 - 84 years",
        age_numeric >= 85 & age_numeric <= 89 ~ "85 - 89 years",
        age_numeric >= 90 & age_numeric <= 94 ~ "90 - 94 years",
        age_numeric >= 95 ~ "95+ years",
        TRUE ~ as.character(age_numeric)
      ),
      # Convert rate per 100,000 to annual probability (divide by 100,000)
      all_cause_annual_prob = as.numeric(all_cause_mortality_per_100k) / 100000
    ) %>%
    dplyr::filter(!is.na(all_cause_mortality_per_100k) & !is.na(age_group)) %>%
    group_by(sex_label, age_group) %>%
    summarise(
      all_cause_annual_prob = mean(all_cause_annual_prob, na.rm = TRUE),
      all_cause_mortality_per_100k = mean(as.numeric(all_cause_mortality_per_100k), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    rename(age = age_group)
  
  # Join with results and calculate competing mortality
  results <- results %>%
    left_join(ons_mortality, by = c("age", "sex_label")) %>%
    mutate(
      # Other-cause mortality = all-cause minus disease-specific (both in same units)
      other_cause_mortality_per_100k = pmax(0, all_cause_mortality_per_100k - mortality_per_100k),
      other_cause_annual_prob = other_cause_mortality_per_100k / 100000,
      other_cause_annual_percent = other_cause_annual_prob * 100,
      all_cause_annual_percent = all_cause_annual_prob * 100
    )
  
  # Validate results
  cat("\nResults summary:\n")
  cat("Final diseases:", unique(results$disease), "\n")
  cat("Age groups:", length(unique(results$age)), "\n")
  cat("Records with NA disease mortality:", sum(is.na(results$annual_disease_mortality_prob)), "\n")
  cat("Records with zero prevalence:", sum(results$prevalence_per_100k == 0, na.rm = TRUE), "\n")
  
  # Return results with all mortality components
  results %>%
    dplyr::select(disease, age, sex_label, prevalence_per_100k, mortality_per_100k,
                  annual_disease_mortality_prob, annual_disease_mortality_prob_percent,
                  all_cause_mortality_per_100k, all_cause_annual_prob, all_cause_annual_percent,
                  other_cause_mortality_per_100k, other_cause_annual_prob, other_cause_annual_percent)
}

# Example usage:
# mortality_probabilities <- calculate_post_incidence_mortality()