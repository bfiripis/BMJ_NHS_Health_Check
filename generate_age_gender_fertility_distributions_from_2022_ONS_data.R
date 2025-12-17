generate_ons_distributions <- function(ons_file_path = "data/raw/ONS_Zipped_population_projections_data_files_England/en_ppp_machine_readable.xlsx",
                                       reference_year = "2022", 
                                       births_year = "2022 - 2023") {
  
  # Load ONS data
  ons_data <- list(
    population = readxl::read_excel(ons_file_path, sheet = "Population"),
    fertility_data = readxl::read_excel(ons_file_path, sheet = "Fertility_assumptions"),
    mortality_data = readxl::read_excel(ons_file_path, sheet = "Mortality_assumptions"),
    births_data = readxl::read_excel(ons_file_path, sheet = "Births"),
    deaths_data = readxl::read_excel(ons_file_path, sheet = "Deaths")
  )
  
  # Clean the raw population data
  ons_data_clean <- ons_data$population %>%
    # Convert Sex to consistent labels
    mutate(
      sex_label = case_when(
        Sex == "Females" ~ "Women",
        Sex == "Males" ~ "Men", 
        TRUE ~ Sex
      )
    ) %>%
    # Select relevant columns
    dplyr::select(sex_label, Age, all_of(reference_year)) %>%
    rename(
      age = Age,
      population = !!sym(reference_year)
    ) %>%
    # Filter out missing values
    dplyr::filter(!is.na(population), !is.na(age)) %>%
    # Clean age column to handle ranges
    mutate(
      age = case_when(
        as.character(age) == "105 - 109" ~ 107,  # Use midpoint
        as.character(age) == "110 and over" ~ 110,  # Use lower bound
        TRUE ~ as.numeric(age)
      )
    ) %>%
    dplyr::filter(!is.na(age))
  
  # Calculate age distributions
  age_distributions <- ons_data_clean %>%
    group_by(sex_label) %>%
    mutate(
      total_pop = sum(population),
      prop_age = population / total_pop
    ) %>%
    ungroup() %>%
    dplyr::select(sex_label, age, population, prop_age)
  
  # Calculate cumulative distributions for inverse transform sampling
  age_distributions_cumulative <- age_distributions %>%
    group_by(sex_label) %>%
    arrange(age) %>%
    mutate(
      cumulative_prop = cumsum(prop_age)
    ) %>%
    ungroup()
  
  # Calculate gender distributions
  gender_discrete <- ons_data_clean %>%
    group_by(sex_label) %>%
    summarise(
      total_pop = sum(population),
      .groups = 'drop'
    ) %>%
    mutate(
      prop_sex = total_pop / sum(total_pop)
    )
  
  gender_cumulative <- gender_discrete %>%
    arrange(sex_label) %>%
    mutate(
      cumulative_prop = cumsum(prop_sex)
    )
  
  # Calculate fertility parameters from ONS births data
  births_female <- ons_data$births_data %>%
    dplyr::filter(Sex == "Females") %>%
    dplyr::select(Age, births = all_of(births_year)) %>%
    mutate(age_numeric = as.numeric(Age)) %>%
    dplyr::filter(!is.na(births), births > 0)
  
  pop_female <- ons_data$population %>%
    dplyr::filter(Sex == "Females") %>%
    dplyr::select(Age, population = all_of(reference_year)) %>%
    mutate(
      age_numeric = case_when(
        as.character(Age) == "105 - 109" ~ 107,  # Use midpoint
        as.character(Age) == "110 and over" ~ 110,  # Use lower bound
        TRUE ~ as.numeric(Age)
      )
    ) %>%
    dplyr::filter(age_numeric >= 15, age_numeric <= 49, !is.na(population))
  
  fertility_data <- births_female %>%
    left_join(pop_female, by = "age_numeric") %>%
    dplyr::filter(!is.na(population), population > 0) %>%
    mutate(
      asfr = births / population,
      total_births = sum(births, na.rm = TRUE)
    ) %>%
    mutate(
      p_birth_given_age = births / total_births
    )
  
  tfr <- sum(fertility_data$asfr, na.rm = TRUE)
  
  # Calculate summary statistics for each sex
  summary_stats <- ons_data_clean %>%
    group_by(sex_label) %>%
    summarise(
      min_age = min(age),
      max_age = max(age),
      mean_age = weighted.mean(age, population),
      weighted_var = sum(population * (age - weighted.mean(age, population))^2) / sum(population),
      sd_age = sqrt(weighted_var),
      total_population = sum(population),
      .groups = 'drop'
    ) %>%
    dplyr::select(-weighted_var)
  
  # Create and return ons_distributions
  ons_distributions <- list(
    data_info = list(
      reference_year = reference_year,
      births_year = births_year,
      total_population = sum(ons_data_clean$population),
      age_range = c(min(ons_data_clean$age), max(ons_data_clean$age)),
      n_age_groups = length(unique(ons_data_clean$age)),
      sex_groups = unique(ons_data_clean$sex_label)
    ),
    
    age_distributions = list(
      discrete = age_distributions,
      cumulative = age_distributions_cumulative
    ),
    
    gender_distributions = list(
      discrete = gender_discrete,
      cumulative = gender_cumulative
    ),
    
    fertility_parameters = list(
      tfr = tfr,
      fertility_data = fertility_data,
      total_births = sum(fertility_data$births, na.rm = TRUE),
      asfr_by_age = fertility_data %>% 
        dplyr::select(age_numeric, asfr) %>%
        arrange(age_numeric)
    ),
    
    summary_statistics = summary_stats,
    
    metadata = list(
      source = "ONS Population Projections and Births Data",
      description = "Age, gender, and fertility distributions for microsimulation sampling",
      variables = list(
        age = "Age in single years (ranges converted to midpoint/lower bound)",
        sex_label = "Sex (Men/Women)",
        population = paste("Population count for", reference_year),
        prop_age = "Proportion of population in each age group by sex",
        cumulative_prop = "Cumulative proportion for inverse transform sampling",
        asfr = "Age-specific fertility rates",
        tfr = "Total fertility rate"
      )
    )
  )
  
  return(ons_distributions)
}

# Example usage
# ons_distributions <- generate_ons_distributions()