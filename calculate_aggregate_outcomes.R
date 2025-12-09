#' Calculate Aggregate Annual Outcomes for Microsimulation with Sex and Disease Disaggregation
#' 
#' This function calculates aggregate life years (LYs), QALYs, costs, deaths, births,
#' disease incidence and prevalence for each year of the microsimulation cycle,
#' with disaggregation by sex and disease
#'
#' @param population Current population dataframe
#' @param current_year Current simulation year
#' @param age_sex_utilities Age-sex utilities dataframe
#' @param disease_utilities Disease utilities dataframe
#' @param costs Disease costs dataframe
#' @param previous_population Previous year's population (for calculating births/incidence)
#' @param utility_method Method for handling multiple diseases in utility calculation
#' @param intervention_costs Costs of the nhs health check
#' @return Named list with aggregate outcomes for the year, including sex and disease disaggregations
#' @export

calculate_aggregate_outcomes <- function(population, 
                                         current_year,
                                         age_sex_utilities,
                                         disease_utilities, 
                                         costs,
                                         previous_population = NULL,
                                         utility_method = "minimum",
                                         intervention_costs = 0) {
  
  
  # Clean utility data (remove BOM characters if present)
  age_sex_utilities <- age_sex_utilities %>%
    rename_with(~gsub("^[^A-Za-z]+", "", .x))
  
  disease_utilities <- disease_utilities %>%
    rename_with(~gsub("^[^A-Za-z]+", "", .x))
  
  costs <- costs %>%
    rename_with(~gsub("^[^A-Za-z]+", "", .x))
  
  # Clean cost data - remove commas and convert to numeric
  costs <- costs %>%
    mutate(Cost = as.numeric(gsub(",", "", Cost)))
  
  # Standardize disease and cost data
  age_sex_utilities <- age_sex_utilities %>%
    mutate(
      Sex = case_when(
        tolower(Sex) %in% c("female", "females", "women", "woman") ~ "Women",
        tolower(Sex) %in% c("male", "males", "men", "man") ~ "Men",
        TRUE ~ Sex
      )
    ) %>%
    rename(sex_label = Sex)
  
  disease_utilities <- disease_utilities %>%
    mutate(
      Disease = gsub("_", "", tolower(Disease)),
      Sex = case_when(
        tolower(Sex) %in% c("female", "females", "women", "woman") ~ "Women",
        tolower(Sex) %in% c("male", "males", "men", "man") ~ "Men",
        TRUE ~ Sex
      )
    ) %>%
    rename(sex_label = Sex)
  
  costs <- costs %>%
    mutate(Disease = gsub("_", "", tolower(Disease)))
  
  # Define diseases
  diseases <- c("chd", "stroke", "copd", "lung_cancer", "colorectal_cancer")
  
  # Calculate population demographics (overall)
  total_population <- nrow(population)
  alive_population <- sum(population$alive, na.rm = TRUE)
  dead_this_year <- sum(!population$alive & 
                          !is.na(population$death_year) & 
                          population$death_year == current_year, na.rm = TRUE)
  
  # Calculate births this year (new individuals)
  births_this_year <- 0
  if (!is.null(previous_population)) {
    new_ids <- setdiff(population$id, previous_population$id)
    births_this_year <- length(new_ids)
  }
  
  # Calculate sex-specific demographics
  sex_demographics <- population %>%
    group_by(sex_label) %>%
    summarise(
      total_pop = n(),
      alive_pop = sum(alive, na.rm = TRUE),
      deaths = sum(!alive & 
                     !is.na(death_year) & 
                     death_year == current_year, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Calculate sex-specific births
  sex_births <- tibble(sex_label = c("Men", "Women"), births = 0)
  if (!is.null(previous_population)) {
    new_births <- population %>%
      dplyr::filter(id %in% setdiff(population$id, previous_population$id)) %>%
      count(sex_label, name = "births")
    
    sex_births <- sex_births %>%
      left_join(new_births, by = "sex_label") %>%
      mutate(births = coalesce(births.y, births.x)) %>%
      dplyr::select(sex_label, births)
  }
  
  # Calculate life years (LYs) - overall and by sex
  life_years <- sum(population$alive, na.rm = TRUE) + (dead_this_year * 0.5)
  
  sex_life_years <- population %>%
    mutate(
      ly_contribution = case_when(
        alive == TRUE ~ 1,
        !alive & !is.na(death_year) & death_year == current_year ~ 0.5,
        TRUE ~ 0
      )
    ) %>%
    group_by(sex_label) %>%
    summarise(life_years = sum(ly_contribution, na.rm = TRUE), .groups = 'drop')
  
  # Calculate QALYs using the utility calculation logic
  population_with_utilities <- calculate_individual_utilities(
    population = population,
    age_sex_utilities = age_sex_utilities,
    disease_utilities = disease_utilities,
    method = utility_method
  )
  
  # Calculate total QALYs (overall and by sex)
  total_qalys <- sum(population_with_utilities$individual_qalys, na.rm = TRUE)
  
  sex_qalys <- population_with_utilities %>%
    group_by(sex_label) %>%
    summarise(qalys = sum(individual_qalys, na.rm = TRUE), .groups = 'drop')
  
  # Calculate costs (overall and by sex)
  cost_results <- calculate_disease_costs(
    population = population,
    costs = costs,
    diseases = diseases
  )
  
  sex_cost_results <- calculate_disease_costs_by_sex(
    population = population,
    costs = costs,
    diseases = diseases
  )
  
  # Calculate disease prevalence (proportion with disease among living) - overall and by sex
  living_pop <- population %>% dplyr::filter(alive == TRUE)
  prevalence_results <- list()
  sex_prevalence_results <- list()
  
  for (disease in diseases) {
    if (disease %in% names(living_pop)) {
      # Overall prevalence
      prevalence_results[[paste0(disease, "_prevalence")]] <- 
        sum(living_pop[[disease]], na.rm = TRUE) / nrow(living_pop) * 100
      
      # Sex-specific prevalence
      sex_prev <- living_pop %>%
        group_by(sex_label) %>%
        summarise(
          prevalence = sum(.data[[disease]], na.rm = TRUE) / n() * 100,
          .groups = 'drop'
        )
      
      for (sex in c("Men", "Women")) {
        prev_value <- sex_prev %>% dplyr::filter(sex_label == sex) %>% pull(prevalence)
        if (length(prev_value) == 0) prev_value <- 0
        sex_prevalence_results[[paste0(disease, "_prevalence_", tolower(sex))]] <- prev_value
      }
    } else {
      prevalence_results[[paste0(disease, "_prevalence")]] <- 0
      sex_prevalence_results[[paste0(disease, "_prevalence_men")]] <- 0
      sex_prevalence_results[[paste0(disease, "_prevalence_women")]] <- 0
    }
  }
  
  # Calculate disease incidence (new cases this year) - overall and by sex
  incidence_results <- list()
  sex_incidence_results <- list()
  
  for (disease in diseases) {
    incidence_year_col <- paste0(disease, "_incidence_year")
    
    if (incidence_year_col %in% names(population)) {
      # Overall incidence
      new_cases <- sum(!is.na(population[[incidence_year_col]]) & 
                         population[[incidence_year_col]] == current_year, na.rm = TRUE)
      incidence_results[[paste0(disease, "_incidence_count")]] <- new_cases
      incidence_results[[paste0(disease, "_incidence_rate")]] <- 
        ifelse(alive_population > 0, new_cases / alive_population * 1000, 0)
      
      # Sex-specific incidence
      sex_inc <- population %>%
        dplyr::filter(!is.na(.data[[incidence_year_col]]), .data[[incidence_year_col]] == current_year) %>%
        count(sex_label, name = "new_cases")
      
      sex_alive <- population %>%
        dplyr::filter(alive == TRUE) %>%
        count(sex_label, name = "alive_count")
      
      sex_inc_complete <- sex_alive %>%
        left_join(sex_inc, by = "sex_label") %>%
        mutate(new_cases = coalesce(new_cases, 0),
               incidence_rate = ifelse(alive_count > 0, new_cases / alive_count * 1000, 0))
      
      for (sex in c("Men", "Women")) {
        sex_data <- sex_inc_complete %>% dplyr::filter(sex_label == sex)
        if (nrow(sex_data) > 0) {
          sex_incidence_results[[paste0(disease, "_incidence_count_", tolower(sex))]] <- sex_data$new_cases[1]
          sex_incidence_results[[paste0(disease, "_incidence_rate_", tolower(sex))]] <- sex_data$incidence_rate[1]
        } else {
          sex_incidence_results[[paste0(disease, "_incidence_count_", tolower(sex))]] <- 0
          sex_incidence_results[[paste0(disease, "_incidence_rate_", tolower(sex))]] <- 0
        }
      }
    } else {
      incidence_results[[paste0(disease, "_incidence_count")]] <- 0
      incidence_results[[paste0(disease, "_incidence_rate")]] <- 0
      
      for (sex in c("men", "women")) {
        sex_incidence_results[[paste0(disease, "_incidence_count_", sex)]] <- 0
        sex_incidence_results[[paste0(disease, "_incidence_rate_", sex)]] <- 0
      }
    }
  }
  
  # Calculate deaths by cause (overall and by sex)
  deaths_by_cause <- list()
  sex_deaths_by_cause <- list()
  
  if ("cause_of_death" %in% names(population)) {
    death_data <- population %>% 
      dplyr::filter(!is.na(death_year), death_year == current_year) %>%
      dplyr::filter(!is.na(cause_of_death), cause_of_death != "", cause_of_death != "NA")
    
    # Overall deaths by cause
    death_causes <- death_data %>%
      count(cause_of_death, .drop = FALSE) %>%
      rename(count = n) %>%
      dplyr::filter(!is.na(cause_of_death))
    
    # Sex-specific deaths by cause
    sex_death_causes <- death_data %>%
      count(sex_label, cause_of_death, .drop = FALSE) %>%
      rename(count = n) %>%
      dplyr::filter(!is.na(cause_of_death))
    
    # Initialize all causes to 0
    all_causes <- c("other_cause", diseases)
    for (cause in all_causes) {
      deaths_by_cause[[paste0("deaths_", cause)]] <- 0
      sex_deaths_by_cause[[paste0("deaths_", cause, "_men")]] <- 0
      sex_deaths_by_cause[[paste0("deaths_", cause, "_women")]] <- 0
    }
    
    # Fill in actual counts - overall
    if (nrow(death_causes) > 0) {
      for (i in 1:nrow(death_causes)) {
        cause <- death_causes$cause_of_death[i]
        count <- death_causes$count[i]
        if (!is.na(cause) && cause != "") {
          deaths_by_cause[[paste0("deaths_", cause)]] <- count
        }
      }
    }
    
    # Fill in actual counts - by sex
    if (nrow(sex_death_causes) > 0) {
      for (i in 1:nrow(sex_death_causes)) {
        cause <- sex_death_causes$cause_of_death[i]
        sex <- tolower(sex_death_causes$sex_label[i])
        count <- sex_death_causes$count[i]
        
        if (!is.na(cause) && cause != "" && !is.na(sex)) {
          sex_deaths_by_cause[[paste0("deaths_", cause, "_", sex)]] <- count
        }
      }
    }
  } else {
    # Initialize to 0 if no death cause data
    for (cause in c("other_cause", diseases)) {
      deaths_by_cause[[paste0("deaths_", cause)]] <- 0
      sex_deaths_by_cause[[paste0("deaths_", cause, "_men")]] <- 0
      sex_deaths_by_cause[[paste0("deaths_", cause, "_women")]] <- 0
    }
  }
  
  # Create sex-specific demographic results
  sex_demo_results <- list()
  for (i in 1:nrow(sex_demographics)) {
    sex <- tolower(sex_demographics$sex_label[i])
    sex_demo_results[[paste0("total_population_", sex)]] <- sex_demographics$total_pop[i]
    sex_demo_results[[paste0("alive_population_", sex)]] <- sex_demographics$alive_pop[i]
    sex_demo_results[[paste0("deaths_total_", sex)]] <- sex_demographics$deaths[i]
  }
  
  # Add sex-specific births
  for (i in 1:nrow(sex_births)) {
    sex <- tolower(sex_births$sex_label[i])
    sex_demo_results[[paste0("births_", sex)]] <- sex_births$births[i]
  }
  
  # Add sex-specific life years and QALYs
  for (i in 1:nrow(sex_life_years)) {
    sex <- tolower(sex_life_years$sex_label[i])
    sex_demo_results[[paste0("life_years_", sex)]] <- sex_life_years$life_years[i]
  }
  
  for (i in 1:nrow(sex_qalys)) {
    sex <- tolower(sex_qalys$sex_label[i])
    sex_demo_results[[paste0("total_qalys_", sex)]] <- sex_qalys$qalys[i]
  }
  
  # Compile results
  outcomes <- list(
    # Basic demographics (overall)
    year = current_year,
    total_population = total_population,
    alive_population = alive_population,
    births = births_this_year,
    deaths_total = dead_this_year,
    
    # Health outcomes (overall)
    life_years = life_years,
    total_qalys = total_qalys,
    
    # Costs (overall)
    total_direct_costs = cost_results$total_direct_costs,
    total_indirect_costs = cost_results$total_indirect_costs,
    total_costs = cost_results$total_costs + intervention_costs,
    intervention_costs = intervention_costs
  )
  
  # Add sex-specific demographics and health outcomes
  outcomes <- c(outcomes, sex_demo_results)
  
  # Add disease-specific costs (overall)
  for (disease in diseases) {
    disease_clean <- gsub("_", "", disease)
    outcomes[[paste0(disease, "_direct_costs")]] <- cost_results[[paste0(disease_clean, "_direct_costs")]]
    outcomes[[paste0(disease, "_indirect_costs")]] <- cost_results[[paste0(disease_clean, "_indirect_costs")]]
  }
  
  # Add sex-specific disease costs
  outcomes <- c(outcomes, sex_cost_results)
  
  # Add prevalence and incidence (overall and by sex)
  outcomes <- c(outcomes, prevalence_results, incidence_results)
  outcomes <- c(outcomes, sex_prevalence_results, sex_incidence_results)
  
  # Add deaths by cause (overall and by sex)
  outcomes <- c(outcomes, deaths_by_cause, sex_deaths_by_cause)
  
  return(outcomes)
}


#' Calculate Disease Costs by Sex (Helper Function)
#'
#' Calculate total and disease-specific costs for the population disaggregated by sex
#'
#' @param population Population dataframe
#' @param costs Disease cost lookup table
#' @param diseases Vector of disease names
#' @return List with cost summaries by sex

calculate_disease_costs_by_sex <- function(population, costs, diseases) {
  
  sex_cost_results <- list()
  
  # Initialize sex-specific totals
  for (sex in c("men", "women")) {
    sex_cost_results[[paste0("total_direct_costs_", sex)]] <- 0
    sex_cost_results[[paste0("total_indirect_costs_", sex)]] <- 0
    sex_cost_results[[paste0("total_costs_", sex)]] <- 0
  }
  
  for (disease in diseases) {
    disease_clean <- gsub("_", "", disease)
    
    # Initialize disease-specific costs by sex
    for (sex in c("men", "women")) {
      sex_cost_results[[paste0(disease, "_direct_costs_", sex)]] <- 0
      sex_cost_results[[paste0(disease, "_indirect_costs_", sex)]] <- 0
    }
    
    if (disease %in% names(population)) {
      # Calculate costs by sex
      sex_disease_counts <- population %>%
        dplyr::filter(.data[[disease]] == TRUE & alive == TRUE) %>%
        count(sex_label, name = "disease_count")
      
      # Only process if there are disease cases
      if (nrow(sex_disease_counts) > 0) {
        for (i in 1:nrow(sex_disease_counts)) {
          sex_label <- sex_disease_counts$sex_label[i]
          
          # Handle NA or invalid sex_label values
          if (is.na(sex_label) || !tolower(sex_label) %in% c("men", "women")) {
            next  # Skip this iteration
          }
          
          sex_lower <- tolower(sex_label)
          disease_count <- sex_disease_counts$disease_count[i]
          
          # Handle NA values in disease_count
          if (is.na(disease_count)) {
            disease_count <- 0
          }
          
          if (disease_count > 0) {
            # Get direct costs
            direct_cost_per_person <- costs %>%
              dplyr::filter(tolower(Disease) == disease_clean, 
                     tolower(Cost_Category) == "direct") %>%
              pull(Cost)
            
            if (length(direct_cost_per_person) > 0) {
              direct_total <- direct_cost_per_person[1] * disease_count
              sex_cost_results[[paste0(disease, "_direct_costs_", sex_lower)]] <- direct_total
              sex_cost_results[[paste0("total_direct_costs_", sex_lower)]] <- 
                sex_cost_results[[paste0("total_direct_costs_", sex_lower)]] + direct_total
            }
            
            # Get indirect costs
            indirect_cost_per_person <- costs %>%
              dplyr::filter(tolower(Disease) == disease_clean,
                     tolower(Cost_Category) == "indirect") %>%
              pull(Cost)
            
            if (length(indirect_cost_per_person) > 0) {
              indirect_total <- indirect_cost_per_person[1] * disease_count
              sex_cost_results[[paste0(disease, "_indirect_costs_", sex_lower)]] <- indirect_total
              sex_cost_results[[paste0("total_indirect_costs_", sex_lower)]] <- 
                sex_cost_results[[paste0("total_indirect_costs_", sex_lower)]] + indirect_total
            }
          }
        }
      } # End if (nrow(sex_disease_counts) > 0)
    }
  }
  
  # Calculate total costs by sex
  for (sex in c("men", "women")) {
    sex_cost_results[[paste0("total_costs_", sex)]] <- 
      sex_cost_results[[paste0("total_direct_costs_", sex)]] + 
      sex_cost_results[[paste0("total_indirect_costs_", sex)]]
  }
  
  return(sex_cost_results)
}


#' Calculate Individual Utilities (Helper Function)
#'
#' Calculate utility scores for each individual for QALY calculation
#'
#' @param population Population dataframe
#' @param age_sex_utilities Age-sex utility lookup table
#' @param disease_utilities Disease utility lookup table  
#' @param method Method for handling multiple diseases
#' @return Population dataframe with individual_qalys column

calculate_individual_utilities <- function(population,
                                           age_sex_utilities,
                                           disease_utilities, 
                                           method = "minimum") {
  
  diseases <- c("chd", "stroke", "copd", "lung_cancer", "colorectal_cancer")
  
  # Create utility age (rounded down to nearest integer)
  pop_with_utilities <- population %>%
    mutate(utility_age = floor(age))
  
  # Join with age-sex utilities
  pop_with_utilities <- pop_with_utilities %>%
    left_join(age_sex_utilities, 
              by = c("utility_age" = "Age", "sex_label" = "sex_label")) %>%
    rename(baseline_utility = Utility)
  
  # Handle missing baseline utilities
  if (any(is.na(pop_with_utilities$baseline_utility))) {
    max_age_utilities <- age_sex_utilities %>%
      group_by(sex_label) %>%
      dplyr::filter(Age == max(Age)) %>%
      dplyr::select(sex_label, Utility) %>%
      rename(fallback_utility = Utility)
    
    pop_with_utilities <- pop_with_utilities %>%
      left_join(max_age_utilities, by = "sex_label") %>%
      mutate(baseline_utility = coalesce(baseline_utility, fallback_utility)) %>%
      dplyr::select(-fallback_utility)
  }
  
  # Initialize final utility with baseline
  pop_with_utilities$final_utility <- pop_with_utilities$baseline_utility
  
  # Apply disease utilities using minimum method
  if (method == "minimum") {
    disease_utility_cols <- c()
    
    # Create columns for each disease utility
    for (disease in diseases) {
      disease_clean <- gsub("_", "", disease)
      
      disease_utils <- disease_utilities %>%
        dplyr::filter(tolower(Disease) == disease_clean) %>%
        dplyr::select(sex_label, Utility) %>%
        rename(!!paste0(disease, "_utility") := Utility)
      
      pop_with_utilities <- pop_with_utilities %>%
        left_join(disease_utils, by = "sex_label")
      
      disease_utility_cols <- c(disease_utility_cols, paste0(disease, "_utility"))
    }
    
    # Calculate final utility using minimum
    for (i in 1:nrow(pop_with_utilities)) {
      applicable_utilities <- c(pop_with_utilities$baseline_utility[i])
      
      for (disease in diseases) {
        if (disease %in% names(pop_with_utilities) && 
            !is.na(pop_with_utilities[[disease]][i]) &&
            pop_with_utilities[[disease]][i] == TRUE) {
          disease_utility <- pop_with_utilities[[paste0(disease, "_utility")]][i]
          if (!is.na(disease_utility)) {
            applicable_utilities <- c(applicable_utilities, disease_utility)
          }
        }
      }
      
      pop_with_utilities$final_utility[i] <- min(applicable_utilities, na.rm = TRUE)
    }
    
    # Clean up temporary columns
    pop_with_utilities <- pop_with_utilities %>%
      dplyr::select(-all_of(disease_utility_cols))
  }
  
  # Calculate individual QALYs
  pop_with_utilities <- pop_with_utilities %>%
    mutate(
      individual_qalys = case_when(
        alive == TRUE ~ final_utility * 1,  # Full year
        alive == FALSE ~ final_utility * 0.5,  # Mid-year death
        TRUE ~ 0
      )
    ) %>%
    dplyr::select(-utility_age)
  
  return(pop_with_utilities)
}


#' Calculate Disease Costs (Helper Function)
#'
#' Calculate total and disease-specific costs for the population
#'
#' @param population Population dataframe
#' @param costs Disease cost lookup table
#' @param diseases Vector of disease names
#' @return List with cost summaries

calculate_disease_costs <- function(population, costs, diseases) {
  
  total_direct_costs <- 0
  total_indirect_costs <- 0
  disease_costs <- list()
  
  for (disease in diseases) {
    disease_clean <- gsub("_", "", disease)
    disease_costs[[paste0(disease_clean, "_direct_costs")]] <- 0
    disease_costs[[paste0(disease_clean, "_indirect_costs")]] <- 0
    
    if (disease %in% names(population)) {
      # Count individuals with this disease who are alive
      disease_count <- sum(population[[disease]] == TRUE & population$alive == TRUE, na.rm = TRUE)
      
      if (disease_count > 0) {
        # Get direct costs
        direct_cost_per_person <- costs %>%
          dplyr::filter(tolower(Disease) == disease_clean, 
                 tolower(Cost_Category) == "direct") %>%
          pull(Cost)
        
        if (length(direct_cost_per_person) > 0) {
          direct_total <- direct_cost_per_person[1] * disease_count
          disease_costs[[paste0(disease_clean, "_direct_costs")]] <- direct_total
          total_direct_costs <- total_direct_costs + direct_total
        }
        
        # Get indirect costs
        indirect_cost_per_person <- costs %>%
          dplyr::filter(tolower(Disease) == disease_clean,
                 tolower(Cost_Category) == "indirect") %>%
          pull(Cost)
        
        if (length(indirect_cost_per_person) > 0) {
          indirect_total <- indirect_cost_per_person[1] * disease_count
          disease_costs[[paste0(disease_clean, "_indirect_costs")]] <- indirect_total
          total_indirect_costs <- total_indirect_costs + indirect_total
        }
      }
    }
  }
  
  disease_costs$total_direct_costs <- total_direct_costs
  disease_costs$total_indirect_costs <- total_indirect_costs
  disease_costs$total_costs <- total_direct_costs + total_indirect_costs
  
  return(disease_costs)
}


#' Initialize Model Outputs Dataframe with Sex and Disease Disaggregation
#'
#' Create empty model_outputs dataframe with proper column structure including disaggregated columns
#'
#' @param diseases Vector of disease names
#' @return Empty dataframe with all required columns including sex and disease disaggregations

initialize_model_outputs <- function(diseases = c("chd", "stroke", "copd", "lung_cancer", "colorectal_cancer")) {
  
  # Base columns (overall)
  base_cols <- c("year", "total_population", "alive_population", "births", "deaths_total", 
                 "life_years", "total_qalys", "total_direct_costs", "total_indirect_costs", 
                 "total_costs", "intervention_costs",
                 "health_check_eligible", "health_check_attendees")
  
  # Sex-specific demographic columns
  sex_demo_cols <- c("total_population_men", "total_population_women",
                     "alive_population_men", "alive_population_women",
                     "births_men", "births_women",
                     "deaths_total_men", "deaths_total_women",
                     "life_years_men", "life_years_women",
                     "total_qalys_men", "total_qalys_women")
  
  # Sex-specific cost columns
  sex_cost_cols <- c("total_direct_costs_men", "total_direct_costs_women",
                     "total_indirect_costs_men", "total_indirect_costs_women",
                     "total_costs_men", "total_costs_women")
  
  # Disease-specific cost columns (overall)
  disease_cost_cols <- c()
  for (disease in diseases) {
    disease_cost_cols <- c(disease_cost_cols, 
                           paste0(disease, "_direct_costs"),
                           paste0(disease, "_indirect_costs"))
  }
  
  # Disease-specific cost columns (by sex)
  disease_sex_cost_cols <- c()
  for (disease in diseases) {
    disease_sex_cost_cols <- c(disease_sex_cost_cols,
                               paste0(disease, "_direct_costs_men"),
                               paste0(disease, "_direct_costs_women"),
                               paste0(disease, "_indirect_costs_men"),
                               paste0(disease, "_indirect_costs_women"))
  }
  
  # Disease prevalence columns (overall and by sex)
  prevalence_cols <- paste0(diseases, "_prevalence")
  prevalence_sex_cols <- c(paste0(diseases, "_prevalence_men"),
                           paste0(diseases, "_prevalence_women"))
  
  # Disease incidence columns (overall and by sex)
  incidence_cols <- c(paste0(diseases, "_incidence_count"),
                      paste0(diseases, "_incidence_rate"))
  incidence_sex_cols <- c(paste0(diseases, "_incidence_count_men"),
                          paste0(diseases, "_incidence_count_women"),
                          paste0(diseases, "_incidence_rate_men"),
                          paste0(diseases, "_incidence_rate_women"))
  
  # Death cause columns (overall and by sex)
  death_cols <- c("deaths_other_cause", paste0("deaths_", diseases))
  death_sex_cols <- c("deaths_other_cause_men", "deaths_other_cause_women",
                      paste0("deaths_", diseases, "_men"),
                      paste0("deaths_", diseases, "_women"))
  
  # Combine all columns
  all_cols <- c(base_cols, sex_demo_cols, sex_cost_cols, disease_cost_cols, disease_sex_cost_cols,
                prevalence_cols, prevalence_sex_cols, incidence_cols, incidence_sex_cols, 
                death_cols, death_sex_cols)
  
  # Create empty dataframe
  model_outputs <- data.frame(matrix(ncol = length(all_cols), nrow = 0))
  colnames(model_outputs) <- all_cols
  
  return(model_outputs)
}


#' Add Annual Outcomes to Model Outputs
#'
#' Add a year's worth of outcomes to the model_outputs dataframe
#'
#' @param model_outputs Existing model outputs dataframe
#' @param annual_outcomes List of annual outcomes from calculate_aggregate_outcomes
#' @return Updated model_outputs dataframe

add_annual_outcomes <- function(model_outputs, annual_outcomes) {
  
  # Convert list to dataframe row
  new_row <- as.data.frame(annual_outcomes)
  
  # If model_outputs is empty, just return the new row
  if (nrow(model_outputs) == 0) {
    return(new_row)
  }
  
  # Ensure all columns exist in model_outputs
  missing_cols <- setdiff(names(new_row), names(model_outputs))
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      model_outputs[[col]] <- NA
    }
  }
  
  # Ensure all columns exist in new_row
  missing_cols_new <- setdiff(names(model_outputs), names(new_row))
  if (length(missing_cols_new) > 0) {
    for (col in missing_cols_new) {
      new_row[[col]] <- NA
    }
  }
  
  # Reorder columns to match
  new_row <- new_row[names(model_outputs)]
  
  # Bind the new row
  model_outputs <- rbind(model_outputs, new_row)
  
  return(model_outputs)
}


#' Generate Summary Statistics by Sex and Disease
#'
#' Generate summary statistics from the disaggregated model outputs
#'
#' @param model_outputs Model outputs dataframe with disaggregated results
#' @param diseases Vector of disease names
#' @return List containing various summary statistics
#' @export

generate_summary_statistics <- function(model_outputs, diseases = c("chd", "stroke", "copd", "lung_cancer", "colorectal_cancer")) {
  
  if (nrow(model_outputs) == 0) {
    warning("No data in model_outputs to summarize")
    return(list())
  }
  
  # Overall summaries
  total_summary <- model_outputs %>%
    summarise(
      total_life_years = sum(life_years, na.rm = TRUE),
      total_qalys = sum(total_qalys, na.rm = TRUE),
      total_costs = sum(total_costs, na.rm = TRUE),
      total_deaths = sum(deaths_total, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Sex-specific summaries
  sex_summary <- model_outputs %>%
    summarise(
      life_years_men = sum(life_years_men, na.rm = TRUE),
      life_years_women = sum(life_years_women, na.rm = TRUE),
      qalys_men = sum(total_qalys_men, na.rm = TRUE),
      qalys_women = sum(total_qalys_women, na.rm = TRUE),
      costs_men = sum(total_costs_men, na.rm = TRUE),
      costs_women = sum(total_costs_women, na.rm = TRUE),
      deaths_men = sum(deaths_total_men, na.rm = TRUE),
      deaths_women = sum(deaths_total_women, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Disease-specific summaries
  disease_summaries <- list()
  for (disease in diseases) {
    disease_summaries[[disease]] <- model_outputs %>%
      summarise(
        total_incidence = sum(.data[[paste0(disease, "_incidence_count")]], na.rm = TRUE),
        incidence_men = sum(.data[[paste0(disease, "_incidence_count_men")]], na.rm = TRUE),
        incidence_women = sum(.data[[paste0(disease, "_incidence_count_women")]], na.rm = TRUE),
        total_costs = sum(.data[[paste0(disease, "_direct_costs")]] + .data[[paste0(disease, "_indirect_costs")]], na.rm = TRUE),
        costs_men = sum(.data[[paste0(disease, "_direct_costs_men")]] + .data[[paste0(disease, "_indirect_costs_men")]], na.rm = TRUE),
        costs_women = sum(.data[[paste0(disease, "_direct_costs_women")]] + .data[[paste0(disease, "_indirect_costs_women")]], na.rm = TRUE),
        total_deaths = sum(.data[[paste0("deaths_", disease)]], na.rm = TRUE),
        deaths_men = sum(.data[[paste0("deaths_", disease, "_men")]], na.rm = TRUE),
        deaths_women = sum(.data[[paste0("deaths_", disease, "_women")]], na.rm = TRUE),
        .groups = 'drop'
      )
  }
  
  return(list(
    total_summary = total_summary,
    sex_summary = sex_summary,
    disease_summaries = disease_summaries
  ))
}

#' Calculate Annual Intervention Costs
#'
#' Calculates total intervention costs for the current year
#'
#' @param population Current population dataframe
#' @param current_year Current simulation year
#' @param nhs_params NHS Health Check parameters
#' @return Total intervention costs for the year
calculate_annual_intervention_costs <- function(population, current_year, nhs_params) {
  # Count NHS Health Check screenings in this year
  nhs_screenings_this_year <- population %>%
    dplyr::filter(!is.na(nhs_last_health_check_year) & nhs_last_health_check_year == current_year) %>%
    nrow()
  
  # Calculate total costs
  total_intervention_costs <- nhs_screenings_this_year * nhs_params$intervention_cost
  
  return(total_intervention_costs)
}