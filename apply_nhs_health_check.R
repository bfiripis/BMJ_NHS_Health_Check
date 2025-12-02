#' Apply NHS Health Check Intervention
#'
#' Implements the NHS Health Check intervention for eligible individuals in the population.
#' Determines attendance based on configurable rates, applies risk factor modifications,
#' and tracks intervention costs.
#'
#' @param population Population dataframe with individual health data
#' @param current_year Current simulation year
#' @param scenario_type Character: "baseline" or "intervention" 
#' @param male_attendance_rate Numeric: Proportion of eligible males who attend (0-1)
#' @param female_attendance_rate Numeric: Proportion of eligible females who attend (0-1)
#' @param intervention_cost Numeric: Cost per health check attendance (£)
#' @param bmi_reduction Numeric: BMI reduction in kg/m² for obese attendees (default: -0.3)
#' @param sbp_reduction Numeric: SBP reduction in mmHg for hypertensive attendees (default: -3.22)
#' @param smoking_cessation_rate Numeric: Probability of smoking cessation for current smokers (0-1, default: 0.0635)
#' @param seed Numeric: Random seed for reproducibility
#'
#' @return List containing:
#'   - population: Updated population dataframe
#'   - intervention_costs: Total costs incurred this year
#'   - attendance_summary: Summary of attendance by demographics
#'
#' @examples
#' \dontrun{
#' # Baseline scenario (lower attendance)
#' result_baseline <- apply_nhs_health_check(
#'   population = population,
#'   current_year = 2025,
#'   scenario_type = "baseline",
#'   male_attendance_rate = 0.3803,
#'   female_attendance_rate = 0.4396,
#'   intervention_cost = 150,
#'   seed = 9001
#' )
#' 
#' # Intervention scenario (higher attendance) 
#' result_intervention <- apply_nhs_health_check(
#'   population = population,
#'   current_year = 2025,
#'   scenario_type = "intervention", 
#'   male_attendance_rate = 0.75,
#'   female_attendance_rate = 0.75,
#'   intervention_cost = 150,
#'   seed = 9001
#' )
#' }
#'
#' @export
apply_nhs_health_check <- function(population,
                                   current_year,
                                   scenario_type = "baseline",
                                   male_attendance_rate = 0.3803,
                                   female_attendance_rate = 0.4396, 
                                   intervention_cost = 150,
                                   bmi_reduction = -0.3,
                                   sbp_reduction = -3.22,
                                   smoking_cessation_rate = 0.0635,
                                   seed = NULL) {
  
  # Set random seed if provided
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Initialize tracking columns if they don't exist
  if (!"health_check_attended" %in% names(population)) {
    population$health_check_attended <- FALSE
  }
  if (!"last_health_check_year" %in% names(population)) {
    population$last_health_check_year <- NA_integer_
  }
  if (!"health_check_attendance_history" %in% names(population)) {
    population$health_check_attendance_history <- ""
  }
  
  # Create working copy
  pop_updated <- population
  
  # Identify eligible individuals
  eligible_individuals <- identify_eligible_individuals(pop_updated, current_year)
  
  if (nrow(eligible_individuals) == 0) {
    # No eligible individuals - return unchanged population
    return(list(
      population = pop_updated,
      intervention_costs = 0,
      attendance_summary = data.frame(
        eligible_males = 0, attending_males = 0,
        eligible_females = 0, attending_females = 0,
        total_eligible = 0, total_attending = 0,
        total_costs = 0
      )
    ))
  }
  
  # Determine attendance for each eligible individual
  attendance_results <- determine_attendance(
    eligible_individuals = eligible_individuals,
    male_attendance_rate = male_attendance_rate,
    female_attendance_rate = female_attendance_rate
  )
  
  attendees <- attendance_results$attendees
  attendance_summary <- attendance_results$summary
  
  # Apply risk factor modifications to attendees
  if (nrow(attendees) > 0) {
    attendees_modified <- apply_risk_factor_modifications(
      attendees = attendees,
      bmi_reduction = bmi_reduction,
      sbp_reduction = sbp_reduction, 
      smoking_cessation_rate = smoking_cessation_rate
    )
    
    # Update population with modified risk factors and attendance records
    pop_updated <- update_population_with_attendees(
      population = pop_updated,
      attendees_modified = attendees_modified,
      current_year = current_year
    )
  }
  
  # Calculate intervention costs
  total_intervention_costs <- nrow(attendees) * intervention_cost
  attendance_summary$total_costs <- total_intervention_costs
  
  # Return results
  return(list(
    population = pop_updated,
    intervention_costs = total_intervention_costs,
    attendance_summary = attendance_summary
  ))
}


#' Identify Eligible Individuals for NHS Health Check
#'
#' @param population Population dataframe
#' @param current_year Current simulation year
#' @return Dataframe of eligible individuals
identify_eligible_individuals <- function(population, current_year) {
  
  eligible <- population %>%
    filter(
      # Must be alive
      alive == TRUE,
      # Age 40-75 years
      age >= 40 & age <= 75,
      # No prior CHD or stroke (at time of assessment)
      (is.na(chd) | chd == FALSE | is.na(chd_incidence_year) | chd_incidence_year > current_year) &
        (is.na(stroke) | stroke == FALSE | is.na(stroke_incidence_year) | stroke_incidence_year > current_year),
      # Must not have attended in last 5 years
      (is.na(last_health_check_year) | (current_year - last_health_check_year) >= 5)
    )
  
  return(eligible)
}


#' Determine Health Check Attendance
#'
#' @param eligible_individuals Dataframe of eligible individuals
#' @param male_attendance_rate Attendance rate for males
#' @param female_attendance_rate Attendance rate for females  
#' @return List with attendees dataframe and attendance summary
determine_attendance <- function(eligible_individuals,
                                 male_attendance_rate,
                                 female_attendance_rate) {
  
  # Count eligible by sex
  eligible_males <- sum(eligible_individuals$sex_label == "Men", na.rm = TRUE)
  eligible_females <- sum(eligible_individuals$sex_label == "Women", na.rm = TRUE)
  
  # Determine attendance for each individual
  eligible_individuals$attends_health_check <- FALSE
  
  # Males
  male_indices <- which(eligible_individuals$sex_label == "Men")
  if (length(male_indices) > 0) {
    male_attendance <- runif(length(male_indices)) < male_attendance_rate
    eligible_individuals$attends_health_check[male_indices] <- male_attendance
  }
  
  # Females  
  female_indices <- which(eligible_individuals$sex_label == "Women")
  if (length(female_indices) > 0) {
    female_attendance <- runif(length(female_indices)) < female_attendance_rate
    eligible_individuals$attends_health_check[female_indices] <- female_attendance
  }
  
  # Filter to attendees
  attendees <- eligible_individuals %>%
    filter(attends_health_check == TRUE) %>%
    select(-attends_health_check)
  
  # Create summary
  attending_males <- sum(attendees$sex_label == "Men", na.rm = TRUE)
  attending_females <- sum(attendees$sex_label == "Women", na.rm = TRUE)
  
  summary <- data.frame(
    eligible_males = eligible_males,
    attending_males = attending_males,
    eligible_females = eligible_females, 
    attending_females = attending_females,
    total_eligible = nrow(eligible_individuals),
    total_attending = nrow(attendees)
  )
  
  return(list(
    attendees = attendees,
    summary = summary
  ))
}


#' Apply Risk Factor Modifications from Health Check
#'
#' @param attendees Dataframe of individuals attending health check
#' @param bmi_reduction BMI reduction for obese individuals
#' @param sbp_reduction SBP reduction for hypertensive individuals
#' @param smoking_cessation_rate Probability of smoking cessation
#' @return Modified attendees dataframe
apply_risk_factor_modifications <- function(attendees,
                                            bmi_reduction,
                                            sbp_reduction,
                                            smoking_cessation_rate) {
  
  attendees_modified <- attendees
  
  # BMI reduction for obese individuals (BMI >= 30)
  obese_mask <- !is.na(attendees_modified$bmi) & attendees_modified$bmi >= 30
  if (any(obese_mask)) {
    attendees_modified$bmi[obese_mask] <- attendees_modified$bmi[obese_mask] + bmi_reduction
    
    # Update BMI category based on new BMI
    attendees_modified <- update_bmi_categories(attendees_modified)
  }
  
  # SBP reduction for hypertensive individuals  
  # Assume hypertensive = SBP >= 140 mmHg (adjust threshold as needed)
  hypertensive_mask <- !is.na(attendees_modified$sbp) & attendees_modified$sbp >= 140
  if (any(hypertensive_mask)) {
    attendees_modified$sbp[hypertensive_mask] <- attendees_modified$sbp[hypertensive_mask] + sbp_reduction
    
    # Update SBP category based on new SBP
    attendees_modified <- update_sbp_categories(attendees_modified)
  }
  
  # Smoking cessation for current smokers
  current_smokers <- which(!is.na(attendees_modified$smoking_status) & 
                             attendees_modified$smoking_status == "current_smoker")
  
  if (length(current_smokers) > 0) {
    # Determine who quits smoking
    quit_smoking <- runif(length(current_smokers)) < smoking_cessation_rate
    
    # Update smoking status for those who quit
    attendees_modified$smoking_status[current_smokers[quit_smoking]] <- "ex_smoker"
  }
  
  return(attendees_modified)
}


#' Update BMI Categories
#'
#' @param data Dataframe with BMI values
#' @return Dataframe with updated BMI categories
update_bmi_categories <- function(data) {
  
  if ("bmi_category" %in% names(data)) {
    data$bmi_category <- case_when(
      is.na(data$bmi) ~ NA_character_,
      data$bmi < 18.5 ~ "underweight", 
      data$bmi < 25 ~ "normal_weight",
      data$bmi < 30 ~ "overweight",
      data$bmi >= 30 ~ "obese",
      TRUE ~ NA_character_
    )
  }
  
  return(data)
}


#' Update SBP Categories  
#'
#' @param data Dataframe with SBP values
#' @return Dataframe with updated SBP categories
update_sbp_categories <- function(data) {
  
  if ("sbp_category" %in% names(data)) {
    data$sbp_category <- case_when(
      is.na(data$sbp) ~ NA_character_,
      data$sbp < 120 ~ "normal",
      data$sbp < 140 ~ "prehypertensive", 
      data$sbp >= 140 ~ "hypertensive",
      TRUE ~ NA_character_
    )
  }
  
  return(data)
}


#' Update Population with Health Check Attendees
#'
#' @param population Original population dataframe
#' @param attendees_modified Modified attendees dataframe
#' @param current_year Current simulation year
#' @return Updated population dataframe
update_population_with_attendees <- function(population, attendees_modified, current_year) {
  
  pop_updated <- population
  
  for (i in 1:nrow(attendees_modified)) {
    attendee_id <- attendees_modified$id[i]
    pop_index <- which(pop_updated$id == attendee_id)
    
    if (length(pop_index) == 1) {
      # Update risk factors
      pop_updated$bmi[pop_index] <- attendees_modified$bmi[i]
      pop_updated$sbp[pop_index] <- attendees_modified$sbp[i]
      pop_updated$smoking_status[pop_index] <- attendees_modified$smoking_status[i]
      
      # Update categories if they exist
      if ("bmi_category" %in% names(attendees_modified) && "bmi_category" %in% names(pop_updated)) {
        pop_updated$bmi_category[pop_index] <- attendees_modified$bmi_category[i]
      }
      if ("sbp_category" %in% names(attendees_modified) && "sbp_category" %in% names(pop_updated)) {
        pop_updated$sbp_category[pop_index] <- attendees_modified$sbp_category[i]
      }
      
      # Update attendance records
      pop_updated$health_check_attended[pop_index] <- TRUE
      pop_updated$last_health_check_year[pop_index] <- current_year
      
      # Update attendance history 
      if (is.na(pop_updated$health_check_attendance_history[pop_index]) || 
          pop_updated$health_check_attendance_history[pop_index] == "") {
        pop_updated$health_check_attendance_history[pop_index] <- as.character(current_year)
      } else {
        pop_updated$health_check_attendance_history[pop_index] <- paste0(
          pop_updated$health_check_attendance_history[pop_index], ",", current_year
        )
      }
    }
  }
  
  return(pop_updated)
}


#' Get Default NHS Health Check Parameters
#'
#' Returns a list of default parameter values for different scenarios
#'
#' @param scenario Character: "baseline_2018", "intervention_75_target", or "custom"
#' @return List of parameter values
get_nhs_health_check_defaults <- function(scenario = "baseline_2018") {
  
  if (scenario == "baseline_2018") {
    return(list(
      male_attendance_rate = 0.3803,
      female_attendance_rate = 0.4396,
      intervention_cost = 150,
      bmi_reduction = -0.3,
      sbp_reduction = -3.22,
      smoking_cessation_rate = 0.0635
    ))
  } else if (scenario == "intervention_75_target") {
    return(list(
      male_attendance_rate = 0.75,
      female_attendance_rate = 0.75,
      intervention_cost = 150,
      bmi_reduction = -0.3,
      sbp_reduction = -3.22,
      smoking_cessation_rate = 0.0635
    ))
  } else {
    # Custom scenario - return empty list for user to fill
    return(list(
      male_attendance_rate = NULL,
      female_attendance_rate = NULL,
      intervention_cost = NULL,
      bmi_reduction = -0.3,
      sbp_reduction = -3.22,
      smoking_cessation_rate = 0.0635
    ))
  }
}

# Example Usage
# # Apply NHS Health Check Intervention
# nhs_health_check_result <- apply_nhs_health_check(
#   population = population,
#   male_attendance_rate = 0.3803,
#   female_attendance_rate = 0.4396,
#   intervention_cost = 150,
#   seed = seed + year
# )
#
# population <- nhs_health_check_result$population
# annual_intervention_costs <- nhs_health_check_result$intervention_costs
# # Store attendance summary for model outputs
# health_check_eligible <- nhs_health_check_result$attendance_summary$total_eligible
# health_check_attendees <- nhs_health_check_result$attendance_summary$total_attending