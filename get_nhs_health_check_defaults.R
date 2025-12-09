#' Get NHS Health Check Default Parameters
#' 
#' Returns parameter sets for different NHS Health Check scenarios
#' Based on BMJ Open study parameters
#'
#' @param scenario Character: scenario type
#' @return List of NHS Health Check parameters
#'
get_nhs_health_check_defaults <- function(scenario = "baseline_2018") {
  
  if (scenario == "baseline_2018") {
    # BMJ study baseline attendance rates (2017-2018 data)
    params <- list(
      male_attendance_rate = 0.3803,      # 38.03% from BMJ study
      female_attendance_rate = 0.4396,    # 43.96% from BMJ study
      intervention_cost = 150,            # Cost per health check
      bmi_reduction = -0.3,               # kg/m² reduction for obese individuals
      sbp_reduction = -3.22,              # mmHg reduction for hypertensive individuals  
      smoking_cessation_rate = 0.0635,    # 6.35% quit rate from BMJ study
      scenario_name = "Baseline 2018"
    )
    
  } else if (scenario == "intervention_75_target") {
    # BMJ study intervention scenario: 75% attendance target
    # This requires calculating what attendance rates achieve 75% overall
    
    # For males: need to go from 38.03% to contribute to 75% overall
    # For females: need to go from 43.96% to contribute to 75% overall
    # Assuming equal male/female population eligible
    
    params <- list(
      male_attendance_rate = 0.75,        # Target 75% attendance for males
      female_attendance_rate = 0.75,      # Target 75% attendance for females
      intervention_cost = 150,            # Cost per health check
      bmi_reduction = -0.3,               # kg/m² reduction for obese individuals
      sbp_reduction = -3.22,              # mmHg reduction for hypertensive individuals
      smoking_cessation_rate = 0.0635,    # 6.35% quit rate from BMJ study  
      scenario_name = "75% Attendance Target"
    )
    
  } else if (scenario == "intervention_bmj_exact") {
    # Exact BMJ study intervention parameters
    # Males: 38.03% baseline → need 59.65% additional to reach 75% total
    # Females: 43.96% baseline → need 55.39% additional to reach 75% total
    
    params <- list(
      male_attendance_rate = 0.75,        # 75% target as per BMJ methodology
      female_attendance_rate = 0.75,      # 75% target as per BMJ methodology
      intervention_cost = 150,            # Cost per health check
      bmi_reduction = -0.3,               # kg/m² reduction for obese individuals
      sbp_reduction = -3.22,              # mmHg reduction for hypertensive individuals
      smoking_cessation_rate = 0.0635,    # 6.35% quit rate from BMJ study
      scenario_name = "BMJ Exact Intervention"
    )
    
  } else {
    stop(sprintf("Unknown scenario: %s. Available: 'baseline_2018', 'intervention_75_target', 'intervention_bmj_exact'", scenario))
  }
  
  return(params)
}