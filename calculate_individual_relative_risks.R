#' Calculate Individual Relative Risk for Microsimulation
#' 
#' This function calculates the relative risk for an individual based on their
#' current risk factor values, using the same relative risk tables used in
#' baseline calibration.
#' 
#' @param bmi Individual's BMI value
#' @param smoking_status Individual's smoking status ("Never smoker", "Ex-smoker", "Current smoker") 
#' @param sbp Individual's systolic blood pressure
#' @param age Individual's current age
#' @param sex Individual's sex ("Men" or "Women")
#' @param disease Disease for which to calculate RR
#' @param relative_risks Relative risk data loaded from CSV files
#' @return Individual's relative risk for the specified disease
#' @export
calculate_individual_rr <- function(bmi, smoking_status, sbp, age, sex, disease, 
                                    relative_risks) {
  
  # Initialize individual RR components
  bmi_rr <- 1.0
  smoking_rr <- 1.0
  bp_rr <- 1.0
  
  # BMI relative risk
  bmi_rr <- calculate_individual_bmi_rr(bmi, age, sex, disease, relative_risks$bmi)
  
  # Smoking relative risk
  smoking_rr <- calculate_individual_smoking_rr(smoking_status, age, sex, disease, relative_risks$smoking)
  
  # Blood pressure relative risk (if data available)
  if (!is.null(relative_risks$blood_pressure)) {
    bp_rr <- calculate_individual_bp_rr(sbp, age, sex, disease, relative_risks$blood_pressure)
  }
  
  # Combine relative risks (multiplicative model)
  total_rr <- bmi_rr * smoking_rr * bp_rr
  
  return(max(0.1, min(total_rr, 50.0)))  # Cap between 0.1 and 50
}

#' Calculate individual BMI relative risk
calculate_individual_bmi_rr <- function(bmi, age, sex, disease, bmi_rr_data) {
  
  # Determine BMI category
  bmi_status <- case_when(
    bmi < 18.5 ~ "Underweight",  # Will default to Normal if no underweight data
    bmi < 25 ~ "Normal",
    bmi < 30 ~ "Overweight", 
    TRUE ~ "Obese"
  )
  
  # Convert to data format
  if (bmi_status == "Underweight") bmi_status <- "Normal"  # Default to normal for underweight
  
  # Find appropriate age range and RR
  disease_rr <- bmi_rr_data %>%
    dplyr::filter(Disease == disease, Sex_label == sex, BMI_Status == bmi_status) %>%
    mutate(
      age_lower = case_when(
        Age == "0-20 years" ~ 0,
        Age == "21-45 years" ~ 21, 
        Age == "45+ years" ~ 45,
        Age == "21-25 years" ~ 21,
        Age == "26-64 years" ~ 26,
        Age == "65-69 years" ~ 65,
        Age == "70-79 years" ~ 70,
        Age == "80+ years" ~ 80,
        TRUE ~ 0
      ),
      age_upper = case_when(
        Age == "0-20 years" ~ 20,
        Age == "21-45 years" ~ 45,
        Age == "45+ years" ~ 120,
        Age == "21-25 years" ~ 25,
        Age == "26-64 years" ~ 64,
        Age == "65-69 years" ~ 69,
        Age == "70-79 years" ~ 79, 
        Age == "80+ years" ~ 120,
        TRUE ~ 120
      )
    ) %>%
    dplyr::filter(age >= age_lower, age <= age_upper)
  
  if (nrow(disease_rr) > 0) {
    return(disease_rr$Relative_Risk[1])
  } else {
    # Default values if no data found
    default_rr <- case_when(
      bmi_status == "Normal" ~ 1.0,
      bmi_status == "Overweight" ~ 1.2,
      bmi_status == "Obese" ~ 1.5,
      TRUE ~ 1.0
    )
    return(default_rr)
  }
}

#' Calculate individual smoking relative risk
calculate_individual_smoking_rr <- function(smoking_status, age, sex, disease, smoking_rr_data) {
  
  # Convert smoking status to data format
  smoking_data_status <- case_when(
    smoking_status == "Never smoker" ~ "Never_Smoked",
    smoking_status == "Ex-smoker" ~ "Former_Smoker", 
    smoking_status == "Current smoker" ~ "Current_Smoker",
    TRUE ~ "Never_Smoked"
  )
  
  # Find appropriate age range and RR
  disease_rr <- smoking_rr_data %>%
    dplyr::filter(Disease == disease, Sex_label == sex, Smoking_Status == smoking_data_status) %>%
    mutate(
      age_lower = case_when(
        Age == "<35 years" ~ 0,
        Age == "35-39 years" ~ 35,
        Age == "40-44 years" ~ 40,
        Age == "45-49 years" ~ 45,
        Age == "50-54 years" ~ 50,
        Age == "55-59 years" ~ 55,
        Age == "60-64 years" ~ 60,
        Age == "65+ years" ~ 65,
        Age == "18-110 years" ~ 18,
        TRUE ~ 0
      ),
      age_upper = case_when(
        Age == "<35 years" ~ 34,
        Age == "35-39 years" ~ 39,
        Age == "40-44 years" ~ 44,
        Age == "45-49 years" ~ 49,
        Age == "50-54 years" ~ 54,
        Age == "55-59 years" ~ 59,
        Age == "60-64 years" ~ 64,
        Age == "65+ years" ~ 120,
        Age == "18-110 years" ~ 110,
        TRUE ~ 120
      )
    ) %>%
    dplyr::filter(age >= age_lower, age <= age_upper)
  
  if (nrow(disease_rr) > 0) {
    return(disease_rr$Relative_Risk[1])
  } else {
    # Default values if no data found
    default_rr <- case_when(
      smoking_data_status == "Never_Smoked" ~ 1.0,
      smoking_data_status == "Former_Smoker" ~ 1.3,
      smoking_data_status == "Current_Smoker" ~ 2.0,
      TRUE ~ 1.0
    )
    return(default_rr)
  }
}

#' Calculate individual blood pressure relative risk
calculate_individual_bp_rr <- function(sbp, age, sex, disease, bp_rr_data) {
  
  # Check if this disease has BP relative risks
  disease_bp <- bp_rr_data %>%
    dplyr::filter(Disease == disease, Sex_label == sex)
  
  if (nrow(disease_bp) == 0) {
    return(1.0)  # No BP effect for this disease
  }
  
  # Handle per 10mmHg increases
  if (any(disease_bp$Hypertension_Status == "per_10mm_Hg")) {
    
    # Find appropriate age range
    disease_rr <- disease_bp %>%
      dplyr::filter(Hypertension_Status == "per_10mm_Hg") %>%
      mutate(
        age_lower = case_when(
          Age == "25-34 years" ~ 25,
          Age == "35-44 years" ~ 35,
          Age == "45-54 years" ~ 45,
          Age == "55-64 years" ~ 55,
          Age == "65-74 years" ~ 65,
          Age == "75-84 years" ~ 75,
          Age == "85+ years" ~ 85,
          TRUE ~ 0
        ),
        age_upper = case_when(
          Age == "25-34 years" ~ 34,
          Age == "35-44 years" ~ 44,
          Age == "45-54 years" ~ 54,
          Age == "55-64 years" ~ 64,
          Age == "65-74 years" ~ 74,
          Age == "75-84 years" ~ 84,
          Age == "85+ years" ~ 120,
          TRUE ~ 120
        )
      ) %>%
      dplyr::filter(age >= age_lower, age <= age_upper)
    
    if (nrow(disease_rr) > 0) {
      # Calculate RR per 10mmHg increase above baseline (120 mmHg)
      baseline_sbp <- 120
      mmhg_increase <- max(0, sbp - baseline_sbp)
      rr_per_10mmhg <- disease_rr$Relative_Risk[1]
      
      # Calculate total RR
      total_rr <- rr_per_10mmhg ^ (mmhg_increase / 10)
      return(total_rr)
    }
  }
  
  # Handle hypertension/no hypertension categories
  hypertension_status <- ifelse(sbp >= 140, "Hypertension", "No_Hypertension")
  
  disease_rr <- disease_bp %>%
    dplyr::filter(Hypertension_Status == hypertension_status)
  
  if (nrow(disease_rr) > 0) {
    return(disease_rr$Relative_Risk[1])
  } else {
    return(1.0)  # Default if no data
  }
}

#' Calculate mortality probability for an individual in a given year
#' @param individual_data Data frame with individual characteristics (age, sex, bmi, smoking_status, sbp)
#' @param baseline_mortality_results Results from calculate_baseline_mortality()
#' @param relative_risks Relative risk data
#' @param diseases Vector of diseases to include
#' @return Data frame with mortality probabilities by disease and total
calculate_individual_mortality <- function(individual_data, baseline_mortality_results, 
                                           relative_risks, diseases = c("CHD", "stroke", "lung_cancer", 
                                                                        "colorectal_cancer", "COPD")) {
  
  individual_data$age_lower <- case_when(
    individual_data$age < 1 ~ 0, individual_data$age < 5 ~ 1, individual_data$age < 10 ~ 5, 
    individual_data$age < 15 ~ 10, individual_data$age < 20 ~ 15, individual_data$age < 25 ~ 20, 
    individual_data$age < 30 ~ 25, individual_data$age < 35 ~ 30, individual_data$age < 40 ~ 35, 
    individual_data$age < 45 ~ 40, individual_data$age < 50 ~ 45, individual_data$age < 55 ~ 50, 
    individual_data$age < 60 ~ 55, individual_data$age < 65 ~ 60, individual_data$age < 70 ~ 65,
    individual_data$age < 75 ~ 70, individual_data$age < 80 ~ 75, individual_data$age < 85 ~ 80, 
    individual_data$age < 90 ~ 85, individual_data$age < 95 ~ 90, TRUE ~ 95
  )
  
  # Get baseline mortality rates
  baseline_disease <- baseline_mortality_results$baseline_mortality$disease_specific
  baseline_other <- baseline_mortality_results$baseline_mortality$other_causes
  
  individual_mortality <- individual_data %>%
    left_join(baseline_other, by = c("age_lower", "sex_label")) %>%
    mutate(other_causes_prob = baseline_other_causes_probability)
  
  # Calculate disease-specific mortality for each disease
  for (disease in diseases) {
    
    # Get baseline rate for this disease
    disease_baseline <- baseline_disease %>%
      dplyr::filter(disease == !!disease) %>%
      select(age_lower, sex_label, baseline_mortality_probability)
    
    individual_mortality <- individual_mortality %>%
      left_join(disease_baseline, by = c("age_lower", "sex_label"), suffix = c("", paste0("_", disease)))
    
    # Calculate individual RR for this disease
    individual_rr <- mapply(
      calculate_individual_rr,
      individual_mortality$bmi,
      individual_mortality$smoking_status, 
      individual_mortality$sbp,
      individual_mortality$age,
      individual_mortality$sex_label,
      MoreArgs = list(disease = disease, relative_risks = relative_risks),
      SIMPLIFY = TRUE
    )
    
    # Calculate individual mortality probability
    baseline_col <- paste0("baseline_mortality_probability_", disease)
    prob_col <- paste0("prob_", disease)
    
    if (baseline_col %in% names(individual_mortality)) {
      individual_mortality[[prob_col]] <- individual_mortality[[baseline_col]] * individual_rr
    } else {
      individual_mortality[[prob_col]] <- 0
    }
  }
  
  # Calculate total disease mortality
  disease_cols <- paste0("prob_", diseases)
  disease_cols <- disease_cols[disease_cols %in% names(individual_mortality)]
  
  if (length(disease_cols) > 0) {
    individual_mortality$total_disease_prob <- rowSums(individual_mortality[disease_cols], na.rm = TRUE)
  } else {
    individual_mortality$total_disease_prob <- 0
  }
  
  # Calculate total mortality (disease + other causes)
  individual_mortality$total_mortality_prob <- pmin(1.0, 
                                                    individual_mortality$total_disease_prob + individual_mortality$other_causes_prob)
  
  return(individual_mortality)
}