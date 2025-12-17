#' Calculate baseline incidence probabilities adjusted for population risk factor distributions
#' 
#' @param hse_distributions List object containing HSE risk factor distributions and proportions
#' @param incidence_data Data frame with incidence data (default: NULL, will read from file)
#' @param smoking_relative_risks Data frame with smoking relative risks (default: NULL, will read from file)
#' @param bmi_relative_risks Data frame with BMI relative risks (default: NULL, will read from file) 
#' @param bp_relative_risks Data frame with blood pressure relative risks (default: NULL, will read from file)
#' @return Data frame with baseline incidence probabilities and rates
calculate_baseline_incidence_probabilities <- function(hse_distributions = NULL,
                                                       incidence_data = NULL,
                                                       smoking_relative_risks = NULL,
                                                       bmi_relative_risks = NULL,
                                                       bp_relative_risks = NULL) {
  
  # Load HSE distributions if not provided
  if (is.null(hse_distributions)) {
    source("R/generate_risk_factor_distributions_from_2021_HSE_data.R")
    hse_distributions <- generate_hse_distributions()
  }
  
  # Load or use incidence data
  if (is.null(incidence_data)) {
    incidence_data <- read_csv("data/incidence.csv", show_col_types = FALSE)
  }
  
  incidence_data <- incidence_data %>%
    dplyr::filter(measure == "incidence", !is.na(rate_per_100k)) %>%
    mutate(
      disease_std = case_when(
        str_detect(tolower(disease), "chd") ~ "CHD",
        str_detect(tolower(disease), "colorectal") ~ "Colorectal_Cancer", 
        str_detect(tolower(disease), "copd") ~ "COPD",
        str_detect(tolower(disease), "lung") ~ "Lung_Cancer",
        str_detect(tolower(disease), "stroke") ~ "Stroke",
        TRUE ~ disease
      ),
      incidence_prob = rate_per_100k / 100000,
      sex_label = str_to_title(sex)
    )
  
  # Load or use relative risk data
  if (is.null(smoking_relative_risks)) {
    smoking_relative_risks <- read_csv("data/smoking_relative_risks.csv", show_col_types = FALSE)
  }
  
  if (is.null(bmi_relative_risks)) {
    bmi_relative_risks <- read_csv("data/BMI_relative_risks.csv", show_col_types = FALSE)
  }
  
  if (is.null(bp_relative_risks)) {
    bp_relative_risks <- read_csv("data/blood_pressure_relative_risks.csv", show_col_types = FALSE)
  }
  
  # Process relative risk data
  smoking_rr <- smoking_relative_risks %>%
    rename(disease_std = Disease, sex_label = Sex, risk_factor_category = Smoking_Status,
           relative_risk = Relative_Risk, age_range = Age) %>%
    mutate(risk_factor = "smoking")
  
  bmi_rr <- bmi_relative_risks %>%
    rename(disease_std = Disease, sex_label = Sex, risk_factor_category = BMI_Status,
           relative_risk = Relative_Risk, age_range = Age) %>%
    mutate(risk_factor = "bmi")
  
  bp_rr <- bp_relative_risks %>%
    rename(disease_std = Disease, sex_label = Sex, risk_factor_category = Hypertension_Status,
           relative_risk = Relative_Risk, age_range = Age) %>%
    mutate(risk_factor = "blood_pressure")
  
  all_rr <- bind_rows(smoking_rr, bmi_rr, bp_rr)
  
  # Age mapping function - maps incidence age groups to HSE age_group factors
  map_age_to_hse_age_group <- function(age_str) {
    case_when(
      age_str == "0 years" ~ "0 - 1 years",
      age_str == "1 - 4 years" ~ "2 - 4 years",
      age_str == "5 - 9 years" ~ "5 - 7 years",
      age_str == "10 - 14 years" ~ "11 - 12 years",
      age_str == "15 - 19 years" ~ "16 - 19 years",
      age_str == "20 - 24 years" ~ "20 - 24 years",
      age_str == "25 - 29 years" ~ "25 - 29 years",
      age_str == "30 - 34 years" ~ "30 - 34 years",
      age_str == "35 - 39 years" ~ "35 - 39 years",
      age_str == "40 - 44 years" ~ "40 - 44 years",
      age_str == "45 - 49 years" ~ "45 - 49 years",
      age_str == "50 - 54 years" ~ "50 - 54 years",
      age_str == "55 - 59 years" ~ "55 - 59 years",
      age_str == "60 - 64 years" ~ "60 - 64 years",
      age_str == "65 - 69 years" ~ "65 - 69 years",
      age_str == "70 - 74 years" ~ "70 - 74 years",
      age_str == "75 - 79 years" ~ "75 - 79 years",
      age_str == "80 - 84 years" ~ "80 - 84 years",
      age_str == "85 - 89 years" ~ "85 - 89 years",
      age_str == "90 - 94 years" ~ "90+ years",
      age_str == "95+ years" ~ "90+ years",
      TRUE ~ NA_character_
    )
  }
  
  # Age mapping function for relative risk data
  map_age_to_range <- function(age_str, rr_age_range) {
    age_lower <- case_when(
      str_detect(age_str, "^0 years") ~ 0,
      str_detect(age_str, "^1 - 4") ~ 1,
      str_detect(age_str, "^5 - 9") ~ 5,
      str_detect(age_str, "^10 - 14") ~ 10,
      str_detect(age_str, "^15 - 19") ~ 15,
      str_detect(age_str, "^20 - 24") ~ 20,
      str_detect(age_str, "^25 - 29") ~ 25,
      str_detect(age_str, "^30 - 34") ~ 30,
      str_detect(age_str, "^35 - 39") ~ 35,
      str_detect(age_str, "^40 - 44") ~ 40,
      str_detect(age_str, "^45 - 49") ~ 45,
      str_detect(age_str, "^50 - 54") ~ 50,
      str_detect(age_str, "^55 - 59") ~ 55,
      str_detect(age_str, "^60 - 64") ~ 60,
      str_detect(age_str, "^65 - 69") ~ 65,
      str_detect(age_str, "^70 - 74") ~ 70,
      str_detect(age_str, "^75 - 79") ~ 75,
      str_detect(age_str, "^80 - 84") ~ 80,
      str_detect(age_str, "^85 - 89") ~ 85,
      str_detect(age_str, "^90 - 94") ~ 90,
      str_detect(age_str, "95\\+") ~ 95,
      TRUE ~ NA_real_
    )
    
    case_when(
      str_detect(rr_age_range, "18 - 110") ~ age_lower >= 18,
      str_detect(rr_age_range, "<35") ~ age_lower < 35,
      str_detect(rr_age_range, "35 - 39") ~ age_lower >= 35 & age_lower <= 39,
      str_detect(rr_age_range, "40 - 44") ~ age_lower >= 40 & age_lower <= 44,
      str_detect(rr_age_range, "45 - 49") ~ age_lower >= 45 & age_lower <= 49,
      str_detect(rr_age_range, "50 - 54") ~ age_lower >= 50 & age_lower <= 54,
      str_detect(rr_age_range, "55 - 59") ~ age_lower >= 55 & age_lower <= 59,
      str_detect(rr_age_range, "60 - 64") ~ age_lower >= 60 & age_lower <= 64,
      str_detect(rr_age_range, "65\\+") ~ age_lower >= 65,
      str_detect(rr_age_range, "0 - 20") ~ age_lower >= 0 & age_lower <= 20,
      str_detect(rr_age_range, "21 - 45") ~ age_lower >= 21 & age_lower <= 45,
      str_detect(rr_age_range, "45\\+") ~ age_lower >= 45,
      str_detect(rr_age_range, "21 - 25") ~ age_lower >= 21 & age_lower <= 25,
      str_detect(rr_age_range, "26 - 64") ~ age_lower >= 26 & age_lower <= 64,
      str_detect(rr_age_range, "65 - 69") ~ age_lower >= 65 & age_lower <= 69,
      str_detect(rr_age_range, "70 - 79") ~ age_lower >= 70 & age_lower <= 79,
      str_detect(rr_age_range, "80\\+") ~ age_lower >= 80,
      str_detect(rr_age_range, "25 - 34") ~ age_lower >= 25 & age_lower <= 34,
      str_detect(rr_age_range, "35 - 44") ~ age_lower >= 35 & age_lower <= 44,
      str_detect(rr_age_range, "45 - 54") ~ age_lower >= 45 & age_lower <= 54,
      str_detect(rr_age_range, "55 - 64") ~ age_lower >= 55 & age_lower <= 64,
      str_detect(rr_age_range, "65 - 74") ~ age_lower >= 65 & age_lower <= 74,
      str_detect(rr_age_range, "75 - 84") ~ age_lower >= 75 & age_lower <= 84,
      str_detect(rr_age_range, "85\\+") ~ age_lower >= 85,
      str_detect(rr_age_range, "per_10mm") ~ TRUE,
      TRUE ~ FALSE
    )
  }
  
  # Get prevalences from HSE data using age_group factors
  get_prevalences <- function(age_str, sex_str, hse_dist) {
    
    hse_age_group <- map_age_to_hse_age_group(age_str)
    
    # Return NULL if no valid age mapping found
    if(is.na(hse_age_group)) return(NULL)
    
    sex_label <- ifelse(sex_str == "Male", "Men", "Women")
    
    # Smoking proportions - filter by age_group and sex_label
    smoking_data <- hse_dist$proportions$smoking %>%
      dplyr::filter(age_group == !!hse_age_group, sex_label == !!sex_label) %>%
      slice(1)
    
    # BMI proportions  
    bmi_data <- hse_dist$proportions$bmi_categories %>%
      dplyr::filter(age_group == !!hse_age_group, sex_label == !!sex_label) %>%
      slice(1)
    
    # SBP proportions
    sbp_data <- hse_dist$proportions$sbp_categories %>%
      dplyr::filter(age_group == !!hse_age_group, sex_label == !!sex_label) %>%
      slice(1)
    
    list(
      smoking = list(
        never = ifelse(nrow(smoking_data) > 0, smoking_data$prop_never, NA_real_),
        former = ifelse(nrow(smoking_data) > 0, smoking_data$prop_ex, NA_real_),
        current = ifelse(nrow(smoking_data) > 0, smoking_data$prop_current, NA_real_)
      ),
      bmi = list(
        normal = ifelse(nrow(bmi_data) > 0, bmi_data$prop_normal + bmi_data$prop_underweight, NA_real_),
        overweight = ifelse(nrow(bmi_data) > 0, bmi_data$prop_overweight, NA_real_),
        obese = ifelse(nrow(bmi_data) > 0, bmi_data$prop_obese, NA_real_)
      ),
      bp = list(
        normal = ifelse(nrow(sbp_data) > 0, sbp_data$prop_normotensive, NA_real_),
        hypertensive = ifelse(nrow(sbp_data) > 0, sbp_data$prop_hypertensive + sbp_data$prop_prehypertensive, NA_real_)
      )
    )
  }
  
  # Calculate weighted average RR
  calculate_weighted_rr <- function(disease_name, age_str, sex_str, hse_dist) {
    
    applicable_rr <- all_rr %>%
      dplyr::filter(disease_std == disease_name, sex_label == sex_str) %>%
      rowwise() %>%
      dplyr::filter(map_age_to_range(age_str, age_range)) %>%
      ungroup()
    
    if (nrow(applicable_rr) == 0) return(1.0)
    
    prevalences <- get_prevalences(age_str, sex_str, hse_dist)
    
    # If no HSE data available for this age group, return 1.0
    if(is.null(prevalences)) return(1.0)
    
    weighted_rrs <- c()
    
    for (rf in unique(applicable_rr$risk_factor)) {
      rf_data <- applicable_rr %>% dplyr::filter(risk_factor == rf)
      
      if (rf == "smoking") {
        # Check if all smoking prevalences are available
        if(any(is.na(unlist(prevalences$smoking)))) {
          weighted_rr <- 1.0
        } else {
          smoking_weighted <- rf_data %>%
            mutate(
              weight = case_when(
                str_detect(risk_factor_category, "Never") ~ prevalences$smoking$never,
                str_detect(risk_factor_category, "Former") ~ prevalences$smoking$former,
                str_detect(risk_factor_category, "Current") ~ prevalences$smoking$current,
                TRUE ~ 0
              )
            )
          weighted_rr <- sum(smoking_weighted$relative_risk * smoking_weighted$weight)
        }
        
      } else if (rf == "bmi") {
        # Check if all BMI prevalences are available
        if(any(is.na(unlist(prevalences$bmi)))) {
          weighted_rr <- 1.0
        } else {
          bmi_weighted <- rf_data %>%
            mutate(
              weight = case_when(
                risk_factor_category == "Normal" ~ prevalences$bmi$normal,
                risk_factor_category == "Overweight" ~ prevalences$bmi$overweight,
                risk_factor_category == "Obese" ~ prevalences$bmi$obese,
                TRUE ~ 0
              )
            )
          weighted_rr <- sum(bmi_weighted$relative_risk * bmi_weighted$weight)
        }
        
      } else if (rf == "blood_pressure") {
        if (any(str_detect(rf_data$risk_factor_category, "per_10mm"))) {
          
          # Get HSE age group for this age/sex combination
          hse_age_group <- map_age_to_hse_age_group(age_str)
          sex_label <- ifelse(sex_str == "Male", "Men", "Women")
          
          if (is.na(hse_age_group)) {
            weighted_rr <- 1.0
          } else {
            # Get SBP distribution data for this age/sex group
            sbp_dist_data <- hse_dist$distributions$sbp %>%
              dplyr::filter(age_group == !!hse_age_group, sex_label == !!sex_label)
            
            if (nrow(sbp_dist_data) == 0) {
              weighted_rr <- 1.0
            } else {
              # Use static SBP as reference (SBP=115 for RR = 1.0)
              reference_sbp <- 115
              
              # Get the age-specific per_10mmHg RR
              per_10mm_rr <- rf_data$relative_risk[1]
              
              # Get population proportions for SBP categories
              sbp_prop_data <- hse_dist$proportions$sbp_categories %>%
                dplyr::filter(age_group == !!hse_age_group, sex_label == !!sex_label) %>%
                slice(1)
              
              if (nrow(sbp_prop_data) == 0) {
                weighted_rr <- 1.0
              } else {
                # For continuous per_10mmHg approach, we need to calculate based on the mean SBP
                # since we don't have category-specific means in the new structure
                mean_sbp <- sbp_dist_data$mean[1]
                
                if(is.na(mean_sbp)) {
                  weighted_rr <- 1.0
                } else {
                  # Calculate RR for population mean SBP relative to reference
                  # RR = per_10mm_rr^((actual_sbp - reference_sbp)/10)
                  sbp_increment <- (mean_sbp - reference_sbp) / 10
                  weighted_rr <- per_10mm_rr^sbp_increment
                }
              }
            }
          }
          
        } else {
          # Handle categorical BP data
          if(any(is.na(unlist(prevalences$bp)))) {
            weighted_rr <- 1.0
          } else {
            bp_weighted <- rf_data %>%
              mutate(
                weight = case_when(
                  risk_factor_category == "No_Hypertension" ~ prevalences$bp$normal,
                  risk_factor_category == "Hypertension" ~ prevalences$bp$hypertensive,
                  TRUE ~ 0
                )
              )
            weighted_rr <- sum(bp_weighted$relative_risk * bp_weighted$weight)
          }
        }
      } else {
        weighted_rr <- 1.0
      }
      
      weighted_rrs <- c(weighted_rrs, weighted_rr)
    }
    
    prod(weighted_rrs)
  }
  
  # Calculate results
  incidence_data %>%
    rowwise() %>%
    mutate(
      weighted_avg_rr = calculate_weighted_rr(disease_std, age, sex_label, hse_distributions),
      baseline_incidence_prob_rr1 = incidence_prob / weighted_avg_rr,
      baseline_rate_per_100k_rr1 = baseline_incidence_prob_rr1 * 100000
    ) %>%
    ungroup() %>%
    dplyr::select(
      disease,
      disease_std,
      age,
      sex_label,
      observed_rate_per_100k = rate_per_100k,
      observed_incidence_prob = incidence_prob,
      weighted_avg_rr,
      baseline_incidence_prob_rr1,
      baseline_rate_per_100k_rr1
    ) %>%
    arrange(disease_std, sex_label, age)
}

# Example usage:
# incidence_probabilities <- calculate_baseline_incidence_probabilities()