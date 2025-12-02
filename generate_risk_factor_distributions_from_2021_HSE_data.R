library(dplyr)
library(fitdistrplus)
library(readr)
library(janitor)
library(purrr)
library(tidyr)

generate_hse_distributions <- function() {
  hse_file_path <- "data/raw/hse_2021_eul_v1.tab"
  prevalence_file_path <- "data/prevalence.csv"
  
  
  if (file.exists(hse_file_path)) {
    hse_data <- read_tsv(hse_file_path) %>%
      clean_names(case = "old_janitor") %>%
      mutate(
        # Create age groups using age35g (which covers all ages 0+)
        age_group = case_when(
          age35g == 1 ~ "0 - 1 years", age35g == 2 ~ "2 - 4 years", age35g == 3 ~ "5 - 7 years",
          age35g == 4 ~ "8 - 10 years", age35g == 5 ~ "11 - 12 years", age35g == 6 ~ "13 - 15 years",
          age35g == 7 ~ "16 - 19 years", age35g == 8 ~ "20 - 24 years", age35g == 9 ~ "25 - 29 years",
          age35g == 10 ~ "30 - 34 years", age35g == 11 ~ "35 - 39 years", age35g == 12 ~ "40 - 44 years",
          age35g == 13 ~ "45 - 49 years", age35g == 14 ~ "50 - 54 years", age35g == 15 ~ "55 - 59 years",
          age35g == 16 ~ "60 - 64 years", age35g == 17 ~ "65 - 69 years", age35g == 18 ~ "70 - 74 years",
          age35g == 19 ~ "75 - 79 years", age35g == 20 ~ "80 - 84 years", age35g == 21 ~ "85 - 89 years",
          age35g == 22 ~ "90+ years", TRUE ~ NA_character_
        ),
        # Convert to factor with proper ordering
        age_group = factor(age_group, levels = c(
          "0 - 1 years", "2 - 4 years", "5 - 7 years", "8 - 10 years", "11 - 12 years", "13 - 15 years",
          "16 - 19 years", "20 - 24 years", "25 - 29 years", "30 - 34 years", "35 - 39 years", "40 - 44 years",
          "45 - 49 years", "50 - 54 years", "55 - 59 years", "60 - 64 years", "65 - 69 years", "70 - 74 years",
          "75 - 79 years", "80 - 84 years", "85 - 89 years", "90+ years"
        )),
        # Flag for child (under 16) or adult (16+)
        is_child = age35g %in% c(1, 2, 3, 4, 5, 6),
        # Create sex labels
        sex_label = ifelse(sex == 1, "Men", "Women"),
        
        # Clean BMI values - filter out negatives and extreme outliers
        bmi_clean = case_when(
          !is.na(bmisr_adj) & bmisr_adj > 0 & bmisr_adj < 100 ~ bmisr_adj,
          TRUE ~ NA_real_
        ),
        
        # Clean SBP values - filter out negatives and extreme outliers
        sbp_clean = case_when(
          !is.na(omsysval) & omsysval > 0 & omsysval < 300 ~ omsysval,
          TRUE ~ NA_real_
        )
      ) %>%
      # Filter based on data availability
      filter(!is.na(age_group)) %>%
      filter(
        (is_child == TRUE) |
          (is_child == FALSE & (!is.na(bmi_clean) | !is.na(sbp_clean) |
                                  (!is.na(smkevr_19) & !is.na(cignow_19))))
      ) %>%
      mutate(
        # Create smoking status
        smoking_status = case_when(
          is_child == TRUE ~ "Never smoker",  # Children default to never smoker
          smkevr_19 == 1 ~ "Never smoker",
          smkevr_19 == 2 & cignow_19 == 2 ~ "Current smoker",
          smkevr_19 == 2 & cignow_19 == 1 ~ "Ex-smoker",
          TRUE ~ NA_character_
        ),
        # Create BMI categories for proportions (using cleaned values)
        bmi_category = case_when(
          !is.na(bmi_clean) & bmi_clean < 18.5 ~ "Underweight",
          !is.na(bmi_clean) & bmi_clean >= 18.5 & bmi_clean < 25 ~ "Normal weight",
          !is.na(bmi_clean) & bmi_clean >= 25 & bmi_clean < 30 ~ "Overweight",
          !is.na(bmi_clean) & bmi_clean >= 30 ~ "Obese",
          TRUE ~ NA_character_
        ),
        # Create SBP categories for proportions (using cleaned values)
        sbp_category = case_when(
          !is.na(sbp_clean) & sbp_clean < 120 ~ "Normotensive",
          !is.na(sbp_clean) & sbp_clean >= 120 & sbp_clean < 140 ~ "Prehypertensive",
          !is.na(sbp_clean) & sbp_clean >= 140 ~ "Hypertensive",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(smoking_status), !is.na(age_group))
    
    # --- Helper Function for Distribution Fitting ---
    
    estimate_normal_params <- function(values) {
      # Need at least 2 values to calculate SD
      if (length(values) < 2) {
        return(data.frame(mean = NA, sd = NA, n = length(values), min_val = NA, max_val = NA))
      }
      fit <- fitdist(values, "norm")
      data.frame(
        mean = fit$estimate["mean"],
        sd = fit$estimate["sd"],
        n = length(values),
        min_val = min(values),
        max_val = max(values)
      )
    }
    
    # --- FALLBACK CALCULATIONS ---
    
    # 1. Primary Fallback: Sex-specific, All Ages combined
    bmi_fallback_sex <- hse_data %>%
      filter(!is.na(bmi_clean)) %>%
      group_by(sex_label) %>%
      do(estimate_normal_params(.$bmi_clean)) %>%
      rename(mean_sex = mean, sd_sex = sd) %>%
      dplyr::select(sex_label, mean_sex, sd_sex)
    
    sbp_fallback_sex <- hse_data %>%
      filter(!is.na(sbp_clean)) %>%
      group_by(sex_label) %>%
      do(estimate_normal_params(.$sbp_clean)) %>%
      rename(mean_sex = mean, sd_sex = sd) %>%
      dplyr::select(sex_label, mean_sex, sd_sex)
    
    # 2. Secondary Fallback: Overall All Ages/Sexes combined
    bmi_overall_params <- hse_data %>% filter(!is.na(bmi_clean)) %>% pull(bmi_clean) %>% estimate_normal_params()
    bmi_overall_mean <- bmi_overall_params$mean
    bmi_overall_sd <- bmi_overall_params$sd
    
    sbp_overall_params <- hse_data %>% filter(!is.na(sbp_clean)) %>% pull(sbp_clean) %>% estimate_normal_params()
    sbp_overall_mean <- sbp_overall_params$mean
    sbp_overall_sd <- sbp_overall_params$sd
    
    # --- Create complete combinations template ---
    all_age_sex_combinations <- expand_grid(
      age_group = factor(c(
        "0 - 1 years", "2 - 4 years", "5 - 7 years", "8 - 10 years", "11 - 12 years", "13 - 15 years",
        "16 - 19 years", "20 - 24 years", "25 - 29 years", "30 - 34 years", "35 - 39 years", "40 - 44 years",
        "45 - 49 years", "50 - 54 years", "55 - 59 years", "60 - 64 years", "65 - 69 years", "70 - 74 years",
        "75 - 79 years", "80 - 84 years", "85 - 89 years", "90+ years"
      ), levels = c(
        "0 - 1 years", "2 - 4 years", "5 - 7 years", "8 - 10 years", "11 - 12 years", "13 - 15 years",
        "16 - 19 years", "20 - 24 years", "25 - 29 years", "30 - 34 years", "35 - 39 years", "40 - 44 years",
        "45 - 49 years", "50 - 54 years", "55 - 59 years", "60 - 64 years", "65 - 69 years", "70 - 74 years",
        "75 - 79 years", "80 - 84 years", "85 - 89 years", "90+ years"
      )),
      sex_label = c("Men", "Women")
    )
    
    # Calculate parameters for all age/sex groups (N < 5 groups will have NA/low-N estimates)
    bmi_distributions_raw <- hse_data %>%
      filter(!is.na(bmi_clean)) %>%
      group_by(age_group, sex_label) %>%
      do(estimate_normal_params(.$bmi_clean)) %>%
      ungroup()
    
    # Complete the distributions to ensure all combinations are present
    bmi_distributions <- all_age_sex_combinations %>%
      left_join(bmi_distributions_raw, by = c("age_group", "sex_label")) %>%
      # Join with primary fallbacks
      left_join(bmi_fallback_sex, by = "sex_label") %>%
      # Apply Primary Fallback: If N < 5, use sex-specific parameters
      mutate(
        mean_final = ifelse(is.na(n) | n < 5, mean_sex, mean),
        sd_final = ifelse(is.na(n) | n < 5, sd_sex, sd),
        n = coalesce(n, 0)  # Replace NA with 0 for missing combinations
      ) %>%
      # Apply Secondary Fallback: If mean_final is NA (e.g. if sex-pool was too small), use overall mean/sd
      mutate(
        mean = coalesce(mean_final, bmi_overall_mean),
        sd = coalesce(sd_final, bmi_overall_sd)
      ) %>%
      dplyr::select(age_group, sex_label, mean, sd, n, min_val, max_val) %>%
      arrange(age_group, sex_label)
    
    # Calculate parameters for all age/sex groups
    sbp_distributions_raw <- hse_data %>%
      filter(!is.na(sbp_clean)) %>%
      group_by(age_group, sex_label) %>%
      do(estimate_normal_params(.$sbp_clean)) %>%
      ungroup()
    
    # Complete the distributions to ensure all combinations are present
    sbp_distributions <- all_age_sex_combinations %>%
      left_join(sbp_distributions_raw, by = c("age_group", "sex_label")) %>%
      # Join with primary fallbacks
      left_join(sbp_fallback_sex, by = "sex_label") %>%
      # Apply Primary Fallback: If N < 5, use sex-specific parameters
      mutate(
        mean_final = ifelse(is.na(n) | n < 5, mean_sex, mean),
        sd_final = ifelse(is.na(n) | n < 5, sd_sex, sd),
        n = coalesce(n, 0)  # Replace NA with 0 for missing combinations
      ) %>%
      # Apply Secondary Fallback: If mean_final is NA, use overall mean/sd
      mutate(
        mean = coalesce(mean_final, sbp_overall_mean),
        sd = coalesce(sd_final, sbp_overall_sd)
      ) %>%
      dplyr::select(age_group, sex_label, mean, sd, n, min_val, max_val) %>%
      arrange(age_group, sex_label)
    
    # Complete smoking proportions to ensure all combinations are present
    smoking_proportions_raw <- hse_data %>%
      group_by(age_group, sex_label) %>%
      summarise(
        total = n(),
        never_smoker = sum(smoking_status == "Never smoker"),
        ex_smoker = sum(smoking_status == "Ex-smoker"),
        current_smoker = sum(smoking_status == "Current smoker"),
        prop_never = never_smoker / total,
        prop_ex = ex_smoker / total,
        prop_current = current_smoker / total,
        .groups = 'drop'
      )
    
    # Calculate overall smoking proportions as fallback
    overall_smoking_props <- hse_data %>%
      summarise(
        total = n(),
        never_smoker = sum(smoking_status == "Never smoker"),
        ex_smoker = sum(smoking_status == "Ex-smoker"),
        current_smoker = sum(smoking_status == "Current smoker"),
        prop_never = never_smoker / total,
        prop_ex = ex_smoker / total,
        prop_current = current_smoker / total
      )
    
    smoking_proportions <- all_age_sex_combinations %>%
      left_join(smoking_proportions_raw, by = c("age_group", "sex_label")) %>%
      # Use overall proportions for missing combinations
      mutate(
        total = coalesce(total, 0),
        never_smoker = coalesce(never_smoker, 0),
        ex_smoker = coalesce(ex_smoker, 0),
        current_smoker = coalesce(current_smoker, 0),
        prop_never = coalesce(prop_never, overall_smoking_props$prop_never),
        prop_ex = coalesce(prop_ex, overall_smoking_props$prop_ex),
        prop_current = coalesce(prop_current, overall_smoking_props$prop_current)
      )
    
    # Complete BMI proportions for adults
    bmi_proportions_adults_raw <- hse_data %>%
      filter(!is.na(bmi_category), !is_child) %>%
      group_by(age_group, sex_label) %>%
      summarise(
        total = n(),
        underweight = sum(bmi_category == "Underweight"),
        normal = sum(bmi_category == "Normal weight"),
        overweight = sum(bmi_category == "Overweight"),
        obese = sum(bmi_category == "Obese"),
        prop_underweight = underweight / total,
        prop_normal = normal / total,
        prop_overweight = overweight / total,
        prop_obese = obese / total,
        .groups = 'drop'
      )
    
    # Calculate overall BMI proportions as fallback for adults
    overall_bmi_props <- hse_data %>%
      filter(!is.na(bmi_category), !is_child) %>%
      summarise(
        total = n(),
        underweight = sum(bmi_category == "Underweight"),
        normal = sum(bmi_category == "Normal weight"),
        overweight = sum(bmi_category == "Overweight"),
        obese = sum(bmi_category == "Obese"),
        prop_underweight = underweight / total,
        prop_normal = normal / total,
        prop_overweight = overweight / total,
        prop_obese = obese / total
      )
    
    # Get adult age groups only
    adult_age_groups <- all_age_sex_combinations %>%
      filter(!age_group %in% c("0 - 1 years", "2 - 4 years", "5 - 7 years", "8 - 10 years", "11 - 12 years", "13 - 15 years"))
    
    bmi_proportions_adults <- adult_age_groups %>%
      left_join(bmi_proportions_adults_raw, by = c("age_group", "sex_label")) %>%
      # Use overall proportions for missing combinations
      mutate(
        total = coalesce(total, 0),
        underweight = coalesce(underweight, 0),
        normal = coalesce(normal, 0),
        overweight = coalesce(overweight, 0),
        obese = coalesce(obese, 0),
        prop_underweight = coalesce(prop_underweight, overall_bmi_props$prop_underweight),
        prop_normal = coalesce(prop_normal, overall_bmi_props$prop_normal),
        prop_overweight = coalesce(prop_overweight, overall_bmi_props$prop_overweight),
        prop_obese = coalesce(prop_obese, overall_bmi_props$prop_obese)
      )
    
    # Use 16-19 years as template for children BMI proportions
    children_bmi_props_template <- bmi_proportions_adults %>%
      filter(age_group == "16 - 19 years") %>%
      dplyr::select(-age_group)
    
    children_bmi_proportions <- bind_rows(
      children_bmi_props_template %>% mutate(age_group = factor("0 - 1 years", levels = levels(all_age_sex_combinations$age_group))),
      children_bmi_props_template %>% mutate(age_group = factor("2 - 4 years", levels = levels(all_age_sex_combinations$age_group))),
      children_bmi_props_template %>% mutate(age_group = factor("5 - 7 years", levels = levels(all_age_sex_combinations$age_group))),
      children_bmi_props_template %>% mutate(age_group = factor("8 - 10 years", levels = levels(all_age_sex_combinations$age_group))),
      children_bmi_props_template %>% mutate(age_group = factor("11 - 12 years", levels = levels(all_age_sex_combinations$age_group))),
      children_bmi_props_template %>% mutate(age_group = factor("13 - 15 years", levels = levels(all_age_sex_combinations$age_group)))
    )
    
    bmi_proportions <- bind_rows(bmi_proportions_adults, children_bmi_proportions) %>%
      arrange(age_group, sex_label)
    
    # Complete SBP proportions
    sbp_proportions_raw <- hse_data %>%
      filter(!is.na(sbp_category)) %>%
      group_by(age_group, sex_label) %>%
      summarise(
        total = n(),
        normotensive = sum(sbp_category == "Normotensive"),
        prehypertensive = sum(sbp_category == "Prehypertensive"),
        hypertensive = sum(sbp_category == "Hypertensive"),
        prop_normotensive = normotensive / total,
        prop_prehypertensive = prehypertensive / total,
        prop_hypertensive = hypertensive / total,
        .groups = 'drop'
      )
    
    # Calculate overall SBP proportions as fallback
    overall_sbp_props <- hse_data %>%
      filter(!is.na(sbp_category)) %>%
      summarise(
        total = n(),
        normotensive = sum(sbp_category == "Normotensive"),
        prehypertensive = sum(sbp_category == "Prehypertensive"),
        hypertensive = sum(sbp_category == "Hypertensive"),
        prop_normotensive = normotensive / total,
        prop_prehypertensive = prehypertensive / total,
        prop_hypertensive = hypertensive / total
      )
    
    sbp_proportions <- all_age_sex_combinations %>%
      left_join(sbp_proportions_raw, by = c("age_group", "sex_label")) %>%
      # Use overall proportions for missing combinations
      mutate(
        total = coalesce(total, 0),
        normotensive = coalesce(normotensive, 0),
        prehypertensive = coalesce(prehypertensive, 0),
        hypertensive = coalesce(hypertensive, 0),
        prop_normotensive = coalesce(prop_normotensive, overall_sbp_props$prop_normotensive),
        prop_prehypertensive = coalesce(prop_prehypertensive, overall_sbp_props$prop_prehypertensive),
        prop_hypertensive = coalesce(prop_hypertensive, overall_sbp_props$prop_hypertensive)
      )
    
    data_source <- "HSE 2021 data"
  }
  
  if (file.exists(prevalence_file_path)) {
    prevalence_raw <- readr::read_csv(prevalence_file_path, show_col_types = FALSE)
    
    disease_prevalence <- prevalence_raw %>%
      mutate(
        age_lower = case_when(
          age == "0 - 1 years" ~ 0, age == "2 - 4 years" ~ 2, age == "5 - 7 years" ~ 5,
          age == "8 - 10 years" ~ 8, age == "11 - 12 years" ~ 11, age == "13 - 15 years" ~ 13,
          age == "16 - 19 years" ~ 16, age == "20 - 24 years" ~ 20,
          age == "25 - 29 years" ~ 25, age == "30 - 34 years" ~ 30, age == "35 - 39 years" ~ 35,
          age == "40 - 44 years" ~ 40, age == "45 - 49 years" ~ 45, age == "50 - 54 years" ~ 50,
          age == "55 - 59 years" ~ 55, age == "60 - 64 years" ~ 60, age == "65 - 69 years" ~ 65,
          age == "70 - 74 years" ~ 70, age == "75 - 79 years" ~ 75, age == "80 - 84 years" ~ 80,
          age == "85 - 89 years" ~ 85, age == "90 - 94 years" ~ 90, age == "95+ years" ~ 95,
          TRUE ~ NA_real_
        ),
        age_upper = case_when(
          age == "0 - 1 years" ~ 1, age == "2 - 4 years" ~ 4, age == "5 - 7 years" ~ 7,
          age == "8 - 10 years" ~ 10, age == "11 - 12 years" ~ 12, age == "13 - 15 years" ~ 15,
          age == "16 - 19 years" ~ 19, age == "20 - 24 years" ~ 24,
          age == "25 - 29 years" ~ 29, age == "30 - 34 years" ~ 34, age == "35 - 39 years" ~ 39,
          age == "40 - 44 years" ~ 44, age == "45 - 49 years" ~ 49, age == "50 - 54 years" ~ 54,
          age == "55 - 59 years" ~ 59, age == "60 - 64 years" ~ 64, age == "65 - 69 years" ~ 69,
          age == "70 - 74 years" ~ 74, age == "75 - 79 years" ~ 79, age == "80 - 84 years" ~ 84,
          age == "85 - 89 years" ~ 89, age == "90 - 94 years" ~ 94, age == "95+ years" ~ 120,
          TRUE ~ NA_real_
        ),
        sex_label = case_when(
          sex == "Male" ~ "Men",
          sex == "Female" ~ "Women",
          TRUE ~ sex
        ),
        prevalence_prob = rate_per_100k / 100000
      ) %>%
      filter(!is.na(age_lower), !is.na(age_upper))
  } else {
    disease_prevalence <- data.frame()
  }
  
  return(list(
    data_summary = list(
      source = data_source,
      risk_factors_included = c("BMI", "Systolic Blood Pressure", "Smoking Status"),
      diseases_included = if(nrow(disease_prevalence) > 0) unique(disease_prevalence$disease) else character(0)
    ),
    
    distributions = list(
      bmi = bmi_distributions,
      sbp = sbp_distributions
    ),
    
    proportions = list(
      smoking = smoking_proportions,
      bmi_categories = bmi_proportions,
      sbp_categories = sbp_proportions
    ),
    
    disease_prevalence = disease_prevalence
  ))
}