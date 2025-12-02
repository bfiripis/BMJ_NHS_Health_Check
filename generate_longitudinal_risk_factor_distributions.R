library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(nnet)
library(mvtnorm)
library(readr)
library(janitor)

#' Process Longitudinal HSE Data
#' 
#' Extracts and processes HSE risk factor data with model-compatible blood pressure categories
#' 
#' @return List containing processed datasets for smoking, BMI, and blood pressure
process_longitudinal_hse_data <- function() {
  
  # Extract smoking data
  extract_smoking_data <- function() {
    hse_smoking_file_path <- "data/raw/Health_Survey_for_England/2022/HSE-2022-Adults'-health-related-behaviours-tables.xlsx"
    data <- read_excel(hse_smoking_file_path, sheet = "Table 2")
    years <- as.numeric(data[3, 2:31])
    
    row_mapping <- list(
      list(sex_label = "Men", status = "Current smoker", age = "16 - 24 years", row = 8),
      list(sex_label = "Men", status = "Current smoker", age = "25 - 34 years", row = 9),
      list(sex_label = "Men", status = "Current smoker", age = "35 - 44 years", row = 10),
      list(sex_label = "Men", status = "Current smoker", age = "45 - 54 years", row = 11),
      list(sex_label = "Men", status = "Current smoker", age = "55 - 64 years", row = 12),
      list(sex_label = "Men", status = "Current smoker", age = "65 - 74 years", row = 13),
      list(sex_label = "Men", status = "Current smoker", age = "75+ years", row = 14),
      list(sex_label = "Men", status = "Former smoker", age = "16 - 24 years", row = 17),
      list(sex_label = "Men", status = "Former smoker", age = "25 - 34 years", row = 18),
      list(sex_label = "Men", status = "Former smoker", age = "35 - 44 years", row = 19),
      list(sex_label = "Men", status = "Former smoker", age = "45 - 54 years", row = 20),
      list(sex_label = "Men", status = "Former smoker", age = "55 - 64 years", row = 21),
      list(sex_label = "Men", status = "Former smoker", age = "65 - 74 years", row = 22),
      list(sex_label = "Men", status = "Former smoker", age = "75+ years", row = 23),
      list(sex_label = "Men", status = "Never smoker", age = "16 - 24 years", row = 26),
      list(sex_label = "Men", status = "Never smoker", age = "25 - 34 years", row = 27),
      list(sex_label = "Men", status = "Never smoker", age = "35 - 44 years", row = 28),
      list(sex_label = "Men", status = "Never smoker", age = "45 - 54 years", row = 29),
      list(sex_label = "Men", status = "Never smoker", age = "55 - 64 years", row = 30),
      list(sex_label = "Men", status = "Never smoker", age = "65 - 74 years", row = 31),
      list(sex_label = "Men", status = "Never smoker", age = "75+ years", row = 32),
      list(sex_label = "Women", status = "Current smoker", age = "16 - 24 years", row = 55),
      list(sex_label = "Women", status = "Current smoker", age = "25 - 34 years", row = 56),
      list(sex_label = "Women", status = "Current smoker", age = "35 - 44 years", row = 57),
      list(sex_label = "Women", status = "Current smoker", age = "45 - 54 years", row = 58),
      list(sex_label = "Women", status = "Current smoker", age = "55 - 64 years", row = 59),
      list(sex_label = "Women", status = "Current smoker", age = "65 - 74 years", row = 60),
      list(sex_label = "Women", status = "Current smoker", age = "75+ years", row = 61),
      list(sex_label = "Women", status = "Former smoker", age = "16 - 24 years", row = 64),
      list(sex_label = "Women", status = "Former smoker", age = "25 - 34 years", row = 65),
      list(sex_label = "Women", status = "Former smoker", age = "35 - 44 years", row = 66),
      list(sex_label = "Women", status = "Former smoker", age = "45 - 54 years", row = 67),
      list(sex_label = "Women", status = "Former smoker", age = "55 - 64 years", row = 68),
      list(sex_label = "Women", status = "Former smoker", age = "65 - 74 years", row = 69),
      list(sex_label = "Women", status = "Former smoker", age = "75+ years", row = 70),
      list(sex_label = "Women", status = "Never smoker", age = "16 - 24 years", row = 73),
      list(sex_label = "Women", status = "Never smoker", age = "25 - 34 years", row = 74),
      list(sex_label = "Women", status = "Never smoker", age = "35 - 44 years", row = 75),
      list(sex_label = "Women", status = "Never smoker", age = "45 - 54 years", row = 76),
      list(sex_label = "Women", status = "Never smoker", age = "55 - 64 years", row = 77),
      list(sex_label = "Women", status = "Never smoker", age = "65 - 74 years", row = 78),
      list(sex_label = "Women", status = "Never smoker", age = "75+ years", row = 79)
    )
    
    result_data <- data.frame()
    for(mapping in row_mapping) {
      row_num <- mapping$row
      values <- as.numeric(data[row_num, 2:31])
      for(i in 1:length(years)) {
        if(!is.na(years[i]) && !is.na(values[i])) {
          row_data <- data.frame(
            sex_label = mapping$sex_label,
            smoking_status = mapping$status,
            age_group = mapping$age,
            year = years[i],
            rate = values[i],
            stringsAsFactors = FALSE
          )
          result_data <- rbind(result_data, row_data)
        }
      }
    }
    
    result_data <- result_data %>%
      dplyr::filter(!is.na(rate) & !is.na(year)) %>%
      dplyr::rename(smoking_category = smoking_status) %>%
      dplyr::filter(!is.na(smoking_category)) %>%
      dplyr::group_by(sex_label, age_group, year) %>%
      dplyr::mutate(
        total = sum(rate, na.rm = TRUE),
        rate = if_else(total > 0, (rate / total) * 100, rate)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        year = as.numeric(year),
        rate = as.numeric(rate),
        age_group = factor(age_group, levels = c("16 - 24 years", "25 - 34 years", "35 - 44 years", 
                                                 "45 - 54 years", "55 - 64 years", "65 - 74 years", 
                                                 "75+ years")),
        sex_label = factor(sex_label),
        smoking_category = factor(smoking_category, levels = c("Current smoker", "Former smoker", "Never smoker"))
      ) %>%
      dplyr::select(sex_label, age_group, year, smoking_category, rate) %>%
      dplyr::arrange(sex_label, age_group, year, smoking_category)
    
    return(result_data)
  }
  
  # Extract BMI data
  extract_bmi_data <- function() {
    hse_bmi_file_path <- "data/raw/Health_Survey_for_England/2022/HSE-2022-Overweight-and-obesity-tables.xlsx"
    data <- read_excel(hse_bmi_file_path, sheet = "Table 3")
    years <- as.numeric(data[3, 2:31])
    
    row_mapping <- list(
      # Men's data
      list(sex_label = "Men", category = "% Underweight", age = "16 - 24 years", row = 7),
      list(sex_label = "Men", category = "% Healthy weight", age = "16 - 24 years", row = 8),
      list(sex_label = "Men", category = "% Overweight", age = "16 - 24 years", row = 9),
      list(sex_label = "Men", category = "% Obese", age = "16 - 24 years", row = 10),
      list(sex_label = "Men", category = "% Morbidly obese", age = "16 - 24 years", row = 11),
      list(sex_label = "Men", category = "% Underweight", age = "25 - 34 years", row = 16),
      list(sex_label = "Men", category = "% Healthy weight", age = "25 - 34 years", row = 17),
      list(sex_label = "Men", category = "% Overweight", age = "25 - 34 years", row = 18),
      list(sex_label = "Men", category = "% Obese", age = "25 - 34 years", row = 19),
      list(sex_label = "Men", category = "% Morbidly obese", age = "25 - 34 years", row = 20),
      list(sex_label = "Men", category = "% Underweight", age = "35 - 44 years", row = 25),
      list(sex_label = "Men", category = "% Healthy weight", age = "35 - 44 years", row = 26),
      list(sex_label = "Men", category = "% Overweight", age = "35 - 44 years", row = 27),
      list(sex_label = "Men", category = "% Obese", age = "35 - 44 years", row = 28),
      list(sex_label = "Men", category = "% Morbidly obese", age = "35 - 44 years", row = 29),
      list(sex_label = "Men", category = "% Underweight", age = "45 - 54 years", row = 34),
      list(sex_label = "Men", category = "% Healthy weight", age = "45 - 54 years", row = 35),
      list(sex_label = "Men", category = "% Overweight", age = "45 - 54 years", row = 36),
      list(sex_label = "Men", category = "% Obese", age = "45 - 54 years", row = 37),
      list(sex_label = "Men", category = "% Morbidly obese", age = "45 - 54 years", row = 38),
      list(sex_label = "Men", category = "% Underweight", age = "55 - 64 years", row = 43),
      list(sex_label = "Men", category = "% Healthy weight", age = "55 - 64 years", row = 44),
      list(sex_label = "Men", category = "% Overweight", age = "55 - 64 years", row = 45),
      list(sex_label = "Men", category = "% Obese", age = "55 - 64 years", row = 46),
      list(sex_label = "Men", category = "% Morbidly obese", age = "55 - 64 years", row = 47),
      list(sex_label = "Men", category = "% Underweight", age = "65 - 74 years", row = 52),
      list(sex_label = "Men", category = "% Healthy weight", age = "65 - 74 years", row = 53),
      list(sex_label = "Men", category = "% Overweight", age = "65 - 74 years", row = 54),
      list(sex_label = "Men", category = "% Obese", age = "65 - 74 years", row = 55),
      list(sex_label = "Men", category = "% Morbidly obese", age = "65 - 74 years", row = 56),
      list(sex_label = "Men", category = "% Underweight", age = "75+ years", row = 61),
      list(sex_label = "Men", category = "% Healthy weight", age = "75+ years", row = 62),
      list(sex_label = "Men", category = "% Overweight", age = "75+ years", row = 63),
      list(sex_label = "Men", category = "% Obese", age = "75+ years", row = 64),
      list(sex_label = "Men", category = "% Morbidly obese", age = "75+ years", row = 65),
      
      # Women's data
      list(sex_label = "Women", category = "% Underweight", age = "16 - 24 years", row = 98),
      list(sex_label = "Women", category = "% Healthy weight", age = "16 - 24 years", row = 99),
      list(sex_label = "Women", category = "% Overweight", age = "16 - 24 years", row = 100),
      list(sex_label = "Women", category = "% Obese", age = "16 - 24 years", row = 101),
      list(sex_label = "Women", category = "% Morbidly obese", age = "16 - 24 years", row = 102),
      list(sex_label = "Women", category = "% Underweight", age = "25 - 34 years", row = 107),
      list(sex_label = "Women", category = "% Healthy weight", age = "25 - 34 years", row = 108),
      list(sex_label = "Women", category = "% Overweight", age = "25 - 34 years", row = 109),
      list(sex_label = "Women", category = "% Obese", age = "25 - 34 years", row = 110),
      list(sex_label = "Women", category = "% Morbidly obese", age = "25 - 34 years", row = 111),
      list(sex_label = "Women", category = "% Underweight", age = "35 - 44 years", row = 116),
      list(sex_label = "Women", category = "% Healthy weight", age = "35 - 44 years", row = 117),
      list(sex_label = "Women", category = "% Overweight", age = "35 - 44 years", row = 118),
      list(sex_label = "Women", category = "% Obese", age = "35 - 44 years", row = 119),
      list(sex_label = "Women", category = "% Morbidly obese", age = "35 - 44 years", row = 120),
      list(sex_label = "Women", category = "% Underweight", age = "45 - 54 years", row = 125),
      list(sex_label = "Women", category = "% Healthy weight", age = "45 - 54 years", row = 126),
      list(sex_label = "Women", category = "% Overweight", age = "45 - 54 years", row = 127),
      list(sex_label = "Women", category = "% Obese", age = "45 - 54 years", row = 128),
      list(sex_label = "Women", category = "% Morbidly obese", age = "45 - 54 years", row = 129),
      list(sex_label = "Women", category = "% Underweight", age = "55 - 64 years", row = 134),
      list(sex_label = "Women", category = "% Healthy weight", age = "55 - 64 years", row = 135),
      list(sex_label = "Women", category = "% Overweight", age = "55 - 64 years", row = 136),
      list(sex_label = "Women", category = "% Obese", age = "55 - 64 years", row = 137),
      list(sex_label = "Women", category = "% Morbidly obese", age = "55 - 64 years", row = 138),
      list(sex_label = "Women", category = "% Underweight", age = "65 - 74 years", row = 143),
      list(sex_label = "Women", category = "% Healthy weight", age = "65 - 74 years", row = 144),
      list(sex_label = "Women", category = "% Overweight", age = "65 - 74 years", row = 145),
      list(sex_label = "Women", category = "% Obese", age = "65 - 74 years", row = 146),
      list(sex_label = "Women", category = "% Morbidly obese", age = "65 - 74 years", row = 147),
      list(sex_label = "Women", category = "% Underweight", age = "75+ years", row = 152),
      list(sex_label = "Women", category = "% Healthy weight", age = "75+ years", row = 153),
      list(sex_label = "Women", category = "% Overweight", age = "75+ years", row = 154),
      list(sex_label = "Women", category = "% Obese", age = "75+ years", row = 155),
      list(sex_label = "Women", category = "% Morbidly obese", age = "75+ years", row = 156)
    )
    
    result_data <- data.frame()
    for(mapping in row_mapping) {
      row_num <- mapping$row
      values <- as.numeric(data[row_num, 2:31])
      for(i in 1:length(years)) {
        if(!is.na(years[i]) && !is.na(values[i])) {
          row_data <- data.frame(
            sex_label = mapping$sex_label,
            bmi_category = mapping$category,
            age_group = mapping$age,
            year = years[i],
            rate = values[i],
            stringsAsFactors = FALSE
          )
          result_data <- rbind(result_data, row_data)
        }
      }
    }
    
    result_data <- result_data %>%
      dplyr::filter(!is.na(rate) & !is.na(year)) %>%
      dplyr::filter(bmi_category %in% c("% Underweight", "% Healthy weight", "% Overweight", "% Obese", "% Morbidly obese")) %>%
      dplyr::mutate(
        bmi_category_new = case_when(
          bmi_category %in% c("% Underweight", "% Healthy weight") ~ "Healthy weight",
          bmi_category == "% Overweight" ~ "Overweight", 
          bmi_category %in% c("% Obese", "% Morbidly obese") ~ "Obese"
        )
      ) %>%
      dplyr::filter(!is.na(bmi_category_new)) %>%
      dplyr::group_by(sex_label, age_group, year, bmi_category_new) %>%
      dplyr::summarise(rate = sum(rate, na.rm = TRUE), .groups = 'drop') %>%
      dplyr::group_by(sex_label, age_group, year) %>%
      dplyr::mutate(
        total = sum(rate, na.rm = TRUE),
        rate = if_else(total > 0, (rate / total) * 100, rate)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        year = as.numeric(year),
        rate = as.numeric(rate),
        age_group = factor(age_group, levels = c("16 - 24 years", "25 - 34 years", "35 - 44 years", 
                                                 "45 - 54 years", "55 - 64 years", "65 - 74 years", 
                                                 "75+ years")),
        sex_label = factor(sex_label),
        bmi_category = factor(bmi_category_new, levels = c("Healthy weight", "Overweight", "Obese"))
      ) %>%
      dplyr::select(sex_label, age_group, year, bmi_category, rate) %>%
      dplyr::arrange(sex_label, age_group, year, bmi_category)
    
    return(result_data)
  }
  
  #' Extract and map blood pressure data
  extract_bp_data_mapped <- function() {
    hse_bp_file_path <- "data/raw/Health_Survey_for_England/2022/HSE-2022-Adult-health-tables.xlsx"
    hse_file_path <- "data/raw/hse_2021_eul_v1.tab"
    
    # Define target age groups for final output
    target_age_groups <- c("16 - 24 years", "25 - 34 years", "35 - 44 years", 
                           "45 - 54 years", "55 - 64 years", "65 - 74 years", "75+ years")
    
    # Estimate age-specific BP proportions using smooth age-SBP relationship
    estimate_age_specific_bp_rates <- function() {
      hse_data <- read_tsv(hse_file_path) %>%
        clean_names(case = "old_janitor") %>%
        filter(!is.na(omsysval) & omsysval > 0 &     # Valid BP measurements
                 !is.na(age35g) & age35g >= 7) %>%     # Adults only (age35g >= 7 means 16+)
        mutate(
          sex_label = ifelse(sex == 1, "Men", "Women"),
          # Use ALL granular age categories for best model fitting
          age_continuous = case_when(
            age35g == 7 ~ 17.5,    # 16-19 -> midpoint 17.5
            age35g == 8 ~ 22,      # 20-24 -> midpoint 22
            age35g == 9 ~ 27,      # 25-29 -> midpoint 27
            age35g == 10 ~ 32,     # 30-34 -> midpoint 32
            age35g == 11 ~ 37,     # 35-39 -> midpoint 37
            age35g == 12 ~ 42,     # 40-44 -> midpoint 42
            age35g == 13 ~ 47,     # 45-49 -> midpoint 47
            age35g == 14 ~ 52,     # 50-54 -> midpoint 52
            age35g == 15 ~ 57,     # 55-59 -> midpoint 57
            age35g == 16 ~ 62,     # 60-64 -> midpoint 62
            age35g == 17 ~ 67,     # 65-69 -> midpoint 67
            age35g == 18 ~ 72,     # 70-74 -> midpoint 72
            age35g == 19 ~ 77,     # 75-79 -> midpoint 77
            age35g == 20 ~ 82,     # 80-84 -> midpoint 82
            age35g == 21 ~ 87,     # 85-89 -> midpoint 87
            age35g == 22 ~ 92,     # 90+ -> estimate 92
            TRUE ~ NA_real_
          )
        ) %>%
        filter(!is.na(age_continuous))
      
      # Include 75+ in target predictions
      target_ages <- data.frame(
        age_group = target_age_groups,
        age_midpoint = c(20, 29.5, 39.5, 49.5, 59.5, 69.5, 82)
      )
      
      bp_age_models <- data.frame()
      
      for(sex_val in c("Men", "Women")) {
        sex_data <- hse_data %>% filter(sex_label == sex_val)
        
        # Fit logistic models for BP thresholds by age
        model_120 <- glm(I(omsysval < 120) ~ age_continuous, 
                         data = sex_data, family = binomial)
        model_140 <- glm(I(omsysval < 140) ~ age_continuous, 
                         data = sex_data, family = binomial)
        
        # Predict for target age groups
        pred_data <- data.frame(age_continuous = target_ages$age_midpoint)
        prob_under_120 <- predict(model_120, newdata = pred_data, type = "response")
        prob_under_140 <- predict(model_140, newdata = pred_data, type = "response")
        
        sex_proportions <- data.frame(
          sex_label = sex_val,
          age_group = target_age_groups,
          prop_under_120 = prob_under_120,
          prop_120_140 = pmax(0, prob_under_140 - prob_under_120),
          prop_over_140 = pmax(0, 1 - prob_under_140)
        ) %>%
          mutate(
            total_temp = prop_under_120 + prop_120_140 + prop_over_140,
            prop_under_120 = prop_under_120 / total_temp,
            prop_120_140 = prop_120_140 / total_temp,
            prop_over_140 = prop_over_140 / total_temp
          ) %>%
          dplyr::select(sex_label, age_group, prop_under_120, prop_120_140, prop_over_140)
        
        bp_age_models <- rbind(bp_age_models, sex_proportions)
      }
      
      return(bp_age_models)
    }
    
    # Get the HSE longitudinal BP trends (overall rates by sex/year)
    extract_longitudinal_trends <- function() {
      table12_raw <- read_excel(hse_bp_file_path, sheet = "Table 12", col_names = FALSE)
      
      years <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 
                 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 
                 2019, 2021, 2022)
      bp_categories <- c("Normotensive untreated", "Hypertensive controlled", 
                         "Hypertensive uncontrolled", "Hypertensive untreated")
      
      longitudinal_data <- data.frame()
      
      # Men's data (rows 7-10)
      for(i in 1:4) {
        row_data <- table12_raw[6 + i, 2:20]
        row_data[row_data == "-"] <- NA
        row_data <- as.character(row_data)
        row_data[is.na(row_data) | row_data == "" | row_data == "-"] <- NA
        rates <- as.numeric(row_data) / 100
        
        valid_idx <- !is.na(rates)
        if(any(valid_idx)) {
          new_data <- data.frame(
            sex_label = "Men",
            year = years[valid_idx],
            bp_category = bp_categories[i],
            rate = rates[valid_idx],
            stringsAsFactors = FALSE
          )
          longitudinal_data <- rbind(longitudinal_data, new_data)
        }
      }
      
      # Women's data (rows 14-17)
      for(i in 1:4) {
        row_data <- table12_raw[13 + i, 2:20]
        row_data[row_data == "-"] <- NA
        row_data <- as.character(row_data)
        row_data[is.na(row_data) | row_data == "" | row_data == "-"] <- NA
        rates <- as.numeric(row_data) / 100
        
        valid_idx <- !is.na(rates)
        if(any(valid_idx)) {
          new_data <- data.frame(
            sex_label = "Women",
            year = years[valid_idx],
            bp_category = bp_categories[i],
            rate = rates[valid_idx],
            stringsAsFactors = FALSE
          )
          longitudinal_data <- rbind(longitudinal_data, new_data)
        }
      }
      
      # Aggregate into mixed_under_140 and over_140
      aggregated_data <- longitudinal_data %>%
        mutate(
          model_bp_category = case_when(
            bp_category %in% c("Normotensive untreated", "Hypertensive controlled") ~ "mixed_under_140",
            bp_category %in% c("Hypertensive uncontrolled", "Hypertensive untreated") ~ "over_140"
          )
        ) %>%
        group_by(sex_label, year, model_bp_category) %>%
        summarise(rate = sum(rate, na.rm = TRUE) * 100, .groups = 'drop')
      
      return(aggregated_data)
    }
    
    # Get smooth age-specific proportions and longitudinal trends
    smooth_proportions <- estimate_age_specific_bp_rates()
    longitudinal_trends <- extract_longitudinal_trends()
    
    # Calculate mapping coefficients for disaggregation
    mapping_coefficients <- smooth_proportions %>%
      mutate(
        total_under_140 = prop_under_120 + prop_120_140,
        prop_under_120_within_under_140 = prop_under_120 / total_under_140,
        prop_120_140_within_under_140 = prop_120_140 / total_under_140
      ) %>%
      dplyr::select(sex_label, age_group, prop_under_120_within_under_140, prop_120_140_within_under_140)
    
    # Create age-specific data by applying longitudinal trends to age-specific proportions
    final_bp_data <- data.frame()
    
    for(i in 1:nrow(longitudinal_trends)) {
      trend_row <- longitudinal_trends[i, ]
      
      # Get proportions for this sex
      sex_coefficients <- mapping_coefficients %>%
        filter(sex_label == trend_row$sex_label)
      
      if(trend_row$model_bp_category == "mixed_under_140") {
        # Split mixed_under_140 into <120 and 120-140 for each age group
        for(j in 1:nrow(sex_coefficients)) {
          age_coef <- sex_coefficients[j, ]
          
          under_120_row <- data.frame(
            sex_label = trend_row$sex_label,
            age_group = age_coef$age_group,
            year = trend_row$year,
            bp_category = "<120 mmHg",
            rate = trend_row$rate * age_coef$prop_under_120_within_under_140,
            stringsAsFactors = FALSE
          )
          
          bp_120_140_row <- data.frame(
            sex_label = trend_row$sex_label,
            age_group = age_coef$age_group,
            year = trend_row$year,
            bp_category = "120-140 mmHg",
            rate = trend_row$rate * age_coef$prop_120_140_within_under_140,
            stringsAsFactors = FALSE
          )
          
          final_bp_data <- rbind(final_bp_data, under_120_row, bp_120_140_row)
        }
        
      } else if(trend_row$model_bp_category == "over_140") {
        # Apply over_140 to each age group
        for(j in 1:nrow(sex_coefficients)) {
          age_coef <- sex_coefficients[j, ]
          
          over_140_row <- data.frame(
            sex_label = trend_row$sex_label,
            age_group = age_coef$age_group,
            year = trend_row$year,
            bp_category = ">140 mmHg",
            rate = trend_row$rate,
            stringsAsFactors = FALSE
          )
          
          final_bp_data <- rbind(final_bp_data, over_140_row)
        }
      }
    }
    
    # Final formatting
    final_bp_data <- final_bp_data %>%
      mutate(
        year = as.numeric(year),
        rate = as.numeric(rate),
        age_group = factor(age_group, levels = target_age_groups),
        sex_label = factor(sex_label),
        bp_category = factor(bp_category, levels = c("<120 mmHg", "120-140 mmHg", ">140 mmHg"))
      ) %>%
      arrange(sex_label, age_group, year, bp_category)
    
    return(final_bp_data)
  }
  
  # Extract all data
  hse_smoking_data <- extract_smoking_data()
  hse_bmi_data <- extract_bmi_data() 
  hse_bp_data_mapped <- extract_bp_data_mapped()
  
  # Return as named list
  return(list(
    smoking = hse_smoking_data,
    bmi = hse_bmi_data,
    sbp = hse_bp_data_mapped
  ))
}

#' Generate Longitudinal Risk Factor Distributions
#' 
#' Takes longitudinal HSE risk factor data and projects future risk factor trends
#' using multinomial logistic regression with parametric bootstrap confidence intervals
#' 
#' @param hse_data List containing extracted HSE datasets (smoking, bmi, sbp)
#' @param projection_years Vector of years to project (default: 2025:2040)
#' @param n_samples Number of samples for confidence intervals (default: 1000)
#' @return List containing projections, model coefficients, and metadata
generate_longitudinal_risk_factor_distributions <- function(projection_years = 2025:2040, n_samples = 1000) {
  
  hse_data <- process_longitudinal_hse_data()
  
  .fit_stratum_model <- function(rf_data, category_col, value_col, age_grp, sex_val, rf_name) {
    stratum_data <- rf_data %>%
      dplyr::filter(age_group == age_grp & sex_label == sex_val) %>%
      dplyr::select(year, all_of(category_col), all_of(value_col)) %>%
      dplyr::filter(!is.na(.data[[value_col]]))
    
    year_min <- min(stratum_data$year)
    stratum_data$year_centered <- stratum_data$year - year_min
    
    expanded_data <- data.frame()
    for (i in 1:nrow(stratum_data)) {
      category <- stratum_data[[category_col]][i]
      rate <- stratum_data[[value_col]][i]
      year_centered <- stratum_data$year_centered[i]
      count <- round(rate)
      
      category_data <- data.frame(
        year_centered = rep(year_centered, count),
        category = rep(category, count),
        stringsAsFactors = FALSE
      )
      expanded_data <- rbind(expanded_data, category_data)
    }
    
    expanded_data$category <- factor(expanded_data$category)
    categories <- levels(expanded_data$category)
    
    if (length(categories) == 2) {
      expanded_data$binary_outcome <- as.numeric(expanded_data$category == categories[2])
      model <- glm(binary_outcome ~ year_centered, data = expanded_data, family = binomial)
      vcov_matrix <- vcov(model)
      reference_category <- categories[1]
    } else {
      model <- multinom(category ~ year_centered, data = expanded_data, trace = FALSE)
      vcov_matrix <- vcov(model)
      reference_category <- levels(expanded_data$category)[1]
    }
    
    return(list(
      model = model,
      coefficients = if (inherits(model, "multinom")) coef(model) else coef(model),
      vcov_matrix = vcov_matrix,
      categories = categories,
      reference_category = reference_category,
      year_min = year_min,
      risk_factor = rf_name,
      age_group = age_grp,
      sex_label = sex_val
    ))
  }
  
  .calculate_ci <- function(model_result, year_centered, n_samples) {
    categories <- model_result$categories
    n_categories <- length(categories)
    
    if (is.matrix(model_result$coefficients)) {
      coef_vec <- as.vector(model_result$coefficients)
    } else {
      coef_vec <- model_result$coefficients
    }
    
    coef_samples <- rmvnorm(n_samples, mean = coef_vec, sigma = model_result$vcov_matrix)
    prob_samples <- matrix(NA, nrow = n_samples, ncol = n_categories)
    
    for (i in 1:n_samples) {
      sample_coefs <- coef_samples[i, ]
      
      if (n_categories == 2) {
        linear_pred <- sample_coefs[1] + sample_coefs[2] * year_centered
        prob_second <- plogis(linear_pred)
        prob_samples[i, ] <- c(1 - prob_second, prob_second)
      } else {
        n_coefs_per_category <- length(sample_coefs) / (n_categories - 1)
        linear_preds <- numeric(n_categories - 1)
        
        for (j in 1:(n_categories - 1)) {
          start_idx <- (j - 1) * n_coefs_per_category + 1
          end_idx <- j * n_coefs_per_category
          category_coefs <- sample_coefs[start_idx:end_idx]
          linear_preds[j] <- category_coefs[1] + category_coefs[2] * year_centered
        }
        
        exp_preds <- c(1, exp(linear_preds))
        prob_samples[i, ] <- exp_preds / sum(exp_preds)
      }
    }
    
    ci_lower <- apply(prob_samples, 2, quantile, probs = 0.025, na.rm = TRUE)
    ci_upper <- apply(prob_samples, 2, quantile, probs = 0.975, na.rm = TRUE)
    
    return(list(
      ci_lower = ci_lower,
      ci_upper = ci_upper
    ))
  }
  
  .project_model <- function(model_result, proj_years, n_samples, all_categories = NULL) {
    projections <- data.frame()
    
    for (year in proj_years) {
      year_centered <- year - model_result$year_min
      pred_data <- data.frame(year_centered = year_centered)
      
      if (inherits(model_result$model, "multinom")) {
        predicted_probs <- predict(model_result$model, newdata = pred_data, type = "probs")
      } else if (inherits(model_result$model, "glm")) {
        prob_second <- predict(model_result$model, newdata = pred_data, type = "response")
        prob_first <- 1 - prob_second
        predicted_probs <- c(prob_first, prob_second)
        names(predicted_probs) <- model_result$categories
      }
      
      if (is.vector(predicted_probs) && length(model_result$categories) == 2) {
        if (length(predicted_probs) == 1) {
          prob_second <- as.numeric(predicted_probs)
          prob_first <- 1 - prob_second
          predicted_probs <- c(prob_first, prob_second)
          names(predicted_probs) <- model_result$categories
        }
      }
      
      ci_results <- .calculate_ci(model_result, year_centered, n_samples)
      
      year_results <- data.frame(
        year = year,
        risk_factor = model_result$risk_factor,
        age_group = model_result$age_group,
        sex_label = model_result$sex_label
      )
      
      categories_to_use <- if (!is.null(all_categories)) all_categories else model_result$categories
      
      for (category in categories_to_use) {
        cat_index <- which(model_result$categories == category)
        prob_value <- if (is.matrix(predicted_probs)) predicted_probs[1, cat_index] else predicted_probs[cat_index]
        ci_lower_value <- ci_results$ci_lower[cat_index]
        ci_upper_value <- ci_results$ci_upper[cat_index]
        
        year_results[[paste0("prob_", gsub("[^A-Za-z0-9]", "_", category))]] <- prob_value
        year_results[[paste0("ci_lower_", gsub("[^A-Za-z0-9]", "_", category))]] <- ci_lower_value
        year_results[[paste0("ci_upper_", gsub("[^A-Za-z0-9]", "_", category))]] <- ci_upper_value
      }
      
      projections <- rbind(projections, year_results)
    }
    
    return(projections)
  }
  
  risk_factors <- list(
    smoking = list(data = hse_data$smoking %>% 
                     filter(!age_group %in% c("All men", "All women")), 
                   category_col = "smoking_category", value_col = "rate"),
    bmi = list(data = hse_data$bmi, category_col = "bmi_category", value_col = "rate"),
    sbp = list(data = hse_data$sbp, category_col = "bp_category", value_col = "rate")
  )
  
  all_projections <- list()
  all_models <- list()
  
  for (rf_name in names(risk_factors)) {
    rf_config <- risk_factors[[rf_name]]
    rf_data <- rf_config$data
    all_categories <- unique(rf_data[[rf_config$category_col]]) %>% 
      sort()
    
    strata <- rf_data %>%
      dplyr::select(age_group, sex_label) %>%
      distinct() %>%
      arrange(sex_label, age_group)
    
    rf_projections <- data.frame()
    
    for (i in 1:nrow(strata)) {
      age_grp <- strata$age_group[i]
      sex_val <- strata$sex_label[i]
      
      model_result <- .fit_stratum_model(
        rf_data, 
        rf_config$category_col, 
        rf_config$value_col, 
        age_grp, 
        sex_val, 
        rf_name
      )
      
      model_name <- paste(rf_name, age_grp, sex_val, sep = "_")
      all_models[[model_name]] <- list(
        coefficients = model_result$coefficients,
        vcov_matrix = model_result$vcov_matrix,
        categories = model_result$categories,
        reference_category = model_result$reference_category,
        risk_factor = rf_name,
        age_group = age_grp,
        sex_label = sex_val
      )
      
      stratum_projections <- .project_model(model_result, projection_years, n_samples, all_categories)
      rf_projections <- rbind(rf_projections, stratum_projections)
    }
    
    all_projections[[rf_name]] <- rf_projections %>% 
      arrange(sex_label, age_group, year)
  }
  
  final_results <- list(
    projections = all_projections,
    model_coefficients = all_models,
    metadata = list(
      projection_years = projection_years,
      n_samples_ci = n_samples,
      risk_factors = names(all_projections),
      smoking_categories = c("Current smoker", "Former smoker", "Never smoker"),
      bmi_categories = c("Healthy weight", "Overweight", "Obese"),
      sbp_categories = c("<120 mmHg", "120-140 mmHg", ">140 mmHg"),
      generated_on = Sys.time(),
      methodology = "Multinomial logistic regression with parametric bootstrap confidence intervals"
    )
  )
  
  return(final_results)
}

# Example usage
# longitudinal_hse_distributions <- generate_longitudinal_risk_factor_distributions(
# projection_years = 2025:2040, n_samples = 1000)