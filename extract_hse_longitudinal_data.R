library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

#' Extract HSE Smoking Data
#' 
#' Extracts longitudinal smoking prevalence data by age group and sex
#' 
#' @return Data frame with columns: sex, age_group, year, smoking_category, rate
extract_smoking_data <- function() {
  
  hse_smoking_file_path <- "data/raw/Health_Survey_for_England/2022/HSE-2022-Adults'-health-related-behaviours-tables.xlsx"
  data <- read_excel(hse_smoking_file_path, sheet = "Table 2")
  
  # Extract years from row 3, columns 2-31 (1993-2022)
  years <- as.numeric(data[3, 2:31])
  
  # Define exact row mappings based on the data structure
  row_mapping <- list(
    # Men - Current smokers (rows 8-14)
    list(sex = "Men", status = "Current smoker", age = "16-24 years", row = 8),
    list(sex = "Men", status = "Current smoker", age = "25-34 years", row = 9),
    list(sex = "Men", status = "Current smoker", age = "35-44 years", row = 10),
    list(sex = "Men", status = "Current smoker", age = "45-54 years", row = 11),
    list(sex = "Men", status = "Current smoker", age = "55-64 years", row = 12),
    list(sex = "Men", status = "Current smoker", age = "65-74 years", row = 13),
    list(sex = "Men", status = "Current smoker", age = "75+ years", row = 14),
    
    # Men - Former smokers (rows 17-23)
    list(sex = "Men", status = "Former smoker", age = "16-24 years", row = 17),
    list(sex = "Men", status = "Former smoker", age = "25-34 years", row = 18),
    list(sex = "Men", status = "Former smoker", age = "35-44 years", row = 19),
    list(sex = "Men", status = "Former smoker", age = "45-54 years", row = 20),
    list(sex = "Men", status = "Former smoker", age = "55-64 years", row = 21),
    list(sex = "Men", status = "Former smoker", age = "65-74 years", row = 22),
    list(sex = "Men", status = "Former smoker", age = "75+ years", row = 23),
    
    # Men - Never smokers (rows 26-32)
    list(sex = "Men", status = "Never smoker", age = "16-24 years", row = 26),
    list(sex = "Men", status = "Never smoker", age = "25-34 years", row = 27),
    list(sex = "Men", status = "Never smoker", age = "35-44 years", row = 28),
    list(sex = "Men", status = "Never smoker", age = "45-54 years", row = 29),
    list(sex = "Men", status = "Never smoker", age = "55-64 years", row = 30),
    list(sex = "Men", status = "Never smoker", age = "65-74 years", row = 31),
    list(sex = "Men", status = "Never smoker", age = "75+ years", row = 32),
    
    # Women - Current smokers
    list(sex = "Women", status = "Current smoker", age = "16-24 years", row = 55),
    list(sex = "Women", status = "Current smoker", age = "25-34 years", row = 56),
    list(sex = "Women", status = "Current smoker", age = "35-44 years", row = 57),
    list(sex = "Women", status = "Current smoker", age = "45-54 years", row = 58),
    list(sex = "Women", status = "Current smoker", age = "55-64 years", row = 59),
    list(sex = "Women", status = "Current smoker", age = "65-74 years", row = 60),
    list(sex = "Women", status = "Current smoker", age = "75+ years", row = 61),
    
    # Women - Former smokers
    list(sex = "Women", status = "Former smoker", age = "16-24 years", row = 64),
    list(sex = "Women", status = "Former smoker", age = "25-34 years", row = 65),
    list(sex = "Women", status = "Former smoker", age = "35-44 years", row = 66),
    list(sex = "Women", status = "Former smoker", age = "45-54 years", row = 67),
    list(sex = "Women", status = "Former smoker", age = "55-64 years", row = 68),
    list(sex = "Women", status = "Former smoker", age = "65-74 years", row = 69),
    list(sex = "Women", status = "Former smoker", age = "75+ years", row = 70),
    
    # Women - Never smokers
    list(sex = "Women", status = "Never smoker", age = "16-24 years", row = 73),
    list(sex = "Women", status = "Never smoker", age = "25-34 years", row = 74),
    list(sex = "Women", status = "Never smoker", age = "35-44 years", row = 75),
    list(sex = "Women", status = "Never smoker", age = "45-54 years", row = 76),
    list(sex = "Women", status = "Never smoker", age = "55-64 years", row = 77),
    list(sex = "Women", status = "Never smoker", age = "65-74 years", row = 78),
    list(sex = "Women", status = "Never smoker", age = "75+ years", row = 79)
  )
  
  # Extract data based on row mapping
  result_data <- data.frame()
  
  for(mapping in row_mapping) {
    row_num <- mapping$row
    values <- as.numeric(data[row_num, 2:31])
    
    # Create long format data
    for(i in 1:length(years)) {
      if(!is.na(years[i]) && !is.na(values[i])) {
        row_data <- data.frame(
          sex = mapping$sex,
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
  
  # Clean and format
  result_data <- result_data %>%
    dplyr::filter(!is.na(rate) & !is.na(year)) %>%
    dplyr::rename(smoking_category = smoking_status) %>%
    dplyr::filter(!is.na(smoking_category)) %>%
    dplyr::group_by(sex, age_group, year) %>%
    dplyr::mutate(
      total = sum(rate, na.rm = TRUE),
      rate = if_else(total > 0, (rate / total) * 100, rate)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      year = as.numeric(year),
      rate = as.numeric(rate),
      age_group = factor(age_group, levels = c("16-24 years", "25-34 years", "35-44 years", 
                                               "45-54 years", "55-64 years", "65-74 years", 
                                               "75+ years")),
      sex = factor(sex),
      smoking_category = factor(smoking_category, levels = c("Current smoker", "Former smoker", "Never smoker"))
    ) %>%
    dplyr::select(sex, age_group, year, smoking_category, rate) %>%
    dplyr::arrange(sex, age_group, year, smoking_category)
  
  return(result_data)
}

#' Extract HSE BMI Data
#' 
#' Extracts longitudinal BMI category prevalence data by age group and sex
#' 
#' @return Data frame with columns: sex, age_group, year, bmi_category, rate
extract_bmi_data <- function() {
  
  hse_bmi_file_path <- "data/raw/Health_Survey_for_England/2022/HSE-2022-Overweight-and-obesity-tables.xlsx"
  data <- read_excel(hse_bmi_file_path, sheet = "Table 3")
  
  # Extract years from row 3, columns 2-31 (1993-2022)
  years <- as.numeric(data[3, 2:31])
  
  # Define exact row mappings for BMI categories
  row_mapping <- list(
    # Men - 16-24 years BMI categories
    list(sex = "Men", category = "% Underweight", age = "16-24 years", row = 6),
    list(sex = "Men", category = "% Healthy weight", age = "16-24 years", row = 7),
    list(sex = "Men", category = "% Overweight", age = "16-24 years", row = 8),
    list(sex = "Men", category = "% Obese", age = "16-24 years", row = 9),
    list(sex = "Men", category = "% Morbidly obese", age = "16-24 years", row = 10),
    
    # Men - 25-34 years BMI categories
    list(sex = "Men", category = "% Underweight", age = "25-34 years", row = 15),
    list(sex = "Men", category = "% Healthy weight", age = "25-34 years", row = 16),
    list(sex = "Men", category = "% Overweight", age = "25-34 years", row = 17),
    list(sex = "Men", category = "% Obese", age = "25-34 years", row = 18),
    list(sex = "Men", category = "% Morbidly obese", age = "25-34 years", row = 19),
    
    # Men - 35-44 years BMI categories
    list(sex = "Men", category = "% Underweight", age = "35-44 years", row = 24),
    list(sex = "Men", category = "% Healthy weight", age = "35-44 years", row = 25),
    list(sex = "Men", category = "% Overweight", age = "35-44 years", row = 26),
    list(sex = "Men", category = "% Obese", age = "35-44 years", row = 27),
    list(sex = "Men", category = "% Morbidly obese", age = "35-44 years", row = 28),
    
    # Men - 45-54 years BMI categories
    list(sex = "Men", category = "% Underweight", age = "45-54 years", row = 33),
    list(sex = "Men", category = "% Healthy weight", age = "45-54 years", row = 34),
    list(sex = "Men", category = "% Overweight", age = "45-54 years", row = 35),
    list(sex = "Men", category = "% Obese", age = "45-54 years", row = 36),
    list(sex = "Men", category = "% Morbidly obese", age = "45-54 years", row = 37),
    
    # Men - 55-64 years BMI categories
    list(sex = "Men", category = "% Underweight", age = "55-64 years", row = 42),
    list(sex = "Men", category = "% Healthy weight", age = "55-64 years", row = 43),
    list(sex = "Men", category = "% Overweight", age = "55-64 years", row = 44),
    list(sex = "Men", category = "% Obese", age = "55-64 years", row = 45),
    list(sex = "Men", category = "% Morbidly obese", age = "55-64 years", row = 46),
    
    # Men - 65-74 years BMI categories
    list(sex = "Men", category = "% Underweight", age = "65-74 years", row = 51),
    list(sex = "Men", category = "% Healthy weight", age = "65-74 years", row = 52),
    list(sex = "Men", category = "% Overweight", age = "65-74 years", row = 53),
    list(sex = "Men", category = "% Obese", age = "65-74 years", row = 54),
    list(sex = "Men", category = "% Morbidly obese", age = "65-74 years", row = 55),
    
    # Men - 75+ years BMI categories
    list(sex = "Men", category = "% Underweight", age = "75+ years", row = 60),
    list(sex = "Men", category = "% Healthy weight", age = "75+ years", row = 61),
    list(sex = "Men", category = "% Overweight", age = "75+ years", row = 62),
    list(sex = "Men", category = "% Obese", age = "75+ years", row = 63),
    list(sex = "Men", category = "% Morbidly obese", age = "75+ years", row = 64),
    
    # Women - 16-24 years BMI categories
    list(sex = "Women", category = "% Underweight", age = "16-24 years", row = 84),
    list(sex = "Women", category = "% Healthy weight", age = "16-24 years", row = 85),
    list(sex = "Women", category = "% Overweight", age = "16-24 years", row = 86),
    list(sex = "Women", category = "% Obese", age = "16-24 years", row = 87),
    list(sex = "Women", category = "% Morbidly obese", age = "16-24 years", row = 88),
    
    # Women - 25-34 years BMI categories
    list(sex = "Women", category = "% Underweight", age = "25-34 years", row = 93),
    list(sex = "Women", category = "% Healthy weight", age = "25-34 years", row = 94),
    list(sex = "Women", category = "% Overweight", age = "25-34 years", row = 95),
    list(sex = "Women", category = "% Obese", age = "25-34 years", row = 96),
    list(sex = "Women", category = "% Morbidly obese", age = "25-34 years", row = 97),
    
    # Women - 35-44 years BMI categories
    list(sex = "Women", category = "% Underweight", age = "35-44 years", row = 102),
    list(sex = "Women", category = "% Healthy weight", age = "35-44 years", row = 103),
    list(sex = "Women", category = "% Overweight", age = "35-44 years", row = 104),
    list(sex = "Women", category = "% Obese", age = "35-44 years", row = 105),
    list(sex = "Women", category = "% Morbidly obese", age = "35-44 years", row = 106),
    
    # Women - 45-54 years BMI categories
    list(sex = "Women", category = "% Underweight", age = "45-54 years", row = 111),
    list(sex = "Women", category = "% Healthy weight", age = "45-54 years", row = 112),
    list(sex = "Women", category = "% Overweight", age = "45-54 years", row = 113),
    list(sex = "Women", category = "% Obese", age = "45-54 years", row = 114),
    list(sex = "Women", category = "% Morbidly obese", age = "45-54 years", row = 115),
    
    # Women - 55-64 years BMI categories
    list(sex = "Women", category = "% Underweight", age = "55-64 years", row = 120),
    list(sex = "Women", category = "% Healthy weight", age = "55-64 years", row = 121),
    list(sex = "Women", category = "% Overweight", age = "55-64 years", row = 122),
    list(sex = "Women", category = "% Obese", age = "55-64 years", row = 123),
    list(sex = "Women", category = "% Morbidly obese", age = "55-64 years", row = 124),
    
    # Women - 65-74 years BMI categories
    list(sex = "Women", category = "% Underweight", age = "65-74 years", row = 129),
    list(sex = "Women", category = "% Healthy weight", age = "65-74 years", row = 130),
    list(sex = "Women", category = "% Overweight", age = "65-74 years", row = 131),
    list(sex = "Women", category = "% Obese", age = "65-74 years", row = 132),
    list(sex = "Women", category = "% Morbidly obese", age = "65-74 years", row = 133),
    
    # Women - 75+ years BMI categories
    list(sex = "Women", category = "% Underweight", age = "75+ years", row = 138),
    list(sex = "Women", category = "% Healthy weight", age = "75+ years", row = 139),
    list(sex = "Women", category = "% Overweight", age = "75+ years", row = 140),
    list(sex = "Women", category = "% Obese", age = "75+ years", row = 141),
    list(sex = "Women", category = "% Morbidly obese", age = "75+ years", row = 142)
  )
  
  # Extract data based on row mapping
  result_data <- data.frame()
  
  for(mapping in row_mapping) {
    row_num <- mapping$row
    values <- as.numeric(data[row_num, 2:31])
    
    # Create long format data
    for(i in 1:length(years)) {
      if(!is.na(years[i]) && !is.na(values[i])) {
        row_data <- data.frame(
          sex = mapping$sex,
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
  
  # Clean and convert to standardized categories
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
    dplyr::group_by(sex, age_group, year, bmi_category_new) %>%
    dplyr::summarise(rate = sum(rate, na.rm = TRUE), .groups = 'drop') %>%
    dplyr::group_by(sex, age_group, year) %>%
    dplyr::mutate(
      total = sum(rate, na.rm = TRUE),
      rate = if_else(total > 0, (rate / total) * 100, rate)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      year = as.numeric(year),
      rate = as.numeric(rate),
      age_group = factor(age_group, levels = c("16-24 years", "25-34 years", "35-44 years", 
                                               "45-54 years", "55-64 years", "65-74 years", 
                                               "75+ years")),
      sex = factor(sex),
      bmi_category = factor(bmi_category_new, levels = c("Healthy weight", "Overweight", "Obese"))
    ) %>%
    dplyr::select(sex, age_group, year, bmi_category, rate) %>%
    dplyr::arrange(sex, age_group, year, bmi_category)
  
  return(result_data)
}

#' Extract HSE Blood Pressure Data
#' 
#' Extracts longitudinal blood pressure category prevalence data by age group and sex
#' 
#' @return Data frame with columns: sex, age_group, year, bp_category, rate
extract_bp_data <- function() {
  
  # File path for HSE blood pressure data
  hse_bp_file_path <- "data/raw/Health_Survey_for_England/2022/HSE-2022-Adult-health-tables.xlsx"
  
  # Check if file exists
  if (!file.exists(hse_bp_file_path)) {
    stop("Excel file not found at: ", hse_bp_file_path)
  }
  
  # Read Table 12 (longitudinal) and Table 13 (age-disaggregated)
  table12_raw <- read_excel(hse_bp_file_path, sheet = "Table 12", col_names = FALSE)
  table13_raw <- read_excel(hse_bp_file_path, sheet = "Table 13", col_names = FALSE)
  
  # Define years and age groups
  years <- c(2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 
             2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 
             2019, 2021, 2022)
  age_groups <- c("16-34", "35-44", "45-54", "55-64", "65-74", "75+")
  bp_categories <- c("Normotensive untreated", "Hypertensive controlled", 
                     "Hypertensive uncontrolled", "Hypertensive untreated")
  
  # Extract longitudinal data (Table 12)
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
        sex = "Men",
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
        sex = "Women",
        year = years[valid_idx],
        bp_category = bp_categories[i],
        rate = rates[valid_idx],
        stringsAsFactors = FALSE
      )
      longitudinal_data <- rbind(longitudinal_data, new_data)
    }
  }
  
  # Extract age-disaggregated data (Table 13)
  age_data <- data.frame()
  
  # Men's age data (rows 7-10)
  for(i in 1:4) {
    row_data <- table13_raw[6 + i, 2:7]
    row_data[row_data == "-"] <- "0"
    row_data <- as.character(row_data)
    row_data[is.na(row_data) | row_data == "" | row_data == "-"] <- "0"
    age_pcts <- as.numeric(row_data)
    
    new_data <- data.frame(
      sex = "Men",
      age_group = age_groups,
      bp_category = bp_categories[i],
      age_pct = age_pcts,
      stringsAsFactors = FALSE
    )
    age_data <- rbind(age_data, new_data)
  }
  
  # Women's age data (rows 14-17)
  for(i in 1:4) {
    row_data <- table13_raw[13 + i, 2:7]
    row_data[row_data == "-"] <- "0"
    row_data <- as.character(row_data)
    row_data[is.na(row_data) | row_data == "" | row_data == "-"] <- "0"
    age_pcts <- as.numeric(row_data)
    
    new_data <- data.frame(
      sex = "Women",
      age_group = age_groups,
      bp_category = bp_categories[i],
      age_pct = age_pcts,
      stringsAsFactors = FALSE
    )
    age_data <- rbind(age_data, new_data)
  }
  
  # Calculate age distributions within each BP category
  age_distributions <- data.frame()
  
  for(sex_val in c("Men", "Women")) {
    for(cat_val in bp_categories) {
      subset_data <- age_data[age_data$sex == sex_val & age_data$bp_category == cat_val, ]
      total_pct <- sum(subset_data$age_pct, na.rm = TRUE)
      
      if(total_pct > 0) {
        subset_data$age_distribution <- subset_data$age_pct / total_pct
      } else {
        subset_data$age_distribution <- 1 / length(age_groups)
      }
      
      age_distributions <- rbind(age_distributions, subset_data)
    }
  }
  
  # Combine longitudinal rates with age distributions
  final_data <- data.frame()
  
  for(i in 1:nrow(longitudinal_data)) {
    sex_val <- longitudinal_data$sex[i]
    year_val <- longitudinal_data$year[i]
    cat_val <- longitudinal_data$bp_category[i]
    overall_rate <- longitudinal_data$rate[i]
    
    age_dist <- age_distributions[
      age_distributions$sex == sex_val & 
        age_distributions$bp_category == cat_val, 
      c("age_group", "age_distribution")]
    
    age_specific_rates <- data.frame(
      sex = sex_val,
      age_group = age_dist$age_group,
      year = year_val,
      bp_category = cat_val,
      rate = overall_rate * age_dist$age_distribution,
      stringsAsFactors = FALSE
    )
    
    final_data <- rbind(final_data, age_specific_rates)
  }
  
  final_data <- final_data %>%
    mutate(
      age_group = case_when(
        age_group == "16-34" ~ "16-34 years",
        age_group == "35-44" ~ "35-44 years", 
        age_group == "45-54" ~ "45-54 years",
        age_group == "55-64" ~ "55-64 years",
        age_group == "65-74" ~ "65-74 years",
        age_group == "75+" ~ "75+ years"
      )
    ) %>%
    filter(!is.na(age_group)) %>%
    mutate(
      year = as.numeric(year),
      rate = as.numeric(rate) * 100,
      age_group = factor(age_group, levels = c("16-34 years", "35-44 years", "45-54 years", 
                                               "55-64 years", "65-74 years", "75+ years")),
      sex = factor(sex),
      bp_category = factor(bp_category, levels = bp_categories)
    ) %>%
    dplyr::select(sex, age_group, year, bp_category, rate) %>%
    arrange(sex, age_group, year, bp_category)
  
  return(final_data)
}


#' Main HSE Data Extraction Function
#' 
#' Extracts and processes all HSE risk factor data for analysis
#' 
#' @return List containing processed datasets for smoking, BMI, and blood pressure
#' @export
extract_hse_longitudinal_data <- function() {
  
  # Extract all datasets
  hse_smoking_data <- extract_smoking_data()
  hse_bmi_data <- extract_bmi_data() 
  hse_bp_data <- extract_bp_data()
  
  # Return as named list
  return(list(
    smoking = hse_smoking_data,
    bmi = hse_bmi_data,
    sbp = hse_bp_data
  ))
}

# Example usage:
# hse_data <- extract_hse_longitudinal_data()
# smoking_data <- hse_data$smoking
# bmi_data <- hse_data$bmi
# sbp_data <- hse_data$sbp