#' Monte Carlo Confidence Intervals for Microsimulation Results
#'
#' This module provides functions to run multiple replications of the microsimulation
#' and calculate confidence intervals for aggregate outcomes.

library(dplyr)
library(parallel)
library(future)
library(future.apply)

#' Run Multiple Microsimulation Replications
#'
#' Execute the microsimulation multiple times with different random seeds
#' to generate a distribution of outcomes for confidence interval calculation
#'
#' @param n_replications Number of simulation replications to run
#' @param base_seed Base random seed (each replication gets base_seed + i)
#' @param n_cores Number of CPU cores to use for parallel processing (NULL = auto-detect)
#' @param save_individual_outputs Whether to save model_outputs from each replication
#' @param progress_callback Function to call for progress updates
#' @param ... All other parameters passed to run_microsimulation()
#' @return List containing replicated results and summary statistics

run_microsimulation_replications <- function(n_replications = 100,
                                             base_seed = 9001,
                                             n_cores = NULL,
                                             save_individual_outputs = FALSE,
                                             progress_callback = NULL,
                                             ...) {
  
  cat(sprintf("Running %d microsimulation replications...\n", n_replications))
  
  # Set up parallel processing
  if (is.null(n_cores)) {
    n_cores <- min(parallel::detectCores() - 1, n_replications)
  }
  
  cat(sprintf("Using %d CPU cores\n", n_cores))
  
  # Set up future for parallel processing
  if (n_cores > 1) {
    future::plan(future::multisession, workers = n_cores)
  } else {
    future::plan(future::sequential)
  }
  
  # Generate seeds for each replication
  replication_seeds <- base_seed + (1:n_replications)
  
  # Define the single replication function
  run_single_replication <- function(rep_number, seed, ...) {
    
    if (!is.null(progress_callback)) {
      progress_callback(rep_number, n_replications)
    }
    
    # Run the microsimulation with this seed
    result <- tryCatch({
      run_microsimulation(seed = seed, ...)
    }, error = function(e) {
      warning(sprintf("Replication %d failed with error: %s", rep_number, e$message))
      return(NULL)
    })
    
    if (is.null(result)) {
      return(list(
        replication = rep_number,
        seed = seed,
        success = FALSE,
        error = TRUE
      ))
    }
    
    # Extract key aggregate outcomes
    model_outputs <- result$model_outputs
    
    # Calculate total outcomes across all years
    total_outcomes <- list(
      replication = rep_number,
      seed = seed,
      success = TRUE,
      error = FALSE,
      
      # Total outcomes over simulation period
      total_life_years = sum(model_outputs$life_years, na.rm = TRUE),
      total_qalys = sum(model_outputs$total_qalys, na.rm = TRUE),
      total_costs = sum(model_outputs$total_costs, na.rm = TRUE),
      total_direct_costs = sum(model_outputs$total_direct_costs, na.rm = TRUE),
      total_indirect_costs = sum(model_outputs$total_indirect_costs, na.rm = TRUE),
      
      # Population dynamics
      total_births = sum(model_outputs$births, na.rm = TRUE),
      total_deaths = sum(model_outputs$deaths_total, na.rm = TRUE),
      final_population = model_outputs$alive_population[nrow(model_outputs)],
      
      # Disease burden (total incident cases)
      total_chd_cases = sum(model_outputs$chd_incidence_count, na.rm = TRUE),
      total_stroke_cases = sum(model_outputs$stroke_incidence_count, na.rm = TRUE),
      total_copd_cases = sum(model_outputs$copd_incidence_count, na.rm = TRUE),
      total_lung_cancer_cases = sum(model_outputs$lung_cancer_incidence_count, na.rm = TRUE),
      total_colorectal_cancer_cases = sum(model_outputs$colorectal_cancer_incidence_count, na.rm = TRUE),
      
      # Final year prevalence
      final_chd_prevalence = model_outputs$chd_prevalence[nrow(model_outputs)],
      final_stroke_prevalence = model_outputs$stroke_prevalence[nrow(model_outputs)],
      final_copd_prevalence = model_outputs$copd_prevalence[nrow(model_outputs)],
      final_lung_cancer_prevalence = model_outputs$lung_cancer_prevalence[nrow(model_outputs)],
      final_colorectal_cancer_prevalence = model_outputs$colorectal_cancer_prevalence[nrow(model_outputs)],
      
      # Deaths by cause (totals)
      total_deaths_other_cause = sum(model_outputs$deaths_other_cause, na.rm = TRUE),
      total_deaths_chd = sum(model_outputs$deaths_chd, na.rm = TRUE),
      total_deaths_stroke = sum(model_outputs$deaths_stroke, na.rm = TRUE),
      total_deaths_copd = sum(model_outputs$deaths_copd, na.rm = TRUE),
      total_deaths_lung_cancer = sum(model_outputs$deaths_lung_cancer, na.rm = TRUE),
      total_deaths_colorectal_cancer = sum(model_outputs$deaths_colorectal_cancer, na.rm = TRUE),
      
      # Average annual outcomes
      mean_annual_qalys = mean(model_outputs$total_qalys, na.rm = TRUE),
      mean_annual_costs = mean(model_outputs$total_costs, na.rm = TRUE),
      mean_annual_population = mean(model_outputs$alive_population, na.rm = TRUE)
    )
    
    # Optionally include full model outputs
    if (save_individual_outputs) {
      total_outcomes$model_outputs <- model_outputs
    }
    
    return(total_outcomes)
  }
  
  # Progress callback function
  default_progress <- function(current, total) {
    if (current %% max(1, floor(total / 20)) == 0 || current == total) {
      cat(sprintf("Completed %d/%d replications (%.1f%%)\n", 
                  current, total, 100 * current / total))
    }
  }
  
  if (is.null(progress_callback)) {
    progress_callback <- default_progress
  }
  
  # Run replications in parallel
  cat("Starting replications...\n")
  start_time <- Sys.time()
  
  replication_results <- future.apply::future_lapply(
    1:n_replications,
    function(i) run_single_replication(i, replication_seeds[i], ...),
    future.seed = TRUE
  )
  
  end_time <- Sys.time()
  cat(sprintf("Completed %d replications in %.2f minutes\n", 
              n_replications, as.numeric(difftime(end_time, start_time, units = "mins"))))
  
  # Reset future plan
  future::plan(future::sequential)
  
  # Convert to data frame and check for errors
  results_df <- do.call(rbind, lapply(replication_results, as.data.frame))
  
  n_successful <- sum(results_df$success)
  n_failed <- sum(!results_df$success)
  
  cat(sprintf("Successful replications: %d/%d (%.1f%%)\n", 
              n_successful, n_replications, 100 * n_successful / n_replications))
  
  if (n_failed > 0) {
    cat(sprintf("Failed replications: %d\n", n_failed))
  }
  
  # Filter to successful replications only
  successful_results <- results_df[results_df$success, ]
  
  return(list(
    replication_results = if (save_individual_outputs) replication_results else NULL,
    summary_results = successful_results,
    n_replications = n_replications,
    n_successful = n_successful,
    n_failed = n_failed,
    runtime_minutes = as.numeric(difftime(end_time, start_time, units = "mins")),
    parameters = list(...)
  ))
}


#' Calculate Confidence Intervals for Simulation Results
#'
#' Calculate percentile-based confidence intervals from multiple simulation replications
#'
#' @param replication_summary Summary results from run_microsimulation_replications
#' @param confidence_level Confidence level (e.g., 0.95 for 95% CI)
#' @param outcomes_to_analyze Vector of outcome names to analyze (NULL = all numeric outcomes)
#' @return Data frame with mean, median, and confidence intervals for each outcome

calculate_confidence_intervals <- function(replication_summary, 
                                           confidence_level = 0.95,
                                           outcomes_to_analyze = NULL) {
  
  if (!"summary_results" %in% names(replication_summary)) {
    stop("Input must be output from run_microsimulation_replications()")
  }
  
  results_df <- replication_summary$summary_results
  
  if (nrow(results_df) == 0) {
    stop("No successful replications found")
  }
  
  # Identify numeric outcome columns
  numeric_cols <- sapply(results_df, is.numeric)
  exclude_cols <- c("replication", "seed", "success", "error")
  outcome_cols <- names(results_df)[numeric_cols & !names(results_df) %in% exclude_cols]
  
  if (!is.null(outcomes_to_analyze)) {
    outcome_cols <- intersect(outcome_cols, outcomes_to_analyze)
  }
  
  if (length(outcome_cols) == 0) {
    stop("No valid numeric outcome columns found")
  }
  
  cat(sprintf("Calculating confidence intervals for %d outcomes from %d replications\n",
              length(outcome_cols), nrow(results_df)))
  cat(sprintf("Confidence level: %.1f%%\n", confidence_level * 100))
  
  # Calculate percentiles
  alpha <- 1 - confidence_level
  lower_percentile <- alpha / 2
  upper_percentile <- 1 - (alpha / 2)
  
  ci_results <- data.frame(
    outcome = character(0),
    n_replications = integer(0),
    mean = numeric(0),
    median = numeric(0),
    std_dev = numeric(0),
    min = numeric(0),
    max = numeric(0),
    lower_ci = numeric(0),
    upper_ci = numeric(0),
    ci_width = numeric(0),
    cv_percent = numeric(0),
    stringsAsFactors = FALSE
  )
  
  for (outcome in outcome_cols) {
    values <- results_df[[outcome]]
    values <- values[!is.na(values)]  # Remove any NA values
    
    if (length(values) == 0) {
      warning(sprintf("No valid values for outcome: %s", outcome))
      next
    }
    
    outcome_mean <- mean(values)
    outcome_median <- median(values)
    outcome_sd <- sd(values)
    outcome_min <- min(values)
    outcome_max <- max(values)
    
    # Calculate confidence interval
    ci_lower <- quantile(values, lower_percentile, na.rm = TRUE)
    ci_upper <- quantile(values, upper_percentile, na.rm = TRUE)
    ci_width <- ci_upper - ci_lower
    
    # Coefficient of variation (CV) as percentage
    cv_percent <- ifelse(outcome_mean != 0, (outcome_sd / abs(outcome_mean)) * 100, NA)
    
    # Add to results
    ci_results <- rbind(ci_results, data.frame(
      outcome = outcome,
      n_replications = length(values),
      mean = outcome_mean,
      median = outcome_median,
      std_dev = outcome_sd,
      min = outcome_min,
      max = outcome_max,
      lower_ci = ci_lower,
      upper_ci = ci_upper,
      ci_width = ci_width,
      cv_percent = cv_percent,
      stringsAsFactors = FALSE
    ))
  }
  
  # Sort by outcome name
  ci_results <- ci_results[order(ci_results$outcome), ]
  rownames(ci_results) <- NULL
  
  return(ci_results)
}


#' Print Confidence Interval Results
#'
#' Pretty print confidence interval results with formatting
#'
#' @param ci_results Output from calculate_confidence_intervals
#' @param outcomes_to_show Vector of outcome names to display (NULL = all)
#' @param currency_outcomes Vector of outcome names that represent currency values

print_confidence_intervals <- function(ci_results, 
                                       outcomes_to_show = NULL,
                                       currency_outcomes = c("total_costs", "total_direct_costs", 
                                                             "total_indirect_costs", "mean_annual_costs")) {
  
  if (!is.null(outcomes_to_show)) {
    ci_results <- ci_results[ci_results$outcome %in% outcomes_to_show, ]
  }
  
  if (nrow(ci_results) == 0) {
    cat("No results to display\n")
    return()
  }
  
  confidence_level <- (1 - 2 * (ci_results$lower_ci[1] - min(ci_results$mean))) * 100
  cat(sprintf("=== Confidence Intervals (%.0f%% level) ===\n\n", confidence_level))
  
  for (i in 1:nrow(ci_results)) {
    row <- ci_results[i, ]
    outcome_name <- gsub("_", " ", row$outcome)
    outcome_name <- tools::toTitleCase(outcome_name)
    
    # Format numbers based on outcome type
    if (row$outcome %in% currency_outcomes) {
      # Currency formatting
      mean_str <- sprintf("£%.0f", row$mean)
      ci_str <- sprintf("£%.0f - £%.0f", row$lower_ci, row$upper_ci)
    } else if (grepl("prevalence", row$outcome)) {
      # Percentage formatting for prevalence
      mean_str <- sprintf("%.2f%%", row$mean)
      ci_str <- sprintf("%.2f%% - %.2f%%", row$lower_ci, row$upper_ci)
    } else if (grepl("rate", row$outcome)) {
      # Rate formatting
      mean_str <- sprintf("%.2f", row$mean)
      ci_str <- sprintf("%.2f - %.2f", row$lower_ci, row$upper_ci)
    } else {
      # Default number formatting
      if (row$mean >= 1000) {
        mean_str <- sprintf("%.0f", row$mean)
        ci_str <- sprintf("%.0f - %.0f", row$lower_ci, row$upper_ci)
      } else {
        mean_str <- sprintf("%.2f", row$mean)
        ci_str <- sprintf("%.2f - %.2f", row$lower_ci, row$upper_ci)
      }
    }
    
    cat(sprintf("%-35s: %s [%s] (CV: %.1f%%)\n", 
                outcome_name, mean_str, ci_str, row$cv_percent))
  }
  
  cat(sprintf("\nBased on %d successful replications\n", ci_results$n_replications[1]))
  cat("CI = Confidence Interval, CV = Coefficient of Variation\n")
}


#' Save Confidence Interval Results
#'
#' Save CI results to CSV file with formatted output
#'
#' @param ci_results Output from calculate_confidence_intervals
#' @param filename Output filename
#' @param include_raw_data Whether to include the raw replication data

save_confidence_intervals <- function(ci_results, filename, include_raw_data = FALSE) {
  
  # Create formatted version for reporting
  formatted_results <- ci_results %>%
    mutate(
      confidence_interval = sprintf("%.2f - %.2f", lower_ci, upper_ci),
      mean_formatted = ifelse(grepl("cost", outcome), 
                              sprintf("£%.0f", mean),
                              sprintf("%.2f", mean))
    ) %>%
    select(outcome, n_replications, mean, mean_formatted, confidence_interval, 
           std_dev, cv_percent, lower_ci, upper_ci, ci_width)
  
  write.csv(formatted_results, filename, row.names = FALSE)
  cat(sprintf("Confidence interval results saved to: %s\n", filename))
}


#' Plot Confidence Intervals
#'
#' Create visualization of confidence intervals for key outcomes
#'
#' @param ci_results Output from calculate_confidence_intervals
#' @param outcomes_to_plot Vector of outcomes to plot
#' @param title Plot title

plot_confidence_intervals <- function(ci_results, 
                                      outcomes_to_plot = c("total_qalys", "total_costs", 
                                                           "total_life_years"),
                                      title = "95% Confidence Intervals for Key Outcomes") {
  
  require(ggplot2)
  
  plot_data <- ci_results[ci_results$outcome %in% outcomes_to_plot, ]
  
  if (nrow(plot_data) == 0) {
    stop("No matching outcomes found for plotting")
  }
  
  # Create outcome labels
  plot_data$outcome_label <- tools::toTitleCase(gsub("_", " ", plot_data$outcome))
  
  p <- ggplot(plot_data, aes(x = reorder(outcome_label, mean), y = mean)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.3, size = 1) +
    coord_flip() +
    theme_minimal() +
    labs(
      title = title,
      x = "Outcome",
      y = "Value",
      caption = sprintf("Based on %d replications", plot_data$n_replications[1])
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}