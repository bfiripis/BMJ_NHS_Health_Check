#' Apply Smoking State Transitions
#'  (testing version used static parameters prior to defining matrix algebra for transition matrix)
#' 
#' Applies annual smoking state transitions to individuals based on assumed
#' transition probabilities. Uses a simple 3-state model: Never, Former, Current.
#' 
#' @param population Data frame containing individual-level population data
#' @param seed Random seed for reproducibility
#' 
#' @return Data frame with updated smoking_status for all individuals
#' 
#' @details
#' Assumed annual transition probabilities:
#' - Never smoker -> Current smoker: 2% (initiation)
#' - Current smoker -> Former smoker: 8% (cessation)
#' - Former smoker -> Current smoker: 3% (relapse)
#' - All other transitions: remain in same state
#' 
#' @export
apply_smoking_transitions <- function(population, seed = NULL) {
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Assumed annual transition probabilities
  prob_initiation <- 0.02   # Never -> Current (2% per year)
  prob_cessation <- 0.08    # Current -> Former (8% per year) 
  prob_relapse <- 0.03      # Former -> Current (3% per year)
  
  # Generate random numbers for each individual
  random_probs <- runif(nrow(population))
  
  # Apply transitions based on current smoking status
  for (i in 1:nrow(population)) {
    
    current_status <- population$smoking_status[i]
    random_prob <- random_probs[i]
    
    # Apply appropriate transition probability based on current state
    if (current_status == "Never smoker") {
      # Probability of initiation (never -> current)
      if (random_prob < prob_initiation) {
        population$smoking_status[i] <- "Current smoker"
      }
      # Otherwise remain never smoker
      
    } else if (current_status == "Current smoker") {
      # Probability of cessation (current -> former)
      if (random_prob < prob_cessation) {
        population$smoking_status[i] <- "Former smoker"
      }
      # Otherwise remain current smoker
      
    } else if (current_status == "Former smoker") {
      # Probability of relapse (former -> current)
      if (random_prob < prob_relapse) {
        population$smoking_status[i] <- "Current smoker"
      }
      # Otherwise remain former smoker
    }
  }
  
  return(population)
}
# Example usage:
# population <- apply_smoking_transitions(population, seed = 12345)