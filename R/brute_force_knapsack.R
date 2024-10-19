#' Brute-force Knapsack Problem Solver
#'
#' This function solves the knapsack problem using a brute-force approach.
#' It checks all possible combinations of elements to determine the one that
#' maximizes the total value without exceeding the maximum weight capacity.
#'
#' @param x A data frame with two columns:
#'   - `w`: Weights of the items.
#'   - `v`: Values of the items.
#' @param W A numeric value representing the maximum weight capacity of the knapsack.
#'
#' @return A list with two elements:
#'   - `value`: The maximum value achievable without exceeding the weight limit.
#'   - `elements`: The indices of the items selected for the optimal solution.
#'
#' @details
#' The function uses a brute-force method to solve the knapsack problem by checking
#' all possible subsets of items. For each subset, it calculates the total weight
#' and value, and if the weight is within the limit and the value is higher than the
#' previously found solutions, it updates the best solution.
#'
#' @examples
#' # Example usage:
#' items <- data.frame(w = c(2, 3, 4, 5), v = c(3, 4, 5, 6))
#' W <- 5
#' brute_force_knapsack(items, W)
#'
#' @export
brute_force_knapsack <- function(x, W) {
  if (!is.data.frame(x) || ncol(x) < 2) {
    stop("Input x must be a data frame with at least two columns.")
  }
  if (W < 0) {
    stop("Weight capacity W must be a non-negative value.")
  }

  # Number of items
  n <- nrow(x)

  # Initialize solution variables
  value <- 0
  elements <- vector()

  # Iterate over all possible combinations (2^n - 1 subsets)
  for (i in 1:(2^n - 1)) {


    mask <- as.logical(intToBits(i)[1:n])
    x_true <- x[mask, ]

    x_true_weight <- sum(x_true$w)
    x_true_value <- round(sum(x_true$v))

    # Check if this subset is a valid and better solution
    if (x_true_weight <= W & x_true_value > value) {
      value <- x_true_value
      elements <- which(mask) # Store the indices of the selected items
    }
  }

  return(list(value = value, elements = elements))
}


# QUESTION: How much time does it takes to run the algorithm for n = 16 objects?

# ANSWER: Using the function `system.time` (which returns the difference between two proc.time calls,
# one before executing the given expression and the other one after), we get

# system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))


# code with parallelization

library(parallel)

brute_force_knapsack <- function(x, W, parallel = FALSE) {

  if (W < 0) {
    stop("Weight capacity W must be a non-negative value.")
  }
  
  # Number of items
  n <- nrow(x)
  
  if (parallel == FALSE) {
    
    # Initialize solution variables
    value <- 0
    elements <- vector()
    
    # Iterate over all possible combinations (2^n - 1 subsets)
    for (i in 1:(2^n - 1)) {
      
      mask <- as.logical(intToBits(i)[1:n])
      x_true <- x[mask, ]
      
      x_true_weight <- sum(x_true$w)
      x_true_value <- round(sum(x_true$v))
      
      # Check if this subset is a valid and better solution
      if (x_true_weight <= W & x_true_value > value) {
        value <- x_true_value
        elements <- which(mask) # Store the indices of the selected items
      }
    }
    return(list(value = value, elements = elements))
    
  } else {
    
    # Parallel implementation
    x$id <- 1:n
    
    # Detect number of available cores
    allcores <- detectCores()
    
    # Create a cluster with the detected number of cores
    cluster <- makeCluster(allcores)
    
    # Export the required variables to the cluster
    clusterExport(cluster, varlist = c("x", "W", "n"), envir = environment())
    
    # Generate all possible combinations in parallel
    all_combinations <- parLapply(cluster, 1:n, function(i) {
      combn(1:n, i, simplify = FALSE)
    })
    
    # Flatten the list of combinations
    all_combinations <- unlist(all_combinations, recursive = FALSE)
    
    # Compute weights and values for each combination in parallel
    all_results <- parLapply(cluster, all_combinations, function(combo) {
      combo_weight <- sum(x$w[combo])
      combo_value <- sum(x$v[combo])
      list(weight = combo_weight, value = combo_value, elements = combo)
    })
    
    # Filter valid combinations that do not exceed the weight limit
    valid_results <- Filter(function(res) res$weight <= W, all_results)
    
    # Find the combination with the maximum value
    if (length(valid_results) > 0) {
      best_result <- valid_results[[which.max(sapply(valid_results, function(res) res$value))]]
      best_value <- best_result$value
      best_elements <- best_result$elements
    } else {
      best_value <- 0
      best_elements <- integer(0)
    }
    
    # Stop the cluster
    stopCluster(cluster)
    
    # Return the best value and corresponding elements
    return(list(value = round(best_value), elements = best_elements))
  }
}


test_that("functions rejects erroneous input", {
  expect_error(brute_force_knapsack(x = knapsack_objects[1:8, ], W = -3500), 
               "Weight capacity W must be a non-negative value.")
})
