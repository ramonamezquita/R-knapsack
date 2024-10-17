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
