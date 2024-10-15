create_knapsack_objects <- function(n = 2000) {

  RNGversion(min(as.character(getRversion()),"3.5.3"))
  set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")

  knapsack_objects <-
    data.frame(
      w=sample(1:4000, size = n, replace = TRUE),
      v=runif(n = n, 0, 10000)
    )

  return(knapsack_objects)

}


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

# Using the function `system.time` (which returns the difference between two proc.time calls,
# one before executing the given expression and the other one after), we get

# system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))




#' Dynamic Programming Knapsack Solver
#'
#' This function solves the knapsack problem using a dynamic programming approach.
#' It finds the optimal subset of items that maximizes the total value without exceeding
#' the weight capacity.
#'
#' @param x A data frame with two columns:
#'   - `w`: Weights of the items.
#'   - `v`: Values of the items.
#' @param W A numeric value representing the maximum weight capacity of the knapsack.
#'
#' @return The maximum value that can be obtained within the weight limit.
#'
#' @details
#' The function uses a recursive approach through dynamic programming. For each stage `k`,
#' representing an item, it calculates the maximum value that can be obtained considering
#' the current item and the remaining capacity `i`. The decision at each stage is whether to
#' take or leave the item, depending on whether it fits in the remaining capacity.
#'
#' The function works backward from the last item to the first, recursively evaluating both
#' decisions (take or leave) and selecting the one that provides the higher value, provided
#' the weight constraint is respected.
#'
#' @examples
#' # Example usage:
#' items <- data.frame(w = c(3, 8, 5), v = c(4, 6, 5))
#' W <- 8
#' knapsack_dynamic(items, W)  # 9
#' @export
knapsack_dynamic <- function(x, W) {

  N <- nrow(x)

  # Memoization table
  memo <- matrix(-1, nrow = N, ncol = W + 1)

  #' Recursive relation for 0-1 Knapsack problem with memoization.
  #'
  #' @param k Stage/item number.
  #' @param i Knapsack remaining capacity (the state of the problem).
  V <- function(k, i) {

    # Base case: if there are no items left or no remaining capacity
    if (k > N || i == 0) {
      return(0)
    }

    # Check if result is already computed
    if (memo[k, i] != -1) {
      return(memo[k, i])
    }

    # Weight and value for stage/item k
    w_k <- x$w[k]
    v_k <- x$v[k]

    # If there is not enough capacity for the current item, skip it
    if (w_k > i) {
      memo[k, i] <<- V(k + 1, i)
    } else {
      # Take the maximum of not taking the item or taking the item
      memo[k, i] <<- max(V(k + 1, i), V(k + 1, i - w_k) + v_k)
    }

    return(memo[k, i])
  }

  # Start the recursion from the first item and the full capacity
  return(V(1, W))
}


#'  Greedy heuristic algorithm Knapsack Solver
#'
#' @param x 
#' @param W 
#'
#' @return
#' @export
#'
#' @examples
#' 


greedy_knapsack <- function(x, W) {
  
  # Check input
  stopifnot(is.data.frame(x), all(x$v >= 0, na.rm = TRUE), all(x$w >= 0, na.rm = TRUE), ncol(x) >= 2)
  
  # Create a new column with the value-to-weight ratio
  x$ratio <- x$v / x$w
  
  # Add a column of the original indices
  x$index <- 1:nrow(x)
  
  # Sort items by ratio in decreasing order
  x <- x[order(x$ratio, decreasing = T), ]
  
  # Initialize value and weight
  value <- 0
  weight <- 0
  elements <- c()
  
  for (i in 1:nrow(x)) {
    
    # Check if adding the item would exceed the capacity
    if (weight + x$w[i] <= W) {
      value <- value + x$v[i]   # update value
      weight <- weight + x$w[i]
      elements <- c(elements, x$index[i])  # Store the original index of the item
    } else {
      break  # Stop if the next item cannot be added
    }
  }
  
  # Return the total value and the selected elements
  return(list(value = round(value), elements = elements))
}




greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)


knapsack_objects <- create_knapsack_objects()[1:500,]
#knapsack_objects <- data.frame(w = c(3, 8, 5), v = c(4, 6, 5))
W = 200000

brute_force_knapsack(x = knapsack_objects, W = W)

gpt_knapsack_dynamic(x = knapsack_objects, W = W)
knapsack_dynamic(x = knapsack_objects, W = W)






