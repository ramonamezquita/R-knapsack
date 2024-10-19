#' Greedy heuristic algorithm Knapsack Solver
#'
#' @param x A data frame with two columns: \code{w} (weights) and \code{v} (values) of the items.
#' @param W The total capacity of the knapsack.
#'
#' @return A list containing:
#' \item{value}{The total value of the selected items, rounded to the nearest integer.}
#' \item{elements}{A vector of the original indices of the selected items.}
#' @details
#' The algorithm follows a greedy heuristic, which sorts the items
#' by their value-to-weight ratio in descending order and adds them to the
#' knapsack if their inclusion does not exceed the weight capacity.
#' This approach does not guarantee an optimal solution but is efficient.
#'
#' @references
#' https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm
#'
#' @export
greedy_knapsack <- function(x, W) {

  # Check input
  if (!is.data.frame(x) || ncol(x) < 2) {
    stop("Input x must be a data frame with at least two columns.")
  }
  
  # Check for non-negative weight capacity
  if (!is.numeric(W) || W < 0) {
    stop("Weight capacity W must be a non-negative value.")
  }
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

