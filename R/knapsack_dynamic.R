#' 0-1 Knapsack Problem Solver (Dynamic optimal)
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
#'
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
  return(round(V(1, W)))
}
