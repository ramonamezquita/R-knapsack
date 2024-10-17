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




# QUESTION: How much time does it takes to run the algorithm for n = 16 objects?

# ANSWER: Using the function `system.time` (which returns the difference between two proc.time calls,
# one before executing the given expression and the other one after), we get

# system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))






#' 0-1 Knapsack Problem Solver (Dynamic sub-optimal)
#'
#' @param x A data frame with two columns: \code{w} (weights) and \code{v} (values) of the items.
#' @param W The total capacity of the knapsack.
#'
#' @return The maximum total value that can be obtained.
#'
#' @details
#' This is not the most optimal dynamic programming version of the knapsack problem.
#' In an optimal solution, once we have selected a fixed number of items, there
#' is no need to recompute for all possible weights, as the number of items and
#' the chosen items are already fixed.
#' A more efficient dynamic programming approach would eliminate unnecessary
#' computations, focusing only on the possible states directly relevant to the
#' final solution.
#'
#' @references
#' https://en.wikipedia.org/wiki/Knapsack_problem#0.2F1_knapsack_problem
suboptimal_knapsack_dynamic <- function(x, W) {

  N <- nrow(x)

  # Results table.
  m <- matrix(-1, nrow = N + 1, ncol = W + 1)

  # Base case or Boundary conditions.
  m[, 1] <- 0
  m[1, ] <- 0

  for (k in 2:(N + 1)) {

    # Weight and value for stage/item k
    w_k = x$w[k - 1]
    v_k = x$v[k - 1]

    for (i in 2:(W + 1)) {

      if (w_k > i) {
        m[k, i] <- m[k - 1, i]
      } else {
        m[k, i] <- max(m[k - 1, i], m[k - 1, i - w_k] + v_k)
      }
    }
  }
  return(m)
}






optimal_knapsack_dynamic <- function(x, W) {

  N <- nrow(x)

  # Define value[n, W]
  value <- matrix(-1, nrow = N, ncol = W)

  m <- function(i, j) {

    # Return if already computed
    if (value[i, j] != -1) return(value[i, j])

    # Base case or Boundary condition.
    # m[0, j] = 0 : The max value of a knapsack with no items is 0.
    # m[i, 0] = 0 : The max value of a knapsack with no capacity is 0.
    if (i == 0 || j <= 0) {
      value[i, j] <<- 0
    }

    # Weight and value for i-th item.
    w_i <- x$w[i]
    v_i <- x$v[i]

    # Case 1: Item cannot fit in the bag
    if (w_i > j) {
      value[i - 1, j] <<- m(i - 1, j)
      value[i, j] <<- value[i - 1, j]

      # Case 2: Item can fit in the bag
    } else {

      m_ij <- max(m(i - 1, j), m(i - 1, j - w_i) + v_i)
    }
  }
}

