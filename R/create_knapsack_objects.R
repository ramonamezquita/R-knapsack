#' Create Knapsack Objects
#'
#' This function generates a data frame of objects to be used by knapsack solvers.
#' The objects have randomly assigned weights and values.
#' It ensures reproducibility by using a specific random number generator (Mersenne-Twister) and setting a seed.
#' The function also enforces compatibility with R versions up to 3.5.3.
#'
#' @param n Integer. The number of objects to generate. Default is 2000.
#' @return A data frame with `n` rows and two columns:
#' \item{w}{Integer. Randomly generated weights for the knapsack objects (1 to 4000).}
#' \item{v}{Numeric. Randomly generated values for the knapsack objects (0 to 10000).}
#'
#' @details
#' The function uses the `Mersenne-Twister` random number generator with `Inversion` for normal generation.
#' The random seed is set to 42 for reproducibility, ensuring that the same set of objects is created each time.
#' The `RNGversion()` function is used to enforce compatibility with R versions 3.5.3 and below.
#'
#' @examples
#' # Generate a data frame of 2000 knapsack objects
#' knapsack_objects <- create_knapsack_objects()
#'
#' # Generate a data frame of 100 objects
#' knapsack_objects_100 <- create_knapsack_objects(n = 100)
#' 
#' @export
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
