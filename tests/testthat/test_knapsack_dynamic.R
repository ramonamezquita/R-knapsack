library(testhat)

context("knapsack_dynamic")

#example data that we will do the test
suppressWarnings(RNGversion(min(as.character(getRversion()),"3.5.3")))
set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000)
)

test_that("Correct object is returned", {
  expect_silent(kd <- knapsack_dynamic(x = knapsack_objects[1:8, ], W = 3500))
  expect_named(kd, c("value", "elements"))
})

test_that("Function rejects erroneous input.", {
  expect_error(knapsack_dynamic("hej", 3500))
  expect_error(knapsack_dynamic(x = knapsack_objects[1:8, ], W = -3500))
})

test_that("Function returns correct results.", {
  kd <- knapsack_dynamic(x = knapsack_objects[1:8, ], W = 3500)
  expect_equal(round(kd$value), 15428)  
  expect_true(all(round(kd$elements) %in% c(5, 8)))
  
  kd <- knapsack_dynamic(x = knapsack_objects[1:12, ], W = 3500)
  expect_equal(round(kd$value), 15428)
  expect_true(all(round(kd$elements) %in% c(5, 8)))
  
  kd <- knapsack_dynamic(x = knapsack_objects[1:8, ], W = 2000)
  expect_equal(round(kd$value), 15428)
  expect_true(all(round(kd$elements) %in% c(3, 8)))
  
  kd <- knapsack_dynamic(x = knapsack_objects[1:12, ], W = 2000)
  expect_equal(round(kd$value), 15428)
  expect_true(all(round(kd$elements) %in% c(3, 8)))
  
  st <- system.time(kd <- knapsack_dynamic(x = knapsack_objects[1:16, ], W = 2000))
  expect_true(as.numeric(st)[3] >= 0.00)  
  
})

# here we compare with greedy_knapsack solution 
test_that("Greedy solution can be better", {
  gk <- greedy_knapsack(x = knapsack_objects[1:12, ], W = 3500)
  kd <- knapsack_dynamic(x = knapsack_objects[1:12, ], W = 3500)
  
  expect_true(kd$value >= gk$value)
})