
# Knapsack Solvers in R

This R package provides implementations of various algorithms to solve the **0-1 Knapsack Problem**, a combinatorial optimization problem. The goal is to determine the most valuable combination of items that can fit within a specified weight limit. The package offers several approaches, ranging from exact solutions (dynamic programming, brute-force) to efficient approximations (greedy heuristic).

## Installation

You can install the package locally from source:

```r
# Download the package and install
devtools::install_github("ramonamezquita/heleramcar6")
```

## Functions Overview

### 1. `suboptimal_knapsack_dynamic()`
This function uses a sub-optimal dynamic programming approach to solve the knapsack problem. While it provides a solution based on dynamic programming, it performs unnecessary computations that could be avoided in a more efficient implementation.


### 2. `knapsack_dynamic()`
An optimal dynamic programming solution that computes the maximum possible value using a recursive approach. It optimally selects items that maximize the value while respecting the weight capacity.


### 3. `greedy_knapsack()`
A greedy heuristic algorithm that approximates the solution by selecting items based on their value-to-weight ratio. Although efficient, it does not guarantee an optimal solution.


### 4. `brute_force_knapsack()`
This function uses a brute-force approach to solve the knapsack problem by exploring all possible subsets of items. It guarantees an optimal solution but is computationally expensive for large sets of items.


## Example Usage

```r
# Example items: Weights and values
items <- data.frame(w = c(3, 8, 5), v = c(4, 6, 5))

# Knapsack capacity
W <- 8

# Solve using different methods
suboptimal_value <- suboptimal_knapsack_dynamic(items, W)
optimal_value <- optimal_knapsack_dynamic(items, W)
greedy_solution <- greedy_knapsack(items, W)
brute_force_solution <- brute_force_knapsack(items, W)

print(suboptimal_value)
print(optimal_value)
print(greedy_solution)
print(brute_force_solution)
```

## References

For more details on the knapsack problem, please visit:  
[Knapsack problem - Wikipedia](https://en.wikipedia.org/wiki/Knapsack_problem)
