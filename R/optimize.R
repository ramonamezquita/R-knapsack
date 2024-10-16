install.packages("profvis")
library(profvis)
profvis({
  knapsack_dynamic(x = knapsack_objects[1:100, ], W = 3500)
})

profvis({
  greedy_knapsack(x = knapsack_objects[1:1000, ], W = 90000)
})

profvis({
  brute_force_knapsack(x = knapsack_objects[1:100, ], W = 3500)
})

#brute_force_knapsack is the most complex function 