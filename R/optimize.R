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

#brute_force_knapsack is the most complex function but we can not optimize more,
# it has a complexity O(2^n) if there are not much numbers is good to work in parallel

#greedy_knapsack has complexity O(n*log(n)) the fastest one 

#knapsack_dynamic is O(W*n) where n is the number of items and 
#W is the maximum capacity