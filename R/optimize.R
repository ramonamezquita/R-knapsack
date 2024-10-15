install.packages("profvis")
library(profvis)
profvis({
  knapsack_dynamic(x = knapsack_objects[1:100, ], W = 3500)
})
