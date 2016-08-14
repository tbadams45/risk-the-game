#library(reshape2)

roll <- function(dice = 1, times = 1) {
  sample(1:6, size = dice, replace = TRUE)
}

# Get the top two of the three offensive rolls.
# Largest one is the first in the vector.
get_top_two <- function(roll) {
  n <- length(roll)
  sorted <- sort(roll, partial = n-1)
  c(sorted[n], sorted[n-1])
}

# compare each pair of dice. Defense wins ties.
compare <- function(attack, defense) {
  if(attack > defense){
    return("attack")
  }
  else return("defense")
}

# one dice roll from both defense and offense.
battle <- function(num_d = 2, num_o = 3) {
  d <- roll(num_d)
  d_top <- sort(d, decreasing = TRUE) # get highest defensive roll in index 1
  o <- roll(num_o)
  o_top <- get_top_two(o)
  
  obv <- c(compare(o_top[1], d_top[1]), compare(o_top[2], d_top[2]))
  
  obv
}

sim <- function(n, num_d, num_o) {
  replicate(n, battle(num_d = num_d, num_o = num_o))
}

