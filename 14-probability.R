# montecarlo simulations for categorical data
library(tibble)

# using the sample function: picking at random
beads <- rep(c('red', 'blue'), times = c(7, 3))
beads

sample(beads, 7)
# This line of code produces one random outcome. We want to repeat
# this experiment an infinite number of times, but it is impossible to
# repeat forever. Instead, we repeat the experiment a large enough
# number of times to make the results practically equivalent to
# repeating forever. This is an example of a Monte Carlo simulation.


# To perform our first Monte Carlo simulation, we use the `replicate`
# function, which permits us to repeat the same task any number
# of times. Here, we repeat the random event B = 10,000 times:
B <- 10000
events <- B %>% 
    replicate(sample(beads, 1))

# We can use table to see the distribution:
tab <- events %>% 
    table()
tab

# and prop.table gives us the proportions:
tab %>% 
    prop.table()
# The numbers above are the estimated probabilities provided by this Monte Carlo simulation.

# The function sample has an argument that permits us to pick more
# than one element from the urn. However, by default, this selection
# occurs without replacement: after a bead is selected, it is not put
# back in the bag.
sample(beads, 5)
sample(beads, 5)
sample(beads, 11)

# To do this, we sample with replacement: return the bead back to the
# urn after selecting it. We can tell sample to do this by
# changing the replace argument, which defaults to FALSE, to
# replace = TRUE:
events <- sample(beads, B, replace = TRUE)
prop.table(table(events))

# Not surprisingly, we get results very similar to those previously obtained with replicate.
