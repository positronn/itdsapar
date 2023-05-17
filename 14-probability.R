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


# compbinations and permutations
# building a deck of cards
suits <- c('Diamonds', 'Clubs', 'Hearts', 'Spades')
numbers <- c('A', '2', '3', '4', '5',
             '6', '7', '8', '9', '10',
             'J', 'Q', 'K')
deck <- expand.grid(number=numbers, suit=suits)
deck <- paste(deck$number, deck$suit)
deck

# With the deck constructed, we can now double check that the
# probability of a King in the first card is 1/13.
kings <- paste('K', suits)
kings
mean(deck %in% kings)

# Now, how about the conditional probability of the second
# card being a King given that the first was a King? Earlier,
# we deduced that if one King is already out of the deck and
# there are 51 left, then this probability is 3/51. Let’s confirm
# by listing out all possible outcomes.

# To do this, we can use the permutations function from the gtools
# package. For any list of size n, this function computes
# all the different combinations we can get when we select r
# items. Here are all the ways we can choose two numbers
# from a list consisting of 1,2,3:
library(gtools)
permutations(3, 2)

# Notice that the order matters here: 3,1 is different than 1,3.
# Also, note that (1,1), (2,2) and (3,3) do not appear because
# once we pick a number, it can’t appear again.

# Optionally, we can add a vector. If you want to see five random
# seven digit phone numbers out of all possible phone numbers
# (without repeats), you can type:
all_phone_numbers <- permutations(10, 7, v = 0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index, ]

# To compute all possible ways we can choose two cards when the
# order matters, we type:
hands <- permutations(52, 2, v = deck)
hands

# get first and seconds cards
first_card <- hands[, 1]
second_card <- hands[, 2 ]

# Now the cases for which the first hand was a King can be computed like this:
kings <- paste('K', suits)
sum(first_card %in% kings)

# To get the conditional probability, we compute what fraction of these have a King in the second card:
sum(first_card %in% kings &
    second_card %in% kings) /
sum(first_card %in% kings)

# which is exactly 3/51, as we had already deduced. Notice that the code above is equivalent to:
mean(first_card %in% kings &
     second_card %in% kings) /
mean(first_card %in% kings)

# this is an R version of:
#   Pr(A and B)
#   -----------
#      Pr(A)

# of this happening, we would enumerate the combinations, not the permutations, since the order does not
# matter. Below are the differences:
permutations(3, 2)
combinations(3, 2)    

# So to compute the probability of a Natural 21 in Blackjack, we can do this:
aces <- paste('A', suits)

facecard <- c('K', 'Q', 'J', '10')
facecard <- expand.grid(number=facecard, suit=suits)
facecard <- paste(facecard$number, facecard$suit)

hands <- combinations(52, 2, v = deck)
mean((hands[, 1] %in% aces & hands[, 2] %in% facecard) |
     (hands[, 2] %in% aces & hands[, 1] %in% facecard))


# 14.6.1 Monte Carlo example
# Instead of using combinations to deduce the exact probability of a Natural 21,
# we can use a Monte Carlo to estimate this probability.
# In this case, we draw two cards over and over and keep track of how many 21s
# we get. We can use the function sample to draw two cards without replacements:
hand <- sample(deck, 2)
hand
# And then check if one card is an Ace and the other a face card or a 10. Going
# forward, we include 10 when we say face card. Now we need to check both
# possibilities:
(hands[1] %in% aces & hands[2] %in% facecard) |
    (hands[2] %in% aces & hands[1] %in% facecard)
# If we repeat this 10,000 times, we get a very good approximation of the
# probability of a Natural 21.
blackjack <- function() {
    hand <- sample(deck, 2)
    (hand[1] %in% aces & hand[2] %in% facecard) |
        (hand[2] %in% aces & hand[1] %in% facecard)
}
blackjack()
# Here we do have to check both possibilities: Ace first or
# Ace second because we are not using the combinations
# function. The function returns TRUE if we get a 21 and
# FALSE otherwise.

# Now we can play this game, say, 100,000 times:
B <- 100000
results <- replicate(B, blackjack())
mean(results)
# [1] 0.04884
# [1] 0.04876
# [1] 0.04763



# 14.11 Theoretical continuous distributions
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>%
    filter(sex=="Male") %>%
    pull(height)


# The cumulative distribution for the normal distribution is defined by
# a mathematical formula which in R can be obtained with the function pnorm.
# We say that a random quantity is normally distributed with average m
# and standard deviation s if its probability distribution is defined by:
# 
# F(a) = rnorm(a, m, s)
# 
# This is useful because if we are willing to use the normal approximation for, say,
# height, we don’t need the entire dataset to answer questions such as: what is
# the probability that a randomly selected student is taller then 70 inches? We
# just need the average height and standard deviation:
m <- mean(x)
s <- sd(x)
1 - pnorm(70.5, m, s)

# 
# With continuous distributions, the probability of a singular value is not even defined.
# Instead, we define probabilities for intervals.
# In cases like height, in which the data is rounded, the normal approximation is
# particularly useful if we deal with intervals that include exactly one round number.
# For example, the normal distribution is useful for approximating the proportion of
# students reporting values in intervals like the following three:
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

# Note how close we get with the normal approximation:
pnorm(68.5, m, s) - pnorm(67.5, m, s)
pnorm(69.5, m, s) - pnorm(68.5, m, s)
pnorm(70.5, m, s) - pnorm(69.5, m, s)

# However, the approximation is not as useful for other intervals.
# For instance, notice how the approximation breaks down when we try to estimate:
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, m, s) - pnorm(70.1, m, s)


# 14.12 Monte Carlo simulations for continuous variables
# R provides functions to generate normally distributed outcomes. Specifically,
# the rnorm function takes three arguments: size, average (defaults to 0),
# and standard deviation (defaults to 1) and produces random numbers. Here
# is an example of how we could generate data that looks like our reported
# heights:
n <- length(x)
m <- mean(x)
s <- sd(x)
simulated_heights <- rnorm(n, m, s)

simulated_heights %>% 
    qplot(binwidth = 1, fill = I('steelblue'), color = I('black'))
# This is one of the most useful functions in R as it will permit us to
# generate data that mimics natural events and answers questions related
# to what could happen by chance by running Monte Carlo simulations.
B <- 10000
tallest <- replicate(B, {
    simulated_data <- rnorm(800, m, s)
    max(simulated_data) })
mean(tallest >= 7*12)

tallest %>% 
    qplot(binwidth=1, fill = I('steelblue'), color = I('black'))


# 14.13 Continuous distributions
# R uses a convention that lets us remember the names, namely using the
# letters d, q, p and r in front of a shorthand for the distribution. We
# have already seen the functions dnorm, pnorm and rnorm for the normal
# distribution. The functions qnorm gives us the quantiles. We can
# therefore draw a distribution like this:
x <- seq(-4, 4, length.out = 100)
x %>% 
    tibble(f = dnorm(x)) %>% 
    ggplot(aes(x, f)) +
    geom_line()

x <- seq(0, 10, length.out = 100)
x %>% 
    tibble(f = dgamma(x, shape = 1),
           g = dgamma(x, shape = 2),
           h = dgamma(x, shape = 3)) %>% 
    ggplot() +
    geom_line(aes(x = x, y = f), color = 'steelblue') +
    geom_line(aes(x = x, y = g), color = 'orangered') +
    geom_line(aes(x = x, y = h), color = 'forestgreen') +
    ylab('gamma')

# For example, for the student-t, 
# the shorthand t is used so the functions are dt for the density, qt for the quantiles,
# pt for the cumulative distribution function, and rt for Monte Carlo simulation.
