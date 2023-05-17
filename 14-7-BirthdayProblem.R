# 14-7-BirthdayProblem.R
# Suppose you are in a classroom with 50 people. If we assume this is a randomly
# selected group of 50 people, what is the chance that at least two people have the
# same birthday? Although it is somewhat advanced, we can deduce this mathematically.
# We will do this later. Here we use a Monte Carlo simulation. For simplicity,
# we assume nobody was born on February 29. This actually doesn’t change the answer much:

# First, note that birthdays can be represented as numbers between 1 and 365,
# so a sample of 50 birthdays can be obtained like this:
n <- 50
birthdays <- sample(1:365, n, replace = TRUE)
birthdays
# To check if in this particular set of 50 people we have at least two with the same birthday,
# we can use the function duplicated, which returns TRUE whenever an element of a vector
# is a duplicate.
duplicated(birthdays)

# to check if two birthdays were the same, we simply use the any and duplicated functions
# like this:
duplicated(birthdays) %>% 
    any()

# To estimate the probability of a shared birthday in the group, we repeat this experiment
# by sampling sets of 50 birthdays over and over:
same_birthday <- function(n) {
    birthdays <- sample(1:365, n, replace = TRUE)
    duplicated(birthdays) %>% 
        any()
}
B <- 100000
results <- replicate(B, same_birthday(50))
mean(results)
# [1] 0.96976

# People tend to underestimate these probabilities. To get an intuition as to why it is
# so high, think about what happens when the group size is close to 365.
# At this stage, we run out of days and the probability is one.
# Say we want to use this knowledge to bet with friends about two people having the
# same birthday in a group of people. When are the chances larger than 50%?
# Larger than 75%?
# Let’s create a look-up table. We can quickly create a function to compute this for any group size:
compute_same_birthday_prob <- function(n, B = 10000) {
    results <- replicate(B, same_birthday(n))
    mean(results)
}

# Using the function sapply, we can perform element-wise operations on any function:
library(purrr)
n <- seq(1, 60)
sim_prob <- map(n, compute_same_birthday_prob) %>% 
    unlist()

# We can now make a plot of the estimated probabilities of two people having the
# same birthday in a group of size n:
library(ggplot2)
qplot(n, sim_prob)

# Now let’s compute the exact probabilities rather than use Monte Carlo
# approximations. Not only do we get the exact answer using math, but the
# computations are much faster since we don’t have to generate experiments.

# To make the math simpler, instead of computing the probability of it happening,
# we will compute the probability of it not happening. For this, we use the multiplication rule.
# Let’s start with the first person. The probability that person 1 has a unique birthday is 1.
# The probability that person 2 has a unique birthday, given that person 1 already took one,
# is 364/365. Then, given that the first two people have unique birthdays, person 3 is left
# with 363 days to choose from. We continue this way and find the chances of all 50 people
# having a unique birthday is:
#
#           364   363   362         365 - n + 1
#       1 * ___ * ___ * --- * ... * ___________
#           365   364   364             365


# we can write a function that does this for any number:
exact_same_birthday_prob <- function(n) {
    seq_prob_unique_birthday <- seq(365, 365 - n + 1) / 365
    1 - prod(seq_prob_unique_birthday)
}

exact_prob <- map(n, exact_same_birthday_prob) %>% 
    unlist()
qplot(n, sim_prob) +
    geom_line(aes(n, exact_prob), col = 'red') +
    ylab('Probability') +
    xlab('n')


# This plot shows that the Monte Carlo simulation provided a very good estimate of the
# exact probability. Had it not been possible to compute the exact probabilities,
# we would have still been able to accurately estimate the probabilities.


# on the stabilization of results of monte carlo simulation:
# birthday problem with n = 22
B <- 10^seq(1, 5, len = 100)
compute_prob <- function(B, n=25){
    same_day <- replicate(B, same_birthday(n))
    mean(same_day)
}
prob <- sapply(B, compute_prob)
qplot(log10(B), prob, geom = "line") +
    geom_hline(yintercept = mean(prob[70:100]), color = 'red', linetype='dashed')

