# Chapter 15
# Random variables
library(tidyverse)
# 15.2 Sampling models
# Suppose a very small casino hires you to consult on whether they should set up
# roulette wheels. To keep the example simple, we will assume that 1,000 people will
# play and that the only game you can play on the roulette wheel is to bet on red
# or black. The casino wants you to predict how much money they will make or lose.
# They want a range of values and, in particular, they want to know what’s the chance
# of losing money. If this probability is too high, they will pass on installing
# roulette wheels.

# We are going to define a random variable S that will represent the casino’s total
# winnings. Let’s start by constructing the urn. A roulette wheel has 18 red
# pockets, 18 black pockets and 2 green ones. So playing a color in one game of
# roulette is equivalent to drawing from this urn:
color <- rep(c('black', 'red', 'green'), c(18, 18, 2))
color
# 
# the 1,000 outcomes from 1,000 people playing are independent draws from this urn.
# If red comes up, the gambler wins and the casino loses a dollar, so we draw a -$1.
# Otherwise, the casino wins a dollar and we draw a $1. To construct our random
# variable S, we can use this code:
n <- 1000
X <- sample(ifelse(color == 'red', -1, 1), n, replace = TRUE)
X[1:10]    

# Because we know the proportions of 1s and -1s, we can generate the draws with
# one line of code, without defining color:
X <- sample(c(-1,1), n, replace = TRUE, prob=c(9/19, 10/19))

# We call this a sampling model since we are modeling the random behavior of
# roulette with the sampling of draws from an urn. The total winnings S is
# simply the sum of these 1,000 independent draws:
X <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))
S <- sum(X)    
S


# 15.3 The probability distribution of a random variable
n <- 1000
B <- 10000
roulette_winnings <- function(n) {
    X <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))
    sum(X)
}
S <- replicate(B, roulette_winnings(n))

# Now we can ask the following: in our simulations, how often did we get sums
# less than or equal to a?
a <- 0
mean(S <= a)

# This will be a very good approximation of F(a). In fact, we can visualize
# the distribution by creating a histogram showing the probability F (b) − F (a)
# for several intervals (a, b]:
S %>% 
    qplot(binwidth=10, color=I('black'), alpha = I(0.8), fill = I('steelblue')) +
    xlab('S') +
    ylab('Probability') +
    scale_y_continuous()

# Now we can easily answer the casino’s question: how likely is it that we will lose money?
mean(S<0)

# In the histogram above, we see that the distribution appears to be approximately
# normal. A qq-plot will confirm that the normal approximation is close to perfect.
# If, in fact, the distribution is normal, then all we need to define the distribution
# is the average and the standard deviation. Because we have the original values
# from which the distribution is created, we can easily compute these:
min(S)
mean(S)
sd(S)

# If we add a normal density with this average and standard deviation to the
# histogram above, we see that it matches very well:
normal_points <- rnorm(10000, mean=mean(S), sd = sd(S))
x <- seq(-20, 100)

coeff <- .12
data.frame(x = S) %>% 
    ggplot(aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), bins = 32, fill = 'steelblue', color = 'black') +
    stat_function(fun = dnorm, n = 10000, args = list(mean = mean(S), sd = sd(S))) +
    stat_function(fun = pnorm,  args = list(mean = mean(S), sd = sd(S))) +
    scale_y_continuous(sec.axis = sec_axis(~.* coeff, name="Second Axis")) +
    ylab("Probability") +
    xlab('S') 

coeff <- 70
tibble(x = seq(-60, 180, length.out = 10000)) %>% 
    mutate(p = pnorm(x, mean = mean(S), sd = sd(S))) %>% 
    mutate(p_tfm = p / coeff) %>% 
    ggplot() +
    geom_histogram(aes(x = S, y = after_stat(density)), color = 'black', alpha = 0.6) +
    stat_function(fun = dnorm, n = 10000, args = list(mean = mean(S), sd = sd(S))) +
    geom_line(aes(x = x, y = p_tfm), color = 'orangered') +
    scale_y_continuous(sec.axis = sec_axis(~ . * coeff, name="Cumulative")) +
    ylab('Density')



# This average and this standard deviation have special names. They are referred to
# as the expected value and standard error of the random variable S. We will say more
# about these in the next section.
# Statistical theory provides a way to derive the distribution of random
# variables defined as independent random draws from an urn.
# Specifically, in our example above, we can show that (S + n)/2 follows
# a binomial distribution. We therefore do not need to run for Monte
# Carlo simulations to know the probability distribution of S. We did
# this for illustrative purposes.
n <- 1000
pbinom(n / 2, size = n, prob = 10/19)
pbinom(n / 2 - 1, size = n, prob = 10/19)



# 15.7 Central Limit Theorem
# The Central Limit Theorem (CLT) tells us that when the number of draws, also
# called the sample size, is large, the probability distribution of the sum of the
# independent draws is approximately normal. 

# We previously ran this monte Calro simulation
n <- 1000
B <- 10000
roulette_winnings <- function(n) {
    X <- sample(c(-1, 1), n, replace = TRUE, prob = c(9/19, 10/19))
    sum(X)
}
S <- replicate(B, roulette_winnings(n))
# The Central Limit Theorem (CLT) tells us that the sum S is
# approximated by a normal distribution. Using the formulas above, we
# know that the expected value and standard error are:
# theoretical values:
n * (20 - 18) / 38
sqrt(n) * 2 * sqrt(90) / 19
# MC simulation:
mean(S)
sd(S)

# Using the CLT, we can skip the Monte Carlo simulation and instead 
# compute the probability of the casino losing money using this
# approximation:
mu <- n * (20 - 18) / 38
se <- sqrt(n) * 2 * sqrt(90) / 19
pnorm(0, mu, se)
# which is also in very good agreement with our Monte Carlo result:
mean(S < 0)
