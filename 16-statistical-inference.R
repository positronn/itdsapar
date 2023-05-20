# 16 - Statistical Inference
#
#
# Statistical Inference is the part of statistics that helps distinguish patterns arising
# from signal from those arising from chance.
# The task of Statistical Inference is to predict the parameter p using the observed data
# in the sample.
# Just ike we use variables to define unkowns in systems of equations, in Statistical Inference
# we define parameters to define unknown parts of our models.

library(tidyverse)
library(dslabs)
take_poll(25)

# We want to predict the proportion of blue beads in the urn. Let’s call this quantity p,
# which then tells us the proportion of red beads 1 − p, and the spread p − (1 − p),
# which simplifies to 2p − 1.

# In statistical textbooks, the beads in the urn are called the population. The proportion of
# blue beads in the population p is called a parameter. The 25 beads we see in the previous
# plot are called a sample. The task of statistical inference is to predict the
# parameter p using the observed data in the sample.


# 
# Suppose we want to use a Monte Carlo simulation to corroborate the tools we
# have built using probability theory. To create the simulation, we would
# write code like this:
B <- 10000
N <- 1000
x_hat <- replicate(B, {
    x <- sample(c(0, 1), size=N, replace=TRUE, prob = c(1 - p, p))
    mean(x)
})    
# The problem is, of course, we don’t know p.
# One thing we therefore do to corroborate theoretical results is to pick one or several
# values of p and run the simulations. Let’s set p=0.45. We can then simulate a poll:
p <- 0.45
N <- 1000
x <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
x_hat <- mean(x)

# In this particular sample, our estimate is x_hat. We can use that code to do a Monte Carlo simulation:
B <- 10000
x_hat <- replicate(B, {
    x <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
    mean(x)
})
mean(x_hat)
sd(x_hat)

scale_coeff <- 20
tibble(x_hat = x_hat,
       x_pnorm = seq(0.35, 0.55, length = 10000))  %>% 
    mutate(pnorm = map_dbl(x_pnorm, pnorm, mean = mean(x_hat), sd = sd(x_hat))) %>%
    ggplot() +
    geom_histogram(mapping = aes(x_hat, y = after_stat(density)),
                   color = 'black',
                   alpha = 0.8) +
    geom_line(mapping = aes(x = x_pnorm, y = pnorm * 25),
              color = 'red2')


# Of course, in real life we would never be able to run such an experiment because we
# don’t know p. But we could run it for various values of p and N and see that the
# theory does indeed work well for most values. You can easily do this by re-running
# the code above after changing p and N.

tibble(x_hat = x_hat,
       x_pnorm = seq(0.35, 0.55, length = 10000))  %>% 
    mutate(pnorm = map_dbl(x_pnorm, pnorm, mean = mean(x_hat), sd = sd(x_hat))) %>%
    ggplot(mapping = aes(sample = x_hat)) +
    stat_qq() +
    stat_qq_line() +
    xlab('Theoretical normal') +
    ylab('X hat')


# 16.6 Confidence intervals
p <- 0.45
N <- 1000
# notice the intervals here
x <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
x_hat <- mean(x)
se_hat <- sqrt(x_hat * (1 - x_hat) / N)
c(x_hat - 1.96 * se_hat, x_hat + 1.96 * se_hat)

# is different from this one:
x <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
x_hat <- mean(x)
se_hat <- sqrt(x_hat * (1 - x_hat) / N)
c(x_hat - 1.96 * se_hat, x_hat + 1.96 * se_hat)
# Keep sampling and creating intervals and you will see the random variation.
# To determine the probability that the interval includes p, we need to compute this:
#
#                Pr(X − 1.96SE(X) ≤ p ≤ X + 1.96SE(X))
# 
# By subtracting and dividing the same quantities in all parts of the equation, we get that the above is
#
#                   Pr(-1.96 <= X_bar - p / SE_hat[X] <= 1.96)
#
#               ----> Pr(-1.96 <= Z <= 1.96)
#
# which we can quickly compute using :
pnorm(1.96) - pnorm(-1.96)

# proving that we have a 95% probability.
# If we want to have a larger probability, say 99%, we need to multiply by whatever z satisfies the following:
#   Pr (−z ≤ Z ≤ z) = 0.99

# Using:
z <- qnorm(0.995)
z
# will achieve this because by definition pnorm(qnorm(0.995)) is 0.995 and by
# symmetry pnorm(1-qnorm(0.995)) is 1 - 0.995. As a consequence, we have that:
pnorm(z) - pnorm(-z)


# 16.6.1 A Monte Carlo simulation
# We can run a Monte Carlo simulation to confirm that, in fact,
# a 95% confidence interval includes p 95% of
N <- 1000
B <- 10000
inside <- replicate(B, {
    x <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
    x_hat <- mean(x)
    se_hat <- sqrt(x_hat * (1 - x_hat) / N)
    between(p, x_hat - 1.96 * se_hat, x_hat + 1.96 * se_hat)
})
mean(inside)

# replicate plot of confidence intervals simulation
N <- 1000
B <- 10000
sim <- function(.x) {
    x <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
    x_hat <- mean(x)
    se_hat <- sqrt(x_hat * (1 - x_hat) / N)
    list(x_hat = x_hat, se_hat = se_hat)
}

results <- tibble(x = 1:10000,
       results = map(x, sim))

results %>% 
    unnest_wider(results) %>% 
    slice_head(n = 100) %>% 
    mutate(lx = x_hat - se_hat, hx = x_hat + se_hat) %>% 
    mutate(inside = if_else(p >= lx & p <= hx, TRUE, FALSE)) %>% 
    ggplot() +
        geom_point(mapping = aes(x = lx, y = x)) +
        geom_point(mapping = aes(x = hx, y = x)) +
        geom_segment(mapping = aes(x = lx, xend = hx, y = x, yend = x,
                               color = inside)) +
        theme(legend.position = 'None') +
        ylab('Simulation trial') +
        xlab('Confidence Interval')
