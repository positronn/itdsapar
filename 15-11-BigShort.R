# 15.11 Case study: The Big Short
# 
# Interest rates explained with chance model
library(tidyverse)
# Suppose your bank will give out 1,000 loans for $180,000 this year. Also, after adding up all costs,
# suppose your bank loses $200,000 per foreclosure. For simplicity, we assume this includes all
# operational costs. A sampling model for this scenario can be coded like this:

n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample(c(0, 1), n, prob = c(1 - p, p), replace = TRUE)
defaults
sum(defaults * loss_per_foreclosure)

# Note that the total loss defined by the final sum is a random variable.
# Every time you run the above code, you get a different answer. We can easily
# construct a Monte Carlo simulation to get an idea of the distribution of
# this random variable.
B <- 10000
losses <- replicate(B, {
    defaults <- sample(c(0, 1), n, prob = c(1 - p, p), replace = TRUE)
    sum(defaults * loss_per_foreclosure)
})

# plot the distribution of the losses
tibble(losses_in_millions = losses / 1000000) %>% 
    ggplot(mapping = aes(x = losses_in_millions)) +
    geom_histogram(bins = 30, color = 'black', fill = 'red4', alpha = 0.8) +
    geom_vline(xintercept = 0, linetype = 'dashed')

# We don’t really need a Monte Carlo simulation though. Using what we
# have learned, the CLT tells us that because our losses are a sum of
# independent draws, its distribution is approximately normal with
# expected value and standard errors given by:
# E[X]
n * (p * loss_per_foreclosure + (1 - p) * 0)
# SE[X]
sqrt(n) * abs(loss_per_foreclosure) * sqrt(p * (1 - p))

# We can now set an interest rate to guarantee that, on average,
# we break even. Basically, we need to add a quantity x to each loan,
# which in this case are represented by draws, so that the expected
# value is 0. If we define l to be the loss per foreclosure, we need:
#
#   l * p + x ( 1 - p) = 0
#
#
#   loss_per_foreclosure * prob_foreclosure + x(1 - prob_foreclosure) = 0
# 
# which implies x is
#
#                    l * p
#           x = - ----------
#                   (1 - p)
#
- (loss_per_foreclosure * p) / (1 - p)
# 1] 4081.633

4081.633 / 180000
# [1] 0.02267574

# However, we still have a problem. Although this interest rate
# guarantees that on average we break even, there is a 50% chance
# that we lose money.

# plot the distribution of the losses, including interest rate to balance to zero loss
B <- 10000
losses <- replicate(B, {
    defaults <- sample(c(0, 1), n, prob = c(1 - p, p), replace = TRUE)
    sum(defaults * loss_per_foreclosure) + sum((n - sum(defaults)) * 4081.633)
})
tibble(losses_in_millions = losses / 1000000) %>% 
    ggplot(mapping = aes(x = losses_in_millions)) +
    geom_histogram(bins = 30, color = 'black', fill = 'red4', alpha = 0.8) +
    geom_vline(xintercept = 0, linetype = 'dashed')


# If our bank loses money, we have to close it down. We therefore need to pick an interest rate
# that makes it unlikely for this to happen. At the same time, if the interest rate
# is too high, our clients will go to another bank so we must be willing to take some risks.
# So let’s say that we want our chances of losing money to be 1 in 100, what does the x quantity
# need to be now? This one is a bit harder. We want the sum S to have:
#
#                   Pr(S < 0) = 0.01
#
# We know that S is approximately normal. The expected value of S is
#
#               E[S] = {lp + x(1 − p)}n
# 
# with n the number of draws, which in this case represents loans. The standard error is
#
#               SD[S] = |x−l| sqrt(  np(1−p)  )


# Now we are going to use a mathematical “trick” that is very common in statistics. Weadd and
# subtract the same quantities to both sides of the event S < 0 so that the probability does not
# change and we end up with a standard normal random variable on the left, which will then permit
# us to write down an equation with only x as an unknown. This “trick” is as follows:
#
#               Pr(S < 0) = 0.01
#
#           Pr(  (S - E[S]) / SE[S] < - E[S] / SE[S)
#
# now the term on the left is a standard normal random variable, which we will rename Z.
# Now we fill in the blanks with the actual formula for expected value and standard error:
#
#       Pr ( Z < (  - {lp + x(1 - p)}n / {(x - l) * sqrt(np(1 - ´p)) )  ) = 0.01
#
#
# Now because the Z is a normal random with expected value 0 and standard error 1, it means
# that the quantity on right side of the < sign must be equal to:
qnorm(0.01)
# for the equation to hold true. Remember that z =qnorm(0.01) gives us the value of z for which:
#
#           Pr(Z ≤ z) = 0.01
#
# So this means that right side of the complicated equation must be z = qnorm(0.01).
# ** after boring algebra
#
#
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x
# Our interest rate now goes up to 0.035. This is still a very competitive interest rate.
# By choosing this interest rate, we now have an expected profit per loan of:
loss_per_foreclosure*p + x*(1-p)
# which is a total expected profit of about:
n*(loss_per_foreclosure*p + x*(1-p))

# We can run a Monte Carlo simulation to double check our theoretical approximations:
B <- 10000
profit <- replicate(B, {
    draws <- sample(c(x, loss_per_foreclosure), n, prob = c(1 - p, p), replace = TRUE)
    sum(draws)
})
tibble(profit_in_millions = profit / 1000000) %>% 
    ggplot(mapping = aes(x = profit_in_millions)) +
    geom_histogram(bins = 30, color = 'black', fill = 'red4', alpha = 0.8) +
    geom_vline(xintercept = 0, linetype = 'dashed')

mean(profit<0)


# 15.11.2 The Big Short
# One of your employees points out that since the bank is making 2124 dollars per loan,
# the bank should give out more loans! Why just n? You explain that finding those n clients was
# hard. You need a group that is predictable and that keeps the chances of defaults low. He then
# points out that even if the probability of default is higher, as long as our expected value
# is positive, you can minimize your chances of losses by increasing n and relying on the law
# of large numbers.

# He claims that even if the default rate is twice as high, say 4%, if we set
# the rate just a bit higher than this value
p <- 0.04
r <- (-loss_per_foreclosure * p / (1 - p)) / 180000
r
# we will profit. At 5%, we are guaranteed a positive expected value of:
r <- 0.05
x <- r * 180000
loss_per_foreclosure * p + x * (1 - p)

# and can minimize our chances of losing money by simply increasing n since:
#
#           Pr(S < 0) = Pr(Z < - E[S]/SE[S])
#

# with Z a standard normal random variable as shown earlier. 
# If we define μ and σ to be the expected value and standard deviation of the urn
# respectively (that is of a single loan), using the formulas above we have:
# E[S] = nμ and S E[S] = √nσ. So if we define z=qnorm(0.01), we have:
#                 n * mu          sqrt(n) * mu
#       - ------------------- = - ------------------ = z
#           sqrt(n) * sigma            sigma
#
# which implies that if we let:
#
#           n >= z^2 sigma^2 / mu^2
#
# we are guaranteed to have a probability of less than 0.01.
# The implication is that, as long as μ is positive, we can find an n that
# minimizes the probability of a loss. This is a form of the law of large
# numbers: when n is large, our average earnings per loan converges to the
# expected earning mu.
#
# 

# With x fixed, now we can ask what n do we need for the probability to be
# 0.01? In our example, if we give out:
z <- qnorm(0.01)
n <- ceiling((z^2 * (x - l) ^ 2 * p * (1 - p)) / (l * p + x * (1 - p)) ^ 2)
n
# loans, the probability of losing is about 0.01 and we are expected to earn a total of
n*(loss_per_foreclosure*p + x * (1-p))
# dollars! We can confirm this with a Monte Carlo simulation:
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
    draws <- sample(c(x, loss_per_foreclosure), n, prob = c(1 - p, p), replace = TRUE)
    sum(draws)
})
mean(profit)

# This seems like a no brainer. As a result, your colleague decides to leave your
# bank and start his own high risk mortgage company. A few months later, your
# colleague’s bank has gone bankrupt. A book is written and eventually a movie
# is made relating the mistake your friend, and many others, made. What happened?
# Your colleague’s scheme was mainly based on this mathematical formula:
#
#               SE[(X1 + X2 + X3 + ... + Xn) / n] = sigma / sqrt(n)
#

# By making n large, we minimize the standard error of our per-loan profit. However,
# for this rule to hold, the Xs must be independent draws: one person defaulting must
# be independent of others defaulting. Note that in the case of averaging the same
# event over and over, an extreme example of events that are not independent, we get
# a standard error that is sqrt(n) times bigger:
#
#                   SE[(X1 + X1 + ... + X1) / n] = SE[nX1/n] = sigma
#

# To construct a more realistic simulation than the original one your colleague ran,
# let’s assume there is a global event that affects everybody with high risk
# mortgages and changes their probability. We will assume that with 50-50 chance,
# all the probabilities go up or down slightly to somewhere between 0.03 and 0.05.
# But it happens to everybody at once, not just one person. These draws are no longer
# independent.
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
    new_p <- 0.04  + sample(seq(-0.01, 0.01, length = 100), 1)
    draws <- sample( c(x, loss_per_foreclosure), n, prob = c(1 - new_p, new_p), replace = TRUE)
    sum(draws)
})

# Note that our expected profit is still large:
mean(profit)
sd(profit)
# However, the probability of the bank having negative earnings shoots up to:
mean(profit<0)

# Even scarier is that the probability of losing more than 10 million dollars is:
mean(profit < -10000000)

# To understand how this happens look at the distribution:
tibble(profit_in_millions = profit / 10^6) %>% 
    ggplot(mapping = aes(x = profit_in_millions)) +
    geom_histogram(color = 'black', fill = 'red4', binwidth = 5, alpha = 0.8) +
    geom_vline(xintercept = 0, linetype='dashed')
# The theory completely breaks down and the random variable has much more variability than expected.
# The financial meltdown of 2007 was due, among other things, to financial “experts” assuming
# independence when there was none.


scale_coeff <- 65
profit_mean <- mean(profit) / 10^6
profit_sd <- sd(profit) / 10^6
tibble(profit_in_millions = profit / 10^6,
       x = seq(-60, 90, length = 10000),
       pnorm = map(seq(-70, 90, length = 10000), pnorm, mean = profit_mean, sd = profit_sd) %>% unlist()) %>%
    mutate(pnorm_scaled = pnorm / scale_coeff) %>% 
    ggplot(mapping = aes(x = profit_in_millions)) +
    geom_histogram(aes(y = after_stat(density)), color = 'black', fill = 'gray', binwidth = 5, alpha = 0.8) +
    geom_vline(xintercept = 0, linetype='dashed') +
    stat_function(fun = dnorm, n = 10000, args = list(mean = profit_mean, sd = profit_sd), color='red1') +
    geom_line(mapping = aes(x = x, y = pnorm_scaled), color = 'royalblue2') +
    scale_y_continuous(sec.axis = sec_axis(~ . * scale_coeff, name="Cumulative"))

# not quiet a normal distribution....