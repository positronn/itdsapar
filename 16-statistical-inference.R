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
p <- 0.45
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

results <- results %>% 
    unnest_wider(results)

results %>% 
    summarise(mean_x_hat = mean(x_hat))

results %>% 
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
        xlab('Confidence Interval') +
        geom_vline(xintercept = p, linetype = 'dashed')


# When using the theory we described above, it is important to remember that
# it is the intervals that are random, not p. In the plot above, we can see
# the random intervals moving around and p, represented with the vertical line,
# staying in the same place. The proportion of blue in the urn p is not. So the
# 95% relates to the probability that this random interval falls on top of p.
# Saying the p has a 95% of being between this and that is technically an incorrect
# statement because p is not random.

# Pollsters are not successful at providing correct confidence intervals,
# but rather at predicting who will win. When we took a 25 bead sample size,
# the confidence interval for the spread:
N <- 25
x_hat <- 0.48
(1.96 * x_hat - 1) + c(-1.96, 1.96) * 2 * sqrt(x_hat * (1-x_hat) / sqrt(N))
# includes 0. If this were a poll and we were forced to make a declaration,
# we would have to say it was a “toss-up”.
# A problem with our poll results is that given the sample size and the value of p, 
# we would have to sacrifice on the probability of an incorrect call to create an
# interval that does not include 0.
# This does not mean that the election is close. It only means that we have a small
# sample size. In statistical textbooks this is called lack of power. In the
# context of polls, power is the probability of detecting spreads different from 0
# By increasing our sample size, we lower our standard error and therefore have a
# much better chance of detecting the direction of the spread.


# 16.9 p-values
N <- 100
z <- sqrt(N) * 0.02 / 0.5
1 - (pnorm(z) - pnorm(-z))


# 16.10 Association Tests
# The statistical tests we have studied up to now leave out a substantial portion
# of data types. Specifically, we have not discussed inference for binary,
# categorical and ordinal data. To give a very specific example, consider the
# following case study.

# results reveal gender bias favoring male applicants over female applicants in
# the prioritization of their “quality of researcher” (but not “quality of
# proposal”) evaluations and success rates, as well as in the language use in
# minstructional and evaluation materials.

# The main evidence for this conclusion comes down to a comparison of the
# percentages. Table S1 in the paper includes the information we need. Here are
# the three columns showing the overall outcomes:
library(tidyverse)
library(dslabs)
data('research_funding_rates')

research_funding_rates %>% 
    tibble() %>% 
    select(discipline, contains('total'))

# We have these values for each gender:
names(research_funding_rates)

## We can compute the totals that were successful and the totals that were not as follows:
totals <- research_funding_rates %>% 
    select(-discipline) %>% 
    summarise_all(list(sum = sum), ) %>% 
    summarise(yes_men = awards_men_sum,
              no_men = applications_men_sum - awards_men_sum,
              yes_women = awards_women_sum,
              no_women = applications_women_sum - awards_women_sum) 

# So we see that a larger percent of men than women received awards:
totals %>% 
    summarise(percent_men = yes_men / (yes_men + no_men),
              percent_women = yes_women / (yes_women + no_women))

# But could this be due just to random variability? Here we learn how to
# perform inference for this type of data.

# 16.10.1 Lady Tasting Tea
tab <- matrix(c(3, 1, 1, 3), 2, 2)
rownames(tab) <- c('poured before', 'poured after')
colnames(tab) <- c('guessed before', 'guessed after')
tab
# These are referred to as a two-by-two table. For each of the four combinations
# one can get with a pair of binary variables, they show the observed counts
# for each occurrence.
# The function fisher.test performs the inference calculations above and can
# be obtained like this:
fisher.test(tab, alternative='greater')

# 16.10.3 Chi-square Test
# Notice that, in a way, our funding rates example is similar to the Lady Tasting
# Tea. However, in the Lady Tasting Tea example, the number of blue and red
# beads is experimentally fixed and the number of answers given for each category
# is also fixed. This is because Fisher made sure there were four cups with milk
# poured before tea and four cups with milk poured after and the lady knew this,
# so the answers would also have to include four befores and four afters. If
# this is the case, the sum of the rows and the sum of the columns are fixed.
# This defines constraints on the possible ways we can fill the two by two table
# and also permits us to use the hypergeometric distribution. In general, this is
# not the case. Nonetheless, there is another approach, the Chi-squared test, which
# is described below.

# Imagine we have 2823 applicants, some are men and some are women and some
# get funded, whereas other don’t. We saw that the success rates for men
# and woman were:
totals %>% 
    summarise(percent_men = yes_men / (yes_men + no_men),
             percent_women = yes_men / (yes_women + no_women))
# respectively. Would we see this again if we randomly assign funding
# at the overall rate:    
funding_rate <- totals %>% 
    summarise(percent_total = 
                  (yes_men + yes_women) /
                  (yes_men + no_men + yes_women + no_women)) %>% 
    pull(percent_total)
funding_rate

# The Chi-square test answers this question. The first step is to create the
# two-by-two data table:
two_by_two <- tibble(awarded = c('no', 'yes'),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two
# The general idea of the Chi-square test is to compare this two-by-two
# table to what you expect to see, which would be: (balanced proportions)
tibble(awarded = c('no', 'yes'),
       men = (totals$no_men + totals$yes_men) * c(1 - funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1 - funding_rate, funding_rate))

# We can see that more men than expected and less women than expected received
# funding. However, under the null hypothesis these observations are random
# variables. The Chi-square test tells us how likely it is to see a deviation
# this large or larger. This test uses an asymptotic result, similar to the CLT,
# related to the sums of independent binary outcomes. The R function chisq.test
# takes a two by two table and returns the results from the test:
chisq_results <- two_by_two %>% 
    select(-awarded) %>% 
    chisq.test()
chisq_results

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  .
# X-squared = 3.8111, df = 1, p-value = 0.05091
chisq_test$p.value

# 16.10.4 The odds ratio
# An informative summary statistic associated with two-by-two tables is the odds
# ratio. Define the two variables as X = 1 if you are a male and 0 otherwise,
# and Y = 1 if you are funded and 0 otherwise. The odds of getting funded
# if you are a man is defined:
#
#                   Pr(Y = 1 | X = 1)
#                   -----------------
#                   Pr(Y = 0 | X = 1)
#

# and can be computed like this:
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
    (two_by_two$men[1] / sum(two_by_two$men))
odds_men

# And the odds of being funded if you are a woman is:
#
#                   Pr(Y = 1 | X = 0)
#                   -----------------
#                   Pr(Y = 0 | X = 0)
#
# and can be computed like this
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
    (two_by_two$women[1] / sum(two_by_two$women))
odds_women
# The odds ratio is the ratio for these two odds: how many times larger are
# the odds for men than for women?
odds_men / odds_women

# 16.10.5 Confidence intervals for the odds ratio
# Computing confidence intervals for the odds ratio is not mathematically
# straightforward. Unlike other statistics, for which we can derive useful
# approximations of their distributions, the odds ratio is not only a ratio,
# but a ratio of ratios. Therefore, there is no simple way of using, for example,
# the CLT.

# However, statistical theory tells us that when all four entries of the
# two by two table are large enough, then the log of the odds ratio is
# approximately normal with standard error
#
#           sqrt(1/a + 1/b + 1/c + 1/d)
#
# This implies that a 95% confidence interval for the log odds ratio can be formed by:
#
#           log(ad/bc) ± 1.96 * sqrt(1/a + 1/b + 1/c + 1/d)
#
# By exponentiating these two numbers we can construct a confidence interval
# of the odds ratio.
log_or <- log(odds_men / odds_women)
se <- two_by_two %>% 
    select(-awarded) %>% 
    summarise(se = sqrt(sum(1/men) + sum(1/women))) %>% 
    pull(se)
ci <- log_or + c(-1, 1) * qnorm(0.975) * se
ci
# If we want to convert it back to the odds ratio scale, we can exponentiate:
exp(ci)
# Note that 0 is not included in the confidence interval for the log odds
# ratio (1 not included for odds ratio)
# which must mean that the p-value is smaller then 0.05. We can confirm this using:
2 * (1 - pnorm(log_or, 0, se))
# This is a slightly different p-value that with the chi-squared test.
# This is because we are using a different asymptotic approximation to the
# null distribution.



# 16.10.7 Large samples, small p-values
# As mentioned earlier, reporting only p-values is not an appropriate way to
# report the results of data analysis. In scientific journals, for example,
# some studies seem to overemphasize p-values. Some of these studies have large
# sample sizes and report impressively small p-values. Yet when one looks closely
# at the results, we realize odds ratios are quite modest: barely bigger than 1.
# In this case the difference may not be practically significant or
# scientifically significant.
# 
# Note that the relationship between odds ratio and p-value is not one-to-one.
# It depends on the sample size. So a very small p-value does not necessarily
# mean very large odds ratio.
# Notice what happens to the p-value if we multiply our two-by-two table by 10:
two_by_two %>% 
    select(-awarded) %>% 
    mutate(men = men * 10,
           women = women * 10) %>% 
    chisq.test()
# Yet the odds ratio is unchanged.
