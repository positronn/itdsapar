# Chapter 17
# Statistical models
#
#
library(tidyverse)
library(dslabs)
# 17.1 Poll aggregators
# As we described earlier, a few weeks before the 2012 election Nate Silver was giving
# Obama a 90% chance of winning. How was Mr. Silver so confident? We will use a Monte
# Carlo simulation to illustrate the insight Mr. Silver had and others missed.
# To do this, we generate results for 12 polls taken the week before the
# election. We mimic sample sizes from actual polls and construct and report
# 95% confidence intervals for each of the 12 polls. We save the results from
# this simulation in a data frame and add a poll ID column.

d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d + 1) / 2

polls <- map_df(Ns, function(N) {
    x <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1 - p, p))
    x_hat <- mean(x)
    se_hat <- sqrt(x_hat * (1 - x_hat) / N)
    list(estimate = 2 * x_hat - 1,
         low = 2 * (x_hat - 1.96 * se_hat) - 1,
         high = 2 * (x_hat + 1.96 * se_hat) - 1,
         sample_size = N)
}) %>% 
    mutate(poll = seq_along(Ns))
polls

polls %>% 
    ggplot() +
    geom_point(mapping = aes(x = estimate, y = poll), color = 'steelblue') +
    geom_point(mapping = aes(x = low, y = poll), shape = '|', size=4) +
    geom_point(mapping = aes(x = high, y = poll), shape = '|', size=4) +
    geom_segment(mapping = aes(x = low, xend = high, y = poll, yend=poll), color = 'steelblue') +
    geom_vline(xintercept = d, linetype = 'dashed')


# Not surprisingly, all 12 polls report confidence intervals that include the
# election night result (dashed line). However, all 12 polls also include 0
# (solid black line) as well. Therefore, if asked individually for a prediction,
# the pollsters would have to say: it’s a toss-up. Below we describe a key insight
# they are missing.
# Poll aggregators, such as Nate Silver, realized that by combining the results
# of different polls you could greatly improve precision. By doing this, we are
# effectively conducting a poll with a huge sample size. We can therefore report
# a smaller 95% confidence interval and a more precise prediction.

# Although as aggregators we do not have access to the raw poll data, we can use 
# mathematics to reconstruct what we would have obtained had we made one large poll with:

    
sum(polls$sample_size)
# participants. Basically, we construct an estimate of the spread, let’s call
# it d, with a weighted average in the following way:
d_hat <- polls %>% 
    summarise(average = sum(estimate * sample_size) / 
                        sum(sample_size)) %>% 
    pull(average)

# Once we have an estimate of d, we can construct an estimate for the proportion
# voting for Obama, which
# we can then use to estimate the standard error. Once we do this, we see that
# our margin of error is 0.018.

# Thus, we can predict that the spread will be 3.1 plus or minus 1.8, which
# not only includes the actual result we eventually observed on election night,
# but is quite far from including 0. Once we combine the 12 polls, we become
# quite certain that Obama will win the popular vote.


# 17.1.1 Poll data
# We use public polling data organized by FiveThirtyEight for the 2016
# presidential election. The data is included as part of of the dslabs package:
data("polls_us_election_2016")
names(polls_us_election_2016)

# The table includes results for national polls, as well as state polls,
# taken during the year prior to the election. For this first example,
# we will filter the data to include national polls conducted during
# the week before the election. We also remove polls that FiveThirtyEight
# has determined not to be reliable and graded with a
# “B” or less. Some polls have not been graded and we include those:
polls <- polls_us_election_2016 %>%
    filter(state == 'U.S.' &
           enddate >= '2016-10-31' &
           (grade %in% c('A+', 'A', 'A-', 'B-') |
                    is.na(grade))) %>% 
    tibble()

# we add a spread estimate
polls <- polls %>% 
    mutate(spread = rawpoll_clinton / 100 - rawpoll_trump / 100)
polls    

# For this example, we will assume that there are only two parties and call
# p the proportion voting for Clinton and 1 − p the proportion voting for
# Trump. We are interested in the spread 2p − 1. Let’s call the spread d
# (for difference).

# We have 49 estimates of the spread. The theory we learned tells us that
# these estimates are a random variable with a probability distribution
# that is approximately normal. The expected value is the election night
# spread d and the standard error is
#
#                   sqrt(p(1 - p)/N)
# Assuming the urn model we described earlier is a
# good one, we can use this information to construct a confidence interval based on the aggregated data. The
# estimated spread is:
d_hat <- polls %>% 
    summarise(d_hat = sum(spread * samplesize) /
                      sum(samplesize)) %>% 
    pull(d_hat)
d_hat    

# and the standard error is:
p_hat <- (d_hat + 1) / 2
p_hat
moe <- 1.96 * 2 * sqrt(p_hat * (1 - p_hat) / sum(polls$samplesize))
moe
# spread of 0.01934504 with a 0.005823094 margin of error

#  On election night, we discover that the actual percentage was 2.1%,
# which is outside a 95% confidence interval. What happened?
# A histogram of the reported spreads shows a problem:
polls %>% 
    ggplot(aes(spread)) +
    geom_histogram(color = 'black', binwidth = 0.01)

# The data does not appear to be normally distributed and the standard error appears to be larger than 0.007.
# The theory is not quite working here.


# 17.1.2 Pollster bias
# Notice that various pollsters are involved and some are taking several polls a week:
polls %>% 
    group_by(pollster) %>% 
    summarise(n = n())

polls %>% 
    ggplot(aes(y = spread)) +
    geom_point(aes(x = pollster)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# This plot reveals a unexpected result. First, consider that the standard error predicted by theory for each poll:
polls %>% 
    group_by(pollster) %>% 
    filter(n() > 6) %>% 
    summarise(se = 2 * sqrt(p_hat * (1 - p_hat) / median(samplesize)))
polls

# is between 0.018 and 0.033, which agrees with the within poll variation we see.
# However, there appears to be differences across the polls. Note, for example,
# how the USC Dornsife/LA Times pollster is predicting a 4% win for Trump, while
# Ipsos is predicting a win larger than 5% for Clinton. The theory we learned says
# nothing about different pollsters producing polls with different expected values.
# All the polls should have the same expected value. FiveThirtyEight refers to
# these differences as “house effects”. We can also call them pollster bias.
# In the following section, rather than use the urn model theory, we are instead
# going to develop a data-driven model.


