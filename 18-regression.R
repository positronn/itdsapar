# 18.1 Case study: is height hereditary?
# We have access to Galton’s family height data through the HistData package. This data contains
# heights on several dozen families: mothers, fathers, daughters and sons. To imitate Galton’s
# analysis, we will create a dataset with the heights of fathers and a randomly selected son of
# each family:
library(tidyverse)
library(HistData)
data('GaltonFamilies')


set.seed(1983)
galton_heights <- GaltonFamilies %>% 
    filter(gender == 'male') %>% 
    group_by(family) %>% 
    sample_n(1) %>% 
    ungroup() %>% 
    select(father, childHeight) %>% 
    rename(son = childHeight)

galton_heights

# Suppose we were asked to summarize the father and son data. Since both
# distributions are well approximated by the normal distribution, we could
# use the two averages and two standard deviations as summaries:
galton_heights %>% 
    summarise(mean(father), sd(father), mean(son), sd(son))

# However, this summary fails to describe an important characteristic of
# the data: the trend that the taller the father, the taller the son.
galton_heights %>% 
    ggplot(aes(father, son)) +
    geom_point(alpha = 0.5)

# We will learn that the correlation coefficient is an informative summary
# of how two variables move together and then see how this can be used
# to predict one variable using the other.

# 18.2 The correlation coefficient
# 
# The correlation coefficient is defined for a list of pairs
# (x1, y1), . . . , (xn, yn) as the average of the produce of the
# standardized values.

# rho <- mean(scale(x) * scale(y))

# The correlation between father and son’s heights is about 0.5:
galton_heights %>% 
    summarise(r = cor(father, son))


# 18.2.1 Sample correlation is a random variable
# Before we continue connecting correlation to regression, let’s remind
# ourselves about random variability.
# In most data science applications, we observe data that includes random
# variation. For example, in many cases, we do not observe data for the
# entire population of interest but rather for a random sample. As with
# the average and standard deviation, the sample correlation is the most
# commonly used estimate of the population correlation. This implies that
# the correlation we compute and use as a summary is a random
# variable.

# By way of illustration, let’s assume that the 179 pairs of fathers and
# sons is our entire population. A less fortunate geneticist can only
# afford measurements from a random sample of 25 pairs. The sample
# correlation can be computed with:
R <- sample_n(galton_heights, 25, replace = TRUE) %>% 
    summarise(r = cor(father, son))
R

# R is a rnadom variable, we can run a MC simulation to see its distirbution
B <- 1000
N <- 25
R <- replicate(B, {
    sample_n(galton_heights, N, replace = TRUE) %>% 
        summarise(r = cor(father, son)) %>% 
        pull(r)
})

tibble(R = R) %>% 
    ggplot() +
    geom_histogram(mapping = aes(x = R),
                   binwidth = 0.05, color = 'black')

# We see that the expected value of R is the population correlation:
mean(R)


# and that it has a relatively high standard error relative to the range of values R can take:
sd(R)

# So, when interpreting correlations, remember that correlations derived
# from samples are estimates containing uncertainty.
# Also, note that because the sample correlation is an average of independent
# draws, the central limit actually applies. Therefore, for large enough N ,
# the distribution of R is approximately normal with expected value ρ.
# The standard deviation, which is somewhat complex to derive, is
#
#           sqrt( { 1 - r^2 } / {N - 2})
#

# In our example, N = 25 does not seem to be large enough to make the approximation a good one:
tibble(R = R) %>% 
    ggplot(aes(sample = R)) +
    stat_qq() +
    geom_abline(intercept = mean(R), slope = sqrt((1 - mean(R)^2)/(N - 2)))

# If you increase N, you will see the distribution converging to normal.


# 
# Correlation is only meaningful in a particular context. To help us understand when it
# is that correlation is meaningful as a summary statistic, we will return to the example
# of predicting son’s height using the father’s height. This will help motivate and define
# linear regression. We start by demonstrating how correlation can be useful for prediction.

# 
# A practical way to improve these estimates of the conditional expectations, is to define
# strata of with similar values of x. In our example, we can round father heights to the
# nearest inch and assume that they are all 72 inches. If we do this, we end up with
# the following prediction for the son of a father that is 72 inches tall:
conditional_avg <- galton_heights %>% 
    filter(round(father) == 72) %>% 
    summarise(avg = mean(son)) %>% 
    pull(avg)

conditional_avg    

# 
# Note that a 72 inch father is taller than average - specifically,
# 72 - 69.1/2.5 = 1.1 standard deviations taller than the average father.
# Our prediction 70.836 is also taller than average, but only 0.55 standard
# deviations larger than the average son. The sons of 72 inch fathers
# have regressed some to the average height. We notice that the reduction
# in how many SDs taller is about 0.5, which happens to be the correlation.
# As we will see in a later section, this is not a coincidence.

# 
# If we want to make a prediction of any height, not just 72, we could
# apply the same approach to each strata. Stratification followed by
# boxplots lets us see the distribution of each group:
galton_heights %>% 
    mutate(father_strata = factor(round(father))) %>% 
    ggplot(aes(father_strata, son)) +
    geom_boxplot() +
    geom_point()

# Not surprisingly, the centers of the groups are increasing with height.
galton_heights %>% 
    mutate(father = round(father)) %>% 
    group_by(father) %>% 
    summarise(son_conditional_avg = mean(son)) %>% 
    ggplot(mapping = aes(father, son_conditional_avg)) +
    geom_point()


# Furthermore, these centers appear to follow a linear relationship.
# Below we plot the averages of each group. If we take into account that
# these averages are random variables with standard errors, the data is
# consistent with these points following a straight line:
galton_heights %>% 
    mutate(father = round(father)) %>% 
    group_by(father) %>% 
    summarise(son_conditional_avg = mean(son)) %>% 
    ggplot(mapping = aes(father, son_conditional_avg)) +
    geom_point() +
    geom_smooth(method = 'lm')
    

# Note that if the correlation is positive and lower than 1, our prediction is closer,
# in standard units, to the average height than the value using to predict, x, is
# to the average of the xs. This is why we call it regression: the son regresses to
# the average height. In fact, the title of Galton’s paper was: Regression toward
# mediocrity in hereditary stature.
mu_x <- mean(galton_heights %>% pull(father))
mu_y <- mean(galton_heights %>% pull(son))
s_x <- sd(galton_heights %>% pull(father))
s_y <- sd(galton_heights %>% pull(son))
r <- cor(galton_heights %>% pull(father), galton_heights %>% pull(son))

m <- r * s_y / s_x
b <- mu_y - m * mu_x

galton_heights %>% 
    ggplot(aes(father, son)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = b, slope = m)

# The regression formula implies that if we first standardize the variables, that is
# subtract the average and divide by the standard deviation, then the regression line
# has intercept 0 and slope equal to the correlation ρ. Here is the same plot,
# but using standard units:
galton_heights %>% 
    ggplot(aes(scale(father), scale(son))) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 0, slope = r)


# 18.4.1 Regression improves precision
# Let’s compare the two approaches to prediction that we have presented:
#   1. Round father’s heights to closest inch, stratify, and then take the average.
#   2. Compute the regression line and use it to predict.
# We use a Monte Carlo simulation sampling N = 50 families:
B <- 1000
N <- 50

set.seed(1983)
conditional_avg <- replicate(B, {
    dat <- sample_n(galton_heights, N)
    dat %>% 
        filter(round(father) == 72) %>% 
        summarise(avg = mean(son)) %>% 
        pull(avg)
})

regression_pred <- replicate(B, {
    dat <- sample_n(galton_heights, N)
    mu_x = mean(dat$father)
    mu_y = mean(dat$son)
    s_x = sd(dat$father)
    s_y = sd(dat$son)
    r <- cor(dat$father, dat$son)
    
    mu_y + r * (72 - mu_x) / s_x * s_y
})
#Although the expected value of these two random variables is about the same:
mean(conditional_avg, na.rm = TRUE)
mean(regression_pred)

# The standard error for the regression prediction is
# substantially smaller:
sd(conditional_avg, na.rm = TRUE)
sd(regression_pred)

tibble(reg = regression_pred,
       cnd = conditional_avg) %>% 
    ggplot() +
    geom_histogram(mapping = aes(x = reg), fill = 'steelblue', alpha = 0.4, color = 'black') +
    geom_histogram(mapping = aes(x = cnd), fill = 'red4', alpha = 0.4, color = 'black')

# The regression line is therefore much more stable than the conditional mean.
# There is an intuitive reason for this. The conditional average is computed
# on a relatively small subset: the fathers that are about 72 inches tall.
# In fact, in some of the permutations we have no data, which is why we use
# na.rm=TRUE. The regression always uses all the data.
#
# So why not always use the regression for prediction? Because it is not
# always appropriate. For example, Anscombe provided cases for which the
# data does not have a linear relationship. So are we justified in using
# the regression line to predict? Galton answered this in the positive for
# height data. The justification, which we include in the next section, is
# somewhat more advanced than the rest of the chapter.

# A more technical way to define the bivariate normal distribution is
# the following: if X is a normally distributed random variable, Y is
# also a normally distributed random variable, and the conditional
# distribution of Y for any X = x is approximately normal, then the pair
# is approximately bivariate normal.
# If we think the height data is well approximated by the bivariate normal
# distribution, then we should see the normal approximation hold for
# each strata. Here we stratify the son heights by the standardized
# father heights and see that the assumption appears to hold:
galton_heights %>% 
    mutate(z_father = round((father - mean(father)) / sd(father))) %>% 
    filter(z_father %in% -2:2) %>% 
    ggplot() +
    stat_qq(aes(sample = son)) +
    facet_wrap(~ z_father)


# Now we come back to defining correlation. Galton used mathematical statistics to
# demonstrate that, when two variables follow a bivariate normal distribution, computing
# the regression line is equivalent to computing conditional expectations. 

#  if our data is approximately bivariate, then the conditional expectation, the best
# prediction of Y given we know the value of X, is given by the regression line.

