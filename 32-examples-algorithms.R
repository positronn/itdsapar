# Chapter 32
# Examples of algorithms

library(tidyverse)
library(dslabs)
library(caret)
data("mnist_27")



# 32.1 Linear regression
library(HistData)

set.seed(1983)
galton_heights <- GaltonFamilies %>% 
    filter(gender == 'male') %>% 
    group_by(family) %>% 
    sample_n(1) %>% 
    ungroup() %>% 
    select(father, childHeight) %>% 
    rename(son = childHeight)

# Suppose you are tasked with building a machine learning algorithm that predicts the son’s height Y using the father’s height X. Let’s generate testing and training sets:
y <- galton_heights$son
test_index <- createDataPartition(y, times=1, p=0.5, list=FALSE)
train_set <- galton_heights %>% 
    slice(-test_index)
test_set <- galton_heights %>% 
    slice(test_index)

# average height of sons.
m <- mean(train_set$son)
# Our squared loss is:
mean((m - test_set$son)^2)
# Can we do better? In the regression chapter, we learned that if the pair (X, Y ) follow a bivariate normal distribution, the conditional expectation (what we want to estimate) is equivalent to the regression line:
fit <- lm(son ~ father, data = train_set)
fit$coef

# We can see that this does indeed provide an improvement over our guessing approach.
y_hat <- fit$coef[1] + fit$coef[2] * test_set$father
mean((y_hat - test_set$son)^2)


# 32.1.1 The predict function
# The predict function is very useful for machine learning applications. This function takes a fitted object from functions such as lm or glm (we learn about glm soon) and a data frame with the new predictors for which to predict. So in our current example, we would use predict like this:
y_hat <- predict(fit, test_set)
# Using predict, we can get the same results as we did previously:
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son) ^ 2)

# predict does not always return objects of the same types; it depends on what type of object is sent to it.


# 32.3 Logistic regression
heights %>% 
    filter(round(height) == 66) %>% 
    summarise(y_hat = mean(sex == 'Female'))


# To construct a prediction algorithm, we want to estimate the proportion of the population that is female for any given height X = x, which we write as the conditional probability described above: Pr(Y = 1|X = x). Let’s see what this looks like for several values of x (we will remove strata of x with few data points):
heights %>% 
    mutate(x = round(height)) %>% 
    group_by(x) %>% 
    filter(n() >= 10) %>% 
    summarise(prop = mean(sex == 'Female')) %>% 
    ggplot(mapping = aes(x, prop)) +
    geom_point()

# Since the results from the plot above look close to linear, and it is the only approach we currently know, we will try regression. We assume that:
#           p(x)=Pr(Y =1|X =x)=β0 +β1x
# Note: because p0(x) = 1 − p1(x), we will only estimate p1(x) and drop the 1 index.
# If we convert the factors to 0s and 1s, we can we can estimate β0 and β1 with least squares.
train_set <- heights %>% 
    slice(-test_index)
test_set <- heights %>% 
    slice(test_index)
lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)


p_hat <- predict(lm_fit, test_set)
y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)


# 32.3.1 Generalized Linear Models
heights %>% 
    mutate(x = round(height)) %>% 
    group_by(x) %>% 
    filter(n() >= 10) %>% 
    summarize(prop = mean(sex == 'Female')) %>% 
    ggplot(aes(mapping = x, prop)) +
    geom_point() +
    geom_abline(intercept = lm_fit$coef[1], slope=lm_fit$coef[2])

range(p_hat)

# But we are estimating a probability: Pr(Y = 1 | X = x) which is constrained between 0 and 1.
glm_fit <- train_set %>% 
    mutate(y = as.numeric(sex == 'Female')) %>% 
    glm(y ~ height, data = ., family = 'binomial')

p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")
# When using predict with a glm object, we have to specify that we want type="response" if we want the
# conditional probabilities, since the default is to return the logistic transformed values.

# Because we have an estimate pˆ(x), we can obtain predictions:
y_hat_logit <- ifelse(p_hat_logit > 0.5, "Female", "Male") %>% factor
confusionMatrix(y_hat_logit, test_set$sex)$overall['Accuracy']
