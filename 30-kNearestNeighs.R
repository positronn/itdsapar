# 30-KNearestNeighs.R
library(tidyverse)
library(dslabs)
data('mnist_27')

mnist_27$test %>% 
    ggplot(aes(x_1, x_2, color=y)) +
    geom_point()

# We will use these data to estimate the conditional probability function
# p(x1,x2)=Pr(Y =1|X1 =x1,X2 =x2).

library(caret)
knn_fit <- knn3(y ~ ., data=mnist_27$train)

# For this function, we also need to pick a parameter: the number of neighbors to include. Let’s start with the default k = 5.
knn_fit <- knn3(y ~ ., data=mnist_27$train, k=5)

# 
# In this case, since our dataset is balanced and we care just as much about sensitivity as we do about specificity, we will use accuracy to quantify performance.
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

# In Section 28.8 we used linear regression to generate an estimate.
fit_lm <- mnist_27$train %>% 
    mutate(y = if_else(y == 7, 1, 0)) %>% 
    lm(y ~ x_1 + x_2, data = .)
p_hat_lm <- fit_lm %>% 
    predict(mnist_27$test)

y_hat_lm <- factor(if_else(p_hat_lm >0.5, 7, 2))
confusionMatrix(data = y_hat_lm, reference = mnist_27$test$y)$overall['Accuracy']


# And we see that kNN, with the default parameter, already beats regression. To see why this is case, we will plot pˆ(x1,x2) and compare it to the the true conditional probability p(x1,x2):
# We see that kNN better adapts to the non-linear shape of p(x1, x2). However, our estimate has some islands of blue in the red area, which intuitively does not make much sense. This is due to what we call over-training. We describe over-training in detail below. Over-training is the reason that we have higher accuracy in the train set compared to the test set:
y_hat_knn <- predict(knn_fit, mnist_27$train, type='class')
confusionMatrix(data=y_hat_knn, reference=mnist_27$train$y)$overall['Accuracy']
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]


# 30.1.1 Over-training
# 
# Over-training is at its worst when we set k = 1. With k = 1, the estimate for each (x1,x2) in the training set is obtained with just the y corresponding to that point. In this case, if the (x1,x2) are unique, we will obtain perfect accuracy in the training set because each point is used to predict itself. Remember that if the predictors are not unique and have different outcomes for at least one set of predictors, then it is impossible to predict perfectly.
# Here we fit a kNN model with k = 1:
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall[["Accuracy"]]

# However, the test set accuracy is actually worse than logistic regression:
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall["Accuracy"]

# 30.1.3 Picking the k in kNN
# So how do we pick k? In principle we want to pick the k that maximizes accuracy, or minimizes the expected MSE as defined in 28.4.8. The goal of cross validation is to estimate these quantities for any given algorithm and set of tuning parameters such as k. To understand why we need a special method to do this let’s repeat what we did above but for different values of k:
ks <- seq(3, 251, 2)

# We do this using map_df function to repeat the above for each one.
library(purrr)
accuracy <- map_df(ks, function(k) {
    fit <- knn3(y ~ ., data = mnist_27$train, k=k)
    
    y_hat <- predict(fit, mnist_27$train, type='class')
    cm_train <- confusionMatrix(data=y_hat, reference=mnist_27$train$y)
    train_error <- cm_train$overall['Accuracy']
    
    y_hat <- predict(fit, mnist_27$test, type = "class")
    cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
    test_error <- cm_test$overall["Accuracy"]
    
    tibble(train = train_error, test = test_error)
})


# 30.5 Bootstrap
# Suppose the income distribution of your population is as follows:
log10(income) %>% 
    ggplot(bins=30, color = 'black')


# The bootstrap permits us to approximate a Monte Carlo simulation without access to the entire distribution. The general idea is relatively simple. We act as if the observed sample is the population. We then sample (with replacement) datasets, of the same sample size as the original dataset. Then we compute the summary statistic, in this case median, on this bootstrap sample.
# Theory tells us that, in many situations, the distribution of the statistics obtained with bootstrap samples approximate the distribution of our actual statistic. This is how we construct bootstrap samples and an approximate distribution:
B <- 10^4
M_stars <- replicate(B, {
    X_star <- sample(X, N, replace=TRUE)
    M_star <- median(X_star)
})
