# 30-caret-package.R
#
# The caret package
#
library(tidyverse)
library(dslabs)
data('mnist_27')

# 31.1 The caret train functon
library(caret)
train_glm <- train(y ~ ., method = "glm", data = mnist_27$train)
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)

y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)
confusionMatrix(y_hat_knn, mnist_27$test$y)


# 31.2 Cross validation
getModelInfo("knn")

# We can also use a quick lookup like this:
modelLookup("knn")
# If we run it with default values:
train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = TRUE)

# By default, the cross validation is performed by taking 25 bootstrap samples comprised of 25% of the observations. For the kNN method, the default is to try k = 5, 7, 9. We change this using the tuneGrid parameter. The grid of values must be supplied by a data frame with the parameter names as specified in the modelLookup output.
# Here, we present an example where we try out 30 values between 9 and 67. To do this with caret, we need to define a column named k, so we use this: data.frame(k = seq(9, 67, 2)).
set.seed(2008)
train_knn <- train(y ~ ., method =  'knn',
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))
ggplot(train_knn, highlight = TRUE)

# To access the parameter that maximized the accuracy, you can use this:
train_knn$bestTune
# and the best performing model like this:
train_knn$finalModel

# The function predict will use this best performing model. Here is the accuracy of the best model when applied to the test set, which we have not used at all yet because the cross validation was done on the training set:
confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"), mnist_27$test$y)

# The want to change how we perform cross validation, we can use the trainControl function. We can make the code above go a bit faster by using, for example, 10-fold cross validation. This means we have 10 samples using 10% of the observations each. We accomplish this using the following code:
control <- trainControl(method='cv', number=10, p=0.9)
train_knn_cv <- train(y ~ ., method='knn',
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9, 71, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

# We notice that the accuracy estimates are more variable, which is expected since we changed the number of samples used to estimate accuracy.
# We can also see the standard deviation bars obtained from the cross validation samples:
train_knn$results %>% 
    ggplot(mapping = aes(x = k, y = Accuracy)) +
    geom_line() +
    geom_point() +
    geom_errorbar(mapping = aes(x = k,
                                ymin = Accuracy - AccuracySD,
                                ymax = Accuracy + AccuracySD))

# 31.3 Example: fitting with loess
# The best fitting kNN model approximates the true conditional probability:
# However, we do see that the boundary is somewhat wiggly. This is because kNN, like the basic bin smoother, does not use a kernel. To improve this we could try loess. By reading through the available models we see that we can use the gamLoess method. In the caret manual we see that we need to install the gam package if we have not done so already:
renv::install('gam')
# Then we see that we have two parameters to optimize:
modelLookup("gamLoess")
# We will stick to a degree of 1. But to try out different values for the span, we still have to include a column in the table with the name degree so we can do this:
grid <- expand.grid(span = seq(0.15, 0.65, len=10), degree = 1)
# We will use the default cross validation control parameters.
train_loess <- train(y ~ .,
                     method = 'gamLoess',
                     tuneGrid=grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)


# We can see that the method performs similar to kNN:
confusionMatrix(data = predict(train_loess, mnist_27$test), reference = mnist_27$test$y)$overall['Accuracy']
