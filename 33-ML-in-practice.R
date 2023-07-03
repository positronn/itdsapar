# 33-ML-in-practice.R
library(tidyverse)
library(dslabs)
mnist <- read_mnist()

# The dataset includes two components, a training set and test set:
names(mnist)


# Each of these components includes a matrix with features in the columns:
dim(mnist$train$images)
# and vector with the classes as integers:
class(mnist$train$labels)
table(mnist$train$labels)

# Because we want this example to run on a small laptop and in less than one hour, we will consider a subset of the dataset. We will sample 10,000 random rows from the training set and 1,000 random rows from the test set:
set.seed(1990)
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

# In machine learning, we often transform predictors before running the machine algorithm. We also remove predictors that are clearly not useful. We call these steps preprocessing.
# Examples of preprocessing include standardizing the predictors, taking the log transform of some predictors, removing predictors that are highly correlated with others, and removing predictors with very few non-unique values or close to zero variation. We show an example below.

# We can run the nearZero function from the caret package to see that several features do not vary much from observation to observation. We can see that there are a large number of features with 0 variability:
library(matrixStats)
sds <- colSds(x)
ggplot() +
    geom_histogram(mapping = aes(x = sds), bins = 256)

# This is expected because there are parts of the image that rarely contain writing (dark pixels).
# The caret packages includes a function that recommends features to be removed due to near zero variance:
library(caret)
nzv <- nearZeroVar(x)

# We can see the columns recommended for removal:
image(matrix(1:784 %in% nzv, 28, 28))
# So we end up keeping this number of columns:
col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

# 
# Now we are ready to fit some models. Before we start, we need to add column names to the feature matrices as these are required by caret:
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(mnist$train$images)


# 33.2 k-Nearest Neighbor and Random Forest
# Let’s start with kNN. The first step is to optimize for k. Keep in mind that when we run the algorithm, we will have to compute a distance between each observation in the test set and each observation in the training set. These are a lot of computations. We will therefore use k-fold cross validation to improve speed.
# If we run the following code, the computing time on a standard laptop will be several minutes.
control <- trainControl(method = 'cv', number = 10, p = 0.9)
train_knn <- train(x[, col_index], y,
                   method = 'knn',
                   tuneGrid = data.frame(k = c(3, 5, 7)),
                   trControl = control)
train_knn

# In general, it is a good idea to try a test run with a subset of the data to get an idea of timing before we start running code that might take hours to complete. We can do this as follows:
n <- 1000
b <- 2
index <- sample(nrow(x), n)
control <- trainControl(method = 'cv', number = b, p = 0.9)
train_knn <- train(x[index, col_index], y[index],
                   method = 'knn',
                   tuneGrid = data.frame(k = c(3, 5, 6)),
                   trControl = control)

# We can then increase n and b and try to establish a pattern of how they affect computing time to get an idea of how long the fitting process will take for larger values of n and b. You want to know if a function is going to take hours, or even days, before you run it.
fit_knn <- knn3(x[, col_index], y, k = 3)

y_hat_knn <- predict(fit_knn, x_test[, col_index], type="class")
cm <- confusionMatrix(y_hat_knn, factor(y_test))
cm$overall["Accuracy"]
cm$byClass[,1:2]


# Now let’s see if we can do even better with the Random Forest algorithm.
# With random forest, computation time is a challenge. For each forest, we need to build hundreds of trees. We also have several parameters we can tune. We use the random forest implementation in the Rborist package, which is faster than the one in the randomForest package.
# Because with random forest the fitting is the slowest part of the procedure rather than the predicting (as with kNN), we will use only 5-fold cross validation. We will also reduce the number of trees that are fit since we are not yet building our final model.
# Finally, to compute on a smaller dataset, we will take a random sample of the observations when constructing each tree. We can change this number with the nSamp argument.
library(Rborist)
control <- trainControl(method="cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1) , predFixed = c(10, 15, 35))

train_rf <- train(x[, col_index],
                  y,
                  method = 'Rborist',
                  nTree = 50,
                  trControl = control,
                  tuneGrid = grid,
                  nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune


# Now that we have optimized our tree, we are ready to fit our final model:
fit_rf <- Rborist(x[, col_index], y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[ ,col_index])$yPred])
cm <- confusionMatrix(y_hat_rf, y_test)
cm$overall["Accuracy"]



# 33.3 Variable importance
# Unfortunately, the Rborist implementation of random forest does not yet support importance calculations, so we demonstrate with a quick fit using the randomForest package.
library(randomForest)
rf <- randomForest(x, y, ntree = 50)

# The following function computes the importance of each feature:
imp <- importance(rf)

# We can see which features are most being used by plotting an image:
image(matrix(imp, 28, 28))


# 33.5 Ensembles
# The idea of an ensemble is similar to the idea of combining data from different pollsters to obtain a better estimate of the true support for each candidate.
# In machine learning, one can usually greatly improve the final results by combining the results of different algorithms.
# Here is a simple example where we compute new class probabilities by taking the average of random forest and kNN. We can see that the accuracy improves to 0.96:
p_rf <- predict(fit_rf, x_test[, col_index])$census
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[, col_index])
p <- (p_rf + p_knn) / 2
y_pred <- factor(apply(p, 1, which.max) - 1)
confusionMatrix(y_pred, y_test)$overall['Accuracy']


