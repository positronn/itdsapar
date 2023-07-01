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
# The resulting predictions are similar. This is because the two estimates of p(x) are larger than 1/2 in about the same region of x:

tibble(x = seq(60, 80)) %>% 
    mutate(logistic = plogis(glm_fit$coef[1] + glm_fit$coef[2] * x),
           regression = lm_fit$coef[1] + lm_fit$coef[2] * x) %>% 
    pivot_longer(-x, names_to = "method", values_to = "p_x") %>% 
    ggplot(aes(x, p_x, color = method)) +
    geom_line() +
    geom_hline(yintercept = 0.5, lty = 5)

# 32.3.2 Logistic regression with more than one predictor
fit_glm <- glm(y ~ x_1 + x_2, data = mnist_27$train, family = 'binomial')
p_hat_glm <- predict(fit_glm, mnist_27$test)
y_hat_glm <- factor(if_else(p_hat_glm > 0.5, 7, 2))
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)$overall['Accuracy']

# Comparing to the results we obtained in Section 28.8, we see that logistic regression performs similarly to regression. This not surprising, given that the estimate of pˆ(x1,x2) looks similar as well:
p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>% 
    mutate(p_hat = p_hat) %>% 
    ggplot(aes(x_1, x_2, z = p_hat, fill = p_hat)) +
    geom_raster() +
    scale_fill_gradientn(colors = c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks = c(0.5), color = 'black')



# 32.5 k-nearest neighbors
# We introduced the kNN algorithm in Section 30.1 and demonstrated how we use cross validation to pick k in Section 31.2. Here we quickly review how we fit a kNN model using the caret package. In Section 31.2 we introduced the following code to fit a kNN model:
train_knn <- train(y ~ ., method = 'knn',
                   data = mnist_27$train,
                   tuneGrid = data.frame(k = seq(9, 71, 2)))

# We saw that the parameter that maximized the estimated accuracy was:
train_knn$bestTune

# This model improves the accuracy over regression and logistic regression:
confusionMatrix(predict(train_knn, mnist_27$test, type = 'raw'),
                mnist_27$test$y)$overall['Accuracy']

# 32.7.1 Naive Bayes
library(tidyverse)
library(caret)

library(dslabs)
data('heights')

y <- heights$height
set.seed(1995)

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% 
    slice(-test_index)
test_set <- heights %>% 
    slice(test_index)

# In this case, the Naive Bayes approach is particularly appropriate because we know that the normal distri- bution is a good approximation for the conditional distributions of height given sex for both classes Y = 1 (female) and Y = 0 (Male).
#  This implies that we can approximate the conditional distributions fX|Y =1 and
# fX|Y =0 by simply estimating averages and standard deviations from the data:
params <- train_set %>% 
    group_by(sex) %>% 
    summarise(avg = mean(height),
              sd = sd(height))
params

# The prevalence, which we will denote with π = Pr(Y = 1), can be estimated from the data with:
pi <- train_set %>% 
    summarize(pi = mean(sex == 'Female')) %>% 
    pull(pi)
pi

# Now we can use our estimates of average and standard deviation to get an actual rule:
x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])
p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))


# Our Naive Bayes estimate pˆ(x) looks a lot like our logistic regression estimate:
ggplot() +
    geom_point(aes(x = x, y = p_hat_bayes))


# 32.7.2 Controlling prevalence
# One useful feature of the Naive Bayes approach is that it includes a parameter to account for differences in prevalence. Using our sample, we estimated fX|Y =1, fX|Y =0 and π. If we use hats to denote the estimates, we can write pˆ(x) as:

# As we discussed earlier, our sample has a much lower prevalence, 0.21, than the general population. So if we
# use the rule pˆ(x) > 0.5 to predict females, our accuracy will be affected due to the low sensitivity:
y_hat_bayes <- if_else(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data  =factor(y_hat_bayes), reference = factor(test_set$sex))

# Again, this is because the algorithm gives more weight to specificity to account for the low prevalence:
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))

# This is due mainly to the fact that πˆ is substantially less than 0.5, so we tend to predict Male more often. It makes sense for a machine learning algorithm to do this in our sample because we do have a higher percentage of males. But if we were to extrapolate this to a general population, our overall accuracy would be affected by the low sensitivity.
# The Naive Bayes approach gives us a direct way to correct this since we can simply force πˆ to be whatever value we want it to be. So to balance specificity and sensitivity, instead of changing the cutoff in the decision rule, we could simply change πˆ to 0.5 like this:
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5))
y_hat_bayes_unbiased <- if_else(p_hat_bayes_unbiased > 0.5, 'Female', 'Male')

# Note the difference in sensitivity with a better balance:
sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))

# The new rule also gives us a very intuitive cutoff between 66-67, which is about the middle of the female and male average heights:
ggplot() +
    geom_line(aes(x, p_hat_bayes_unbiased)) +
    geom_hline(yintercept = 0.5, lty = 2) +
    geom_vline(xintercept = 67, lty=2)



# 32.7.3 Quadratic Discriminant Analysis
# 
# Quadratic Discriminant Analysis (QDA) is a version of Naive Bayes in which we assume that the distributions pX|Y =1(x) and pX|Y =0(x) are multivariate normal. The simple example we described in the previous section is actually QDA. Let’s now look at a slightly more complicated case: the 2 or 7 example.
data('mnist_27')
# In this case, we have two predictors so we assume each one is bivariate normal. This implies that we need to estimate two averages, two standard deviations, and a correlation for each case Y = 1 and Y = 0. Once we have these, we can approximate the distributions fX1 ,X2 |Y =1 and fX1 ,X2 |Y =0 . We can easily estimate parameters from the data:
params <- mnist_27$train %>% 
    group_by(y) %>% 
    summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
              sd_1 = sd(x_1), sd_2 = sd(x_2),
              r = cor(x_1, x_2))
params


# Here we provide a visual way of showing the approach. We plot the data and use contour plots to give an idea of what the two estimated normal densities look like (we show the curve representing a region that includes 95% of the points):
mnist_27$train %>% 
    mutate(y = factor(y)) %>% 
    ggplot(aes(x_1, x_2, fill = y, color = y)) +
    geom_point(show.legend = FALSE) +
    stat_ellipse(type = 'norm', lwd = 1.5)


# This defines the following estimate of f(x1,x2).
# We can use the train function from the caret package to fit the model and obtain predictors:
library(caret)
train_qda <- train(y ~ ., method = "qda", data = mnist_27$train)
# We see that we obtain relatively good accuracy:
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

# One reason QDA does not work as well as the kernel methods is perhaps because the assumption of normality does not quite hold. Although for the 2s it seems reasonable, for the 7s it does seem to be off. Notice the slight curvature in the points for the 7s:
mnist_27$train %>% 
    mutate(y = factor(y)) %>% 
    ggplot(aes(x_1, x_2, fill = y, color = y)) +
    geom_point(show.legend = FALSE) +
    stat_ellipse(type = 'norm') +
    facet_wrap(~ y)

# 2 predictors and had to compute 4 means, 4 SDs and 2 correlations. How many parameters would we have if instead of 2 predictors, we had 10? The main problem comes from estimating correlations for 10 predictors. With 10, we have 45 correlations for each class. In general, the formula is K × p(p − 1)/2, which gets big fast. Once the number of parameters approaches the size of our data, the method becomes impractical due to overfitting.



# 32.7.4 Linear discriminant analysis
# In this case, we would compute just one pair of standard deviations and one correlation, so the parameters would look something like this:
params <- mnist_27$train %>% 
    group_by(y) %>% 
    summarise(avg_1 = mean(x_1), avg_2 = mean(x_2), sd_1 = sd(x_1), sd_2 = sd(x_2),
              r = cor(x_1, x_2))

params <- params %>% 
    mutate(sd_1 = mean(sd_1), sd_2 = mean(sd_2), r = mean(r))
params


# In the case of LDA, the lack of flexibility does not permit us to capture the non-linearity in the true conditional probability function.
# Note we can fit the LDA model using caret:
train_lda <- train(y ~ .,
                   method = 'lda',
                   data = mnist_27$train)
y_hat <- predict(train_lda,  mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall['Accuracy']



#32.7.5 Connection to distance
# 32.7.6 Case study: more than three classes
if(!exists("mnist")) mnist <- read_mnist()
set.seed(3456)
index_127 <- sample(which(mnist$train$labels %in% c(1, 2, 7)), 2000)
y <- mnist$train$labels[index_127]
x <- mnist$train$images[index_127, ]
index_train <- createDataPartition(y, p = 0.8, list = FALSE)

# get the quadrants
# temporary object to help figure out the quadrants
row_column <- expand.grid(row = 1:28, col = 1:28)
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)

# # binarize the values. Above 200 is ink, below is no ink
x <- x > 200
# cbind proportion of pixels in upper right quadrant and
## proportion of pixels in lower right quadrant
x <- cbind(rowSums(x[ , upper_left_ind]) / rowSums(x),
           rowSums(x[ , lower_right_ind]) / rowSums(x))

train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train, 1],
                        x_2 = x[index_train, 2])
test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train, 1],
                       x_2 = x[-index_train, 2])


# Here is the training data:
train_set %>% 
    ggplot(aes(x_1, x_2, color = y)) +
    geom_point()

# We use the caret package to train the QDA model:
train_qda <- train(y ~ ., method = "qda", data = train_set)
# Now we estimate three conditional probabilities (although they have to add to 1):
predict(train_qda, test_set, type = "prob") %>% head()

# And our predictions are one of the three classes:
predict(train_qda, test_set) %>% head()

#So the confusion matrix is a 3 by 3 table:
confusionMatrix(predict(train_qda, test_set), test_set$y)$table

confusionMatrix(predict(train_qda, test_set), test_set$y)$overall["Accuracy"]

train_lda <- train(y ~ ., method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$y)$overall["Accuracy"]

# The accuracy is much worse because the model is more rigid. This is what the decision rule looks like:
# The results for kNN are much better:
train_knn <- train(y ~ ., method = 'knn', tuneGrid = data.frame(k = seq(15, 51, 2)),
                   data = train_set)
confusionMatrix(predict(train_knn, test_set), test_set$y)$overall["Accuracy"]

# Note that one of the limitations of LDA here is due to the lack of fit of the normal assumption, in particular for class 1.

train_set %>% 
    mutate(y = factor(y)) %>% 
    ggplot(aes(x_1, x_2, fill = y, color = y)) +
    geom_point(show.legend = FALSE) +
    stat_ellipse(type = 'norm')


# Generative models can be very powerful, but only when we are able to successfully approximate the joint distribution of predictors conditioned on each class.



# 32.9 Classification and Regression Trees (CART)
# 32.9.1 The curse of dimensionality
p <- 1:100
ggplot() +
geom_point(aes(p, .1 ^ (1 / p))) +
    ylim(c(0, 1))

# By the time we reach 100 predictors, the neighborhood is no longer very local, as each side covers almost the entire dataset.
# Here we look at a set of elegant and versatile methods that adapt to higher dimensions and also allow these regions to take more complex shapes while still producing models that are interpretable. These are very popular, well-known and studied methods. We will concentrate on regression and decision trees and their extension to random forests.

# 32.9.2 CART motivation
# To motivate this section, we will use a new dataset that includes the breakdown of the composition of olive oil into 8 fatty acids:
data('olive')
olive %>% 
    as_tibble()
# For illustrative purposes, we will try to predict the region using the fatty acid composition values as predic- tors.
table(olive$region)

# We remove the area column because we won’t use it as a predictor.
olive <- select(olive, -area)
# Let’s very quickly try to predict the region using kNN:
library(caret)
fit <- train(region ~ ., method = 'knn',
             tuneGrid = data.frame(k = seq(1, 15, 2)),
             data = olive)
ggplot(fit)

# We see that using just one neighbor, we can predict relatively well. However, a bit of data exploration reveals that we should be able to do even better. For example, if we look at the distribution of each predictor stratified by region we see that eicosenoic is only present in Southern Italy and that linoleic separates Northern Italy from Sardinia.
olive %>% 
    as_tibble() %>% 
    gather(fatty_acid, percentage, -region) %>% 
    ggplot(aes(region, percentage, fill = region)) +
    geom_boxplot() +
    facet_wrap(~ fatty_acid, scales = 'free') +
    theme(axis.text.x = element_blank())

# This implies that we should be able to build an algorithm that predicts perfectly! We can see this clearly by plotting the values for eicosenoic and linoleic.
p <- olive %>% 
    ggplot(aes(eicosenoic, linoleic, color = region)) +
    geom_point() 
p

# In Section 34.3.4 we define predictor spaces. The predictor space here consists of eight-dimensional points with values between 0 and 100. In the plot above, we show the space defined by the two predictors eicosenoic and linoleic, and, by eye, we can construct a prediction rule that partitions the predictor space so that each partition contains only outcomes of a one category. This in turn can be used to define an algorithm with perfect accuracy.
p + geom_vline(xintercept = 0.065, lty = 2) +
    geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = 'black', lty=2)

# Specifically, we define the following decision rule. If eicosenoic is larger than 0.065, predict Southern Italy. If not, then if linoleic is larger than 10.535, predict Sardinia, and if lower, predict Northern Italy. We can draw this decision tree like this:
# A tree is basically a flow chart of yes or no questions. The general idea of the methods we are describing is to define an algorithm that uses data to create these trees with predictions at the ends, referred to as nodes. Regression and decision trees operate by predicting an outcome variable Y by partitioning the predictors.

# 32.9.3 Regression trees
data("polls_2008")
qplot(day, margin, data = polls_2008)

library(rpart)
fit <- rpart(margin ~ ., data = polls_2008)

# Here, there is only one predictor. Thus we do not have to decide which predictor j to split by, we simply
# have to decide what value s we use to split. We can visually see where the splits were made:
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

# The first split is made on day 39.5. One of those regions is then split at day 86.5. The two resulting new partitions are split on days 49.5 and 117.5 respectively, and so on. We end up with 8 partitions. The final
# estimate f(x) looks like this:
polls_2008 %>% 
    mutate(y_hat = predict(fit)) %>% 
    ggplot() +
    geom_point(aes(day, margin)) +
    geom_step(aes(day, y_hat), col = 'red')
# Note that the algorithm stopped partitioning at 8. Now we explain how this decision is made.



# First we need to define the term complexity parameter (cp). Every time we split and define two new partitions, our training set RSS decreases. This is because with more partitions, our model has more flexibility to adapt to the training data. In fact, if you split until every point is its own partition, then RSS goes all the way down to 0 since the average of one value is that same value. To avoid this, the algorithm sets a minimum for how much the RSS must improve for another partition to be added. This parameter is referred to as the complexity parameter (cp). The RSS must improve by a factor of cp for the new partition to be added. Large values of cp will therefore force the algorithm to stop earlier which result in less nodes.
# However, cp is not the only parameter used to decide if we should partition a current partition or not. An- other common parameter is the minimum number of observations required in a partition before partitioning it further. The argument used in the rpart function is minsplit and the default is 20. The rpart imple- mentation of regression trees also permits users to determine a minimum number observations in each node. The argument is minbucket and defaults to round(minsplit/3).
fit <- rpart(margin ~ ., data = polls_2008, control = rpart.control(cp = 0, minsplit = 2))
polls_2008 %>% 
    mutate(y_hat = predict(fit)) %>% 
    ggplot() +
    geom_point(aes(day, margin)) +
    geom_step(aes(day, y_hat), col = 'red')



# Intuitively we know that this is not a good approach as it will generally result in over-training. These cp, minsplit and minbucket three parameters can be used to control the variability of the final predictors. The larger these values are the more data is averaged to compute a predictor and thus reduce variability. The drawback is that it restrict flexibility.
# So how do we pick these parameters? We can use cross validation, described in Section 30, just like with any tuning parameter. Here is an example of using cross validation to chose cp.
library(caret)
train_rpart <- train(margin ~ .,
                     method = 'rpart',
                     tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)),
                     data = polls_2008)
ggplot(train_rpart)

# To see the resulting tree, we access the finalModel and plot it:
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

# And because we only have one predictor, we can actually plot f(x):
polls_2008 %>% 
    mutate(y_hat = predict(train_rpart)) %>% 
    ggplot() +
    geom_point(aes(day, margin)) +
    geom_step(aes(day, y_hat), col ='red')

# Note that if we already have a tree and want to apply a higher cp value, we can use the prune function. We call this pruning a tree because we are snipping off partitions that do not meet a cp criterion. We previously created a tree that used a cp = 0 and saved it to fit. We can prune it like this:
pruned_fit <- prune(fit, cp = 0.01)


# 32.9.4 Classification (decision) trees

# Classification trees, or decision trees, are used in prediction problems where the outcome is categorical. We use the same partitioning principle with some differences to account for the fact that we are now working with a categorical outcome.
# The second is that we can no longer use RSS to choose the partition. While we could use the naive approach of looking for partitions that minimize training error, better performing approaches use more sophisticated metrics. Two of the more popular ones are the Gini Index and Entropy.
# In a perfect scenario, the outcomes in each of our partitions are all of the same category since this will permit perfect accuracy. The Gini Index is going to be 0 in this scenario, and become larger the more we deviate from this scenario. To define the Gini Index, we define pˆj,k as the proportion of observations in partition j that are of class k. 
# Let us look at how a classification tree performs on the digits example we examined before: We can use this code to run the algorithm and plot the resulting tree:
train_rpart <- train(y ~ .,
                     method = 'rpart',
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
plot(train_rpart)
# The accuracy achieved by this approach is better than what we got with regression, but is not as good as what we achieved with kernel methods:
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall['Accuracy']
# Classification trees have certain advantages that make them very useful. They are highly interpretable, even more so than linear models. They are easy to visualize (if small enough). Finally, they can model human decision processes and don’t require use of dummy predictors for categorical variables. On the other hand, the approach via recursive partitioning can easily over-train and is therefore a bit harder to train than, for example, linear regression or kNN. Furthermore, in terms of accuracy, it is rarely the best performing method since it is not very flexible and is highly unstable to changes in training data. Random forests, explained next, improve on several of these shortcomings.

# 32.10 Random Forests
# Random forests are a very popular machine learning approach that addresses the shortcomings of decision trees using a clever idea. The goal is to improve prediction performance and reduce instability by averag- ing multiple decision trees (a forest of trees constructed with randomness). It has two features that help accomplish this.
# The first step is bootstrap aggregation or bagging. The general idea is to generate many predictors, each using regression or classification trees, and then forming a final prediction based on the average prediction of all these trees. To assure that the individual trees are not the same, we use the bootstrap to induce randomness. These two features combined explain the name: the bootstrap makes the individual trees randomly different, and the combination of trees is the forest. 
# . We will use the randomForest function in the randomForest package:
library(randomForest)
fit <- randomForest(margin ~ ., data = polls_2008)
plot(fit)
# We can see that in this case, the accuracy improves as we add more trees until about 30 trees where accuracy stabilizes.
# The resulting estimate for this random forest can be seen like this:
polls_2008 %>% 
    mutate(y_hat = predict(fit, newdata = polls_2008)) %>% 
    ggplot() +
    geom_point(aes(day, margin)) +
    geom_line(aes(day, y_hat), col = 'red')
# Notice that the random forest estimate is much smoother than what we achieved with the regression tree in the previous section. This is possible because the average of many step functions can be smooth. We can see this by visually examining how the estimate changes as we add more trees. In the following figure you see each of bootstrap samples for several values if b and for each one we see the tree that is fitted in grey, the previous trees that were fitted in lighter grey, and the result of averaging all the trees estimated up to that point.
# Here is the random forest fit for our digits example based on two predictors:
train_rf <- randomForest(y ~ ., data = mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall['Accuracy']

# We can train the parameters of the random forest. Below, we use the caret package to optimize over the minimum node size. To do this with the caret package, we need to use another implementation random that permits us to optimize for this parameter. The function Rborist of the Rborist package permits this. We can run cross validation to choose this parameter using this code:
grid <- expand.grid(mtry = 100,
                    predFixed = 2,
                    minNode = c(3, 50))
train_rf_2 <- train(y ~ .,
                    mehotd = 'Rborist',
                    tuneGrid = grid,
                    data = mnist_27$train)
confusionMatrix(predict(train_rf_2, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]
