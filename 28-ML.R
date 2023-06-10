# Chapter 28
# Introduction to Machine Learning

# Perhaps the most popular data science methodologies come from the field of Machine Learning. Machine learning success stories include the handwritten zip code readers implemented by the postal service, speech recognition technology such as Apple’s Siri, movie recommendation systems, spam and malware detectors, housing price predictors, and driverless cars. Although today Artificial Intelligence and Machine Learning are often used interchangeably, we make the following distinction: while the first artificial intelligence algorithms, such as those used by chess playing machines, implemented decision making based on programmable rules derived from theory or first principles, in Machine Learning decisions are based on algorithms built with data.


# In Machine Learning, data comes in the form of:
# 1. the outcome we want to predict and
# 2. the features that we will use to predict the outcome

# Here we will use Y to denote the outcome and X1, . . . , Xp to denote features. Note that features are sometimes referred to as predictors or covariates. We consider all these to be synonyms.

# To build a model that provides a prediction for any set of observed values X1 = x1, X2 = x2, . . . X5 = x5, we collect data for which we know the outcome

# When the output is continuous we refer to the machine learning task as prediction,
# and the main output of the model is a function f that automatically produces a
# prediction, denoted with yˆ, for any set of predictors: yˆ = f(x1, x2, . . . , xp).
# We use the term actual outcome to denote what we ended up observing. So we want
# the prediction yˆ to match the actual outcome y as best as possible. Because our
# outcome is continuous, our predictions yˆ will not be either exactly right or wrong,
# but instead we will determine an error defined as the difference between the
# prediction and the actual outcome y − yˆ.


# When the outcome is categorical, we refer to the machine learning task as
# classification, and the main output of the model will be a decision rule which
# prescribes which of the K classes we should predict. In this scenario, most
# models provide functions of the predictors for each class k, fk(x1, x2, . . . , xp),
# that are used to make this decision. When the data is binary a typical decision
# rules looks like this: if f1(x1, x2, . . . , xp) > C, predict category 1, if
# not the other category, with C a predetermined cutoff. Because the outcomes are
# categorical, our predictions will be either right or wrong.




# 28.4 Evaluation Metrics
# For our first introduction to machine learning concepts, we will start with a boring and simple example: how to predict sex using height. As we explain machine learning step by step, this example will let us set down the first building block. Soon enough, we will be attacking more interesting challenges. 
library(tidyverse)
library(caret)

library(dslabs)
data(heights)

y <- heights$sex
x <- heights$height


# 28.4.1 Training and test sets
set.seed(2007)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]
# We will now develop an algorithm using only the training set. Once we are done developing the algorithm, we will freeze it and evaluate it using the test set. The simplest way to evaluate the algorithm when the outcomes are categorical is by simply reporting the proportion of cases that were correctly predicted in the test set. This metric is usually referred to as overall accuracy.


# 28.4.2 Overall accuracy
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
# Note that we are completely ignoring the predictor and simply guessing the sex.

# In machine learning applications, it is useful to use factors to represent the categorical outcomes because R functions developed for machine learning, such as those in the caret package, require or recommend that categorical outcomes be coded as factors. So convert y_hat to factors using the factor function:
y_hat <- sample(c('Male', 'Female'), length(test_index),
                replace = TRUE) %>% 
    factor(levels = levels(test_set$sex))


# The overall accuracy is simply defined as the overall proportion that is predicted correctly:
mean(y_hat == test_set$sex)

# Not surprisingly, our accuracy is about 50%. We are guessing!
# Can we do better? Exploratory data analysis suggests we can because, on average, males are slightly taller than females:
heights %>% 
    group_by(sex) %>% summarize(mean(height), sd(height))

#- But how do we make use of this insight? Let’s try another simple approach: predict Male if height is within two standard deviations from the average male:
y_hat <- ifelse(x > 62, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))

# The accuracy goes up from 0.50 to about 0.80:
mean(y == y_hat)

# But can we do even better? In the example above, we used a cutoff of 62, but we can examine the accuracy obtained for other cutoffs and then pick the value that provides the best results. But remember, it is important that we optimize the cutoff using only the training set: the test set is only for evaluation. Although for this simplistic example it is not much of a problem, later we will learn that evaluating an algorithm on the training set can lead to overfitting, which often results in dangerously over-optimistic assessments.
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x) {
    y_hat <- if_else(train_set$height > x, 'Male', 'Female') %>% 
        factor(levels = levels(test_set$sex))
    mean(y_hat == train_set$sex)
})

# We can make a plot showing the accuracy obtained on the training set for males and females:

tibble(x = seq(61, 70),
       y = accuracy) %>% 
    ggplot(mapping = aes(x = x, y = y)) +
    geom_point() +
    geom_line()

max(accuracy)


# which is much higher than 0.5. The cutoff resulting in this accuracy is:
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

# We can now test this cutoff on our test set to make sure our accuracy is not overly optimistic:
y_hat <- if_else(test_set$height > best_cutoff, 'Male', 'Female') %>% 
    factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

# We see that it is a bit lower than the accuracy observed for the training set, but it is still better than guessing. And by testing on a dataset that we did not train on, we know our result is not due to cherry-picking a good result.


# 28.4.3 The confusion matrix
table(predicted = y_hat, actual = test_set$sex)

# If we study this table closely, it reveals a problem. If we compute the accuracy separately for each sex, we
# get
test_set %>% 
    mutate(y_hat = y_hat) %>% 
    group_by(sex) %>% 
    summarize(accuracy = mean(y_hat == sex))

# There is an imbalance in the accuracy for males and females: too many females are predicted to be male. We are calling almost half of the females male! How can our overall accuracy be so high then? This is because the prevalence of males in this dataset is high. These heights were collected from three data sciences courses, two of which had more males enrolled:
prev <- mean(y == "Male")
prev

# So when computing overall accuracy, the high percentage of mistakes made for females is outweighed by the gains in correct calls for men. This can actually be a big problem in machine learning. If your training data is biased in some way, you are likely to develop algorithms that are biased as well. The fact that we used a test set does not matter because it is also derived from the original biased dataset. This is one of the reasons we look at metrics other than overall accuracy when evaluating a machine learning algorithm.
# There are several metrics that we can use to evaluate an algorithm in a way that prevalence does not cloud our assessment, and these can all be derived from the confusion matrix. A general improvement to using overall accuracy is to study sensitivity and specificity separately.

# 28.4.4 Sensitivity and specificity
# In general, sensitivity is defined as the ability of an algorithm to predict a positive outcome when the actual outcome is positive: Yˆ = 1 when Y = 1. Because an algorithm that calls everything positive (Yˆ = 1 no matter what) has perfect sensitivity, this metric on its own is not enough to judge an algorithm. For this reason, we also examine specificity, which is generally defined as the ability of an algorithm to not predict a positive Yˆ = 0 when the actual outcome is not a positive Y = 0. We can summarize in the following way:
# High sensitivity: Y = 1 ⇒ Yˆ = 1
# High specificity: Y = 0 ⇒ Yˆ = 0
# Another way to define specificity is by the proportion of positive calls that are actually positive:
# High specificity: Yˆ = 1 ⇒ Y =1.

# Sensitivity is typically quantified by TP/(TP +FN), This quantity is referred to as the true positive rate (TPR) or recall.
# Specificity is typically quantified as TN/(TN + FP) or the proportion of negatives (the second column = F P + T N ) that are called negatives (T N ). This quantity is also called the true negative rate (TNR).
#  There is another way of quantifying specificity which is TP /(TP + FP ) or the proportion of outcomes called positives (the first row or T P + F P ) that are actually positives (T P ). This quantity is referred to as precision and also as positive predictive value (PPV). 
# Note that, unlike TPR and TNR, precision depends on prevalence since higher prevalence implies you can get higher precision even when guessing.

# The caret function confusionMatrix computes all these metrics for us once we define what category “pos- itive” is.
confusionMatrix(data  = y_hat, reference = test_set$sex)


# 28.4.5 Balanced accuracy and F1 score
# The F_meas function in the caret package computes this summary with beta defaulting to 1.
# Let’s rebuild our prediction algorithm, but this time maximizing the F-score instead of overall accuracy:
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x) {
    y_hat <- if_else(train_set$height > x, 'Male', 'Female') %>% 
        factor(levels = levels(test_set$sex))
    F_meas(data = y_hat, reference = factor(train_set$sex))
})

tibble(cutoff = cutoff,
       F1 = F_1) %>% 
    ggplot(mapping = aes(x = cutoff, y = F1)) +
    geom_point() +
    geom_line()
max(F_1)

# This maximum is achieved when we use the following cutoff:
best_cutoff <- cutoff[which.max(F_1)]
best_cutoff
# A cutoff of 65 makes more sense than 64. Furthermore, it balances the specificity and sensitivity of our confusion matrix:
y_hat <- if_else(test_set$height > best_cutoff, 'Male', 'Female') %>% 
    factor(levels = levels(test_set$sex))
confusionMatrix(data  = y_hat, reference = test_set$sex)
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

# We now see that we do much better than guessing, that both sensitivity and specificity are relatively high, and that we have built our first machine learning algorithm. It takes height as a predictor and predicts female if you are 65 inches or shorter.

# 28.4.7 ROC and precision-recall curves
# When comparing the two methods (guessing versus using a height cutoff), we looked at accuracy and F1. The second method clearly outperformed the first. However, while we considered several cutoffs for the second method, for the first we only considered one approach: guessing with equal probability. Note that guessing Male with higher probability would give us higher accuracy due to the bias in the sample:
p <- 0.9
n <- length(test_index)
y_hat <- sample(c('Male', 'Female'), n, replace = TRUE, prob = c(p, 1 - p)) %>% 
    factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)

# Remember that for each of these parameters, we can get a different sensitivity and specificity. For this reason, a very common approach to evaluating methods is to compare them graphically by plotting both.

# A widely used plot that does this is the receiver operating characteristic (ROC) curve. If you are wondering where this name comes from, you can consult the ROC Wikipedia.
# The ROC curve plots sensitivity (TPR) versus 1 - specificity or the false positive rate (FPR). Here is an ROC curve for guessing sex but using different probabilities of guessing male:
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p) {
    y_hat <- sample(c('Male', 'Female'), n, replace = TRUE, prob = c(p, 1 - p)) %>% 
        factor(levels = c('Female', 'Male'))
    list(method = 'Guessing',
         FPR = 1 - specificity(y_hat, test_set$sex),
         TPR = sensitivity(y_hat, test_set$sex))
})

guessing %>% 
    ggplot() +
    geom_point(mapping = aes(x = FPR, TPR)) +
    xlab('1 - Specificity') +
    ylab('Sensitivity')

# The ROC curve for guessing always looks like a straight line. So how does our second approach compare? We can construct an ROC curve for the height-based approach:
cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x) {
    y_hat <- if_else(test_set$height > x, 'Male', 'Female') %>% 
        factor(levels = c('Female', 'Male'))
    list(method = 'Height cutoff',
         FPR = 1-specificity(y_hat, test_set$sex),
         TPR = sensitivity(y_hat, test_set$sex))
})

# By plotting both curves together, we are able to compare sensitivity for different values of specificity:
bind_rows(guessing, height_cutoff) %>%
    ggplot(aes(FPR, TPR, color = method)) +
    geom_line() +
    geom_point() +
    xlab("1 - Specificity") +
    ylab("Sensitivity")

# We can see that we obtain higher sensitivity with this approach for all values of specificity, which implies it
#is in fact a better method.
# When making ROC curves, it is often nice to add the cutoff used to the points:
library(ggrepel)

map_df(cutoffs, function(x) {
    y_hat <- if_else(test_set$height > x, 'Male', 'Female') %>% 
        factor(levels = c('Female', 'Male'))
    list(method = 'Height cutoff',
         cutoff = x,
         FPR = 1 - specificity(y_hat, test_set$sex),
         TPR = sensitivity(y_hat, test_set$sex))
}) %>% 
    ggplot(aes(x = FPR, y = TPR, label = cutoff)) +
    geom_line() +
    geom_point() +
    geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# ROC curves have one weakness and it is that neither of the measures plotted depend on prevalence. In cases in which prevalence matters, we may instead make a precision-recall plot. The idea is similar, but we instead plot precision against recall:
guessing <- map_df(probs, function(p) {
    y_hat <- sample(c('Male', 'Female'), length(test_index),
                    replace = TRUE, prob = c(p, 1 - p)) %>% 
        factor(levels = c('Female', 'Male'))
    list(method = 'Guess',
         recall = sensitivity(y_hat, test_set$sex),
         precision = precision(y_hat, test_set$sex))
})
height_cutoff <- map_df(cutoffs, function(x) {
    y_hat <- if_else(test_set$height > x, 'Male', 'Female') %>% 
        factor(levels = c('Female', 'Male'))
    list(method = 'Height cutoff',
         recall = sensitivity(y_hat, test_set$sex),
         precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>% 
    ggplot(mapping = aes(recall, precision, color = method)) +
    geom_point() +
    geom_line()


# 28.4.8 The loss function
# Up to now we have described evaluation metrics that apply exclusively to categorical data. Specifically, for binary outcomes, we have described how sensitivity, specificity, accuracy and F1 can be used as quantification. However, these metrics are not useful for continuous outcomes. In this section, we describe how the general approach to defining “best” in machine learning is to define a loss function, which can be applied to both categorical and continuous data.
# The most commonly used loss function is the squared loss function. If yˆ is our predictor and y is the observed outcome, the squared loss function is simply:
#       ( yˆ − y )^2
# Because we often have a test set with many observations, say N, we use the mean squared error (MSE)
# In practice, we often report the root mean squared error because it is in the same units as the outcomes

# If the outcomes are binary, both RMSE and MSE are equivalent to accuracy, since (yˆ − y)2 is 1 if the prediction was correct and 0 otherwise. In general, our goal is to build an algorithm that minimizes the loss so it is as close to 0 as possible.
# Because our data is usually a random sample, we can think of the MSE as a random variable and the observed MSE can be thought of as an estimate of the expected MSE, which in mathematical notation we write like this:
    


# 28.8 Casestudy: isita2ora7?
library(tidyverse)
library(dslabs)
data("mnist_27")

mnist_27$train %>% 
    ggplot(aes(x_1, x_2, color = y)) +
    geom_point()

# p(x1,x2)=Pr(Y =1|X1 =x1,X2 =x2)=β0 +β1x1 +β2x2
fit <- mnist_27$train %>% 
    mutate(y = if_else(y == 7, 1, 0)) %>% 
    lm(y ~ x_1 + x_2, data = .)

# We can now build a decision rule based on the estimate of pˆ(x1,x2):
library(caret)
p_hat <- predict(fit, newdata = mnist_27$test)
y_hat <- factor(if_else(p_hat > 0.5, 7, 2))
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall[["Accuracy"]]


mnist_27$true_p %>% 
    ggplot(mapping = aes(x_1, x_2, fill = p)) +
    geom_raster()


mnist_27$true_p %>% 
    ggplot(mapping = aes(x_1, x_2, z = p, fill = p)) +
    geom_raster() +
    scale_fill_gradientn(colors = c("#F8766D", "white", "#00BFC4")) +
    stat_contour(breaks = c(0.5), color = 'black')
