# 34-large-datasets.R
# Large datasets
# 34.1 Matrix algebra
# In machine learning, situations in which all predictors are numeric, or can be converted to numeric in a meaningful way, are common. The digits data set is an example: every pixel records a number between 0 and 255. Let’s load the data:
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

# In these cases, it is often convenient to save the predictors in a matrix and the outcome in a vector rather than using a data frame. You can see that the predictors are saved as a matrix:
class(mnist$train$images)

x <- mnist$train$images[1:1000, ]
y <- mnist$train$labels[1:1000]


#  we will have to perform mathematical operations (linear algebra) involving several variables. The tidyverse is not developed to perform these types of mathematical operations. For this task, it is convenient to use matrices.

# A matrix can be defined as a series of vectors of the same size joined together as columns:
x_1 <- 1:5
x_2 <- 6:10
cbind(x_1, x_2)

# The dimension of a matrix is often an important characteristic needed to assure that certain operations can be performed. The dimension is a two-number summary defined as the number of rows × the number of columns. In R, we can extract the dimension of a matrix with the function dim:
dim(x)

# Vectors can be thought of as N × 1 matrices. However, in R, a vector does not have dimensions:
dim(x_1)

# Yet we explicitly convert a vector into a matrix using the function as.matrix:
dim(as.matrix(x_1))



# 34.1.2 Converting a vector to a matrix
# We can convert a vector into a matrix with the matrix function and specifying the number of rows and columns that the resulting matrix should have. The matrix is filled in by column: the first column is filled first, then the second second and so on. This example helps illustrate:
my_vector <- 1:15
mat <- matrix(my_vector, 5, 3)
mat

# We can fill by row by using the byrow argument. So, for example, to transpose the matrix mat, we can use:
mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t
# When we turn the columns into rows, we refer to the operations as transposing the matrix. The function t can be used to directly transpose a matrix:
identical(t(mat), mat_t)

# Warning: The matrix function recycles values in the vector without warning if the product of columns and rows does not match the length of the vector:
matrix(my_vector, 5, 5)

# To put the pixel intensities of our, say, 3rd entry, which is a 4 into grid, we can use:
grid <- matrix(x[3, ], 28, 28)

# To confirm that in fact we have done this correctly, we can use the function image which shows an image of its third argument:
image(1:28, 1:28, grid)

# The top of this plot is pixel 1, which is shown at the bottom so the image is flipped. To flip it back we can use:
image(1:28, 1:28, grid[, 28:1])


# 34.1.3 Row and column summaries
# For the first task, related to total pixel darkness, we want to sum the values of each row and then visualize how these values vary by digit.
# The function rowSums takes a matrix as input and computes the desired values:
sums <- rowSums(x)
# We can also compute the averages with rowMeans if we want the values to remain between 0 and 255:
avg <- rowMeans(x)
tibble(labels = as.factor(y), row_averages = avg) %>% 
    ggplot() +
    geom_boxplot(mapping = aes(x = labels, y = row_averages))

# From this plot we see that, not surprisingly, 1s use less ink than the other digits.
# We can compute the column sums and averages using the function colSums and colMeans respectively.
# The matrixStats package adds functions that performs operations on each row or column very efficiently,
# including the functions rowSds and colSds.

# 34.1.4 apply
# The functions just described are performing an operation similar to what sapply and the purrr function map do: apply the same function to a part of your object. In this case, the function is applied to either each row or each column. The apply function lets you apply any function, not just sum or mean, to a matrix. The first argument is the matrix, the second is the dimension, 1 for rows, 2 for columns, and the third is the function. So, for example, rowMeans can be written as:
avgs <- apply(x, 2, mean)
# But notice that just like with sapply and map, we can perform any function. So if we wanted the standard
# deviation for each column, we could write:
sds <- apply(x, 2, sd)


# 34.1.5 Filtering columns based on summaries
# We now turn to task 2: studying the variation of each pixel and removing columns associated with pixels that don’t change much and thus do not inform the classification. Although a simplistic approach, we will quantify the variation of each pixel with its standard deviation across all entries. Since each column represents a pixel, we use the colSds function from the matrixStats package:
library(matrixStats)
sds <- colSds(x)

# A quick look at the distribution of these values shows that some pixels have very low entry to entry variability:
ggplot() +
    geom_histogram(aes(sds), bins=30, color = 'black')


# This makes sense since we don’t write in some parts of the box. Here is the variance plotted by location:
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

# We see that there is little variation in the corners.
# We could remove features that have no variation since these can’t help us predict. In Section 3.5.6, we
# described the operations used to extract columns:
x[ , c(351, 352)]
# and rows
x[c(2, 3), ]

# We can also use logical indexes to determine which columns or rows to keep. So if we wanted to remove uninformative predictors from our matrix, we could write this one line of code:
new_x <- x[ , colSds(x) > 60]
dim(new_x)


# Only the columns for which the standard deviation is above 60 are kept, which removes over half the predictors.
# Here we add an important warning related to subsetting matrices: if you select one column or one row, the result is no longer a matrix but a vector.
class(x[, 1])
dim(x[1, ])

# However, we can preserve the matrix class by using the argument drop=FALSE:
class(x[ , 1, drop=FALSE])
dim(x[, 1, drop=FALSE])


# 34.1.6 Indexing with matrices
# We can quickly make a histogram of all the values in our dataset. We saw how we can turn vectors into matrices. We can also undo this and turn matrices into vectors. The operation will happen by row:
mat <- matrix(1:15, 5, 3)
mat
as.vector(mat)

# To see a histogram of all our predictor data, we can use:
ggplot() +
    geom_histogram(aes(x = as.vector(x)), bins=30)

# We notice a clear dichotomy which is explained as parts of the image with ink and parts without. If we think that values below, say, 50 are smudges, we can quickly make them zero using:
new_x <- x
new_x[new_x < 50] <- 0

# To see what this does, we look at a smaller matrix:
mat <- matrix(1:15, 5, 3)
mat[mat < 3] <- 0
mat

# We can also use logical operations with matrix logical:
mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat



# 34.1.7 Binarizing the data
# The histogram above seems to suggest that this data is mostly binary. A pixel either has ink or does not. Using what we have learned, we can binarize the data using just matrix operations:
bin_x <- x
bin_x[bin_x < 255 / 2] <- 0
bin_x[bin_x < 255 / 2] <- 1

# We can also convert to a matrix of logicals and then coerce to numbers like this:
bin_X <- (x > 255 / 2) * 1


# 34.1.8 Vectorization for matrices
# In R, if we subtract a vector from a matrix, the first element of the vector is subtracted from the first row, the second element from the second row, and so on.
# The same holds true for other arithmetic operations. This implies that we can scale each row of a matrix
# like this:
(x - rowMeans(x)) / rowSds(x)

# If you want to scale each column, be careful since this approach does not work for columns. To perform a similar operation, we convert the columns to rows using the transpose t, proceed as above, and then transpose back:
t(t(x)) - colMeans(x)

# We can also use a function called sweep that works similarly to apply. It takes each entry of a vector and subtracts it from the corresponding row or column.
X_mean_0 <- sweep(x, 2, colMeans(x))
X_mean_0


# The function sweep actually has another argument that lets you define the arithmetic operation. So to divide
# by the standard deviation, we do the following:
X_mean_0 <- sweep(x, 2, colMeans(x))
X_standardized <- sweep(X_mean_0, 2, colSds(x), FUN = '/')


# 34.1.9 Matrix algebra operations
# 1. Matrix multiplication is done with %*%. For example, the cross product is:
t(x) %*% x
# 2. We can compute the cross product directly with the function:
crossprod(x)
# 3. To compute the inverse of a function, we use solve. Here it is applied to the cross product:
solve(crossprod(x))
# 4. The QR decomposition is readily available by using the qr function:
qr(x)
# 34.3 Distance
# Many of the analyses we perform with high-dimensional data relate directly or indirectly to distance. Most clustering and machine learning techniques rely on being able to define distance between observations, using features or predictors.

# 34.3.1 Euclidean distance
# 34.3.2 Distance in higher dimensions
# 34.3.5 Distance between predictors
# To compute the distance between all pairs of the 784 predictors, we can transpose the matrix first and then use dist:
d <- dist(t(x))
dim(as.matrix(d))
# An interesting thing to note here is that if we pick a predictor (a pixel), we can see which pixels are close. That is, the pair of pixels either have ink in the same images (small distance) or they don’t (large distance). The distance between, for example, and all other pixels is given by:
d_492 <- as.matrix(d)[492, ]
# We can now see the spatial pattern of these distances with the following code:
image(1:28, 1:28, matrix(d_492, 28, 28))
# Not surprisingly, points physically nearby are mathematically closer.


#  34.5.1 Preserving distance
# We consider an example with twin heights. Some pairs are adults the others are children. Here we simulate 100 two-dimensional points that represent the number of standard deviations each individual is from the mean height. Each point is a pair of twins. We use the mvrnorm function from the MASS package to simulate bivariate normal data.
set.seed(1988)
library(MASS)
n <- 100
x <- rbind(mvrnorm(n / 2, c(69, 69), matrix(c(9, 9 * 0.9, 9 * 0.92, 9 * 1), 2, 2)),
           mvrnorm(n / 2, c(55, 55), matrix(c(9, 9 * 0.9, 9 * 0.92, 9 * 1), 2, 2)))

ggplot() +
    geom_point(mapping = aes(x = x[, 1], y = x[, 2]), shape = 'o')

# Our features are N two-dimensional points, the two heights, and, for illustrative purposes, we will act as if visualizing two dimensions is too challenging. We therefore want to reduce the dimensions from two to one, but still be able to understand important characteristics of the data, for example that the observations cluster into two groups: adults and children.
# We can compute these distances using dist:
d <- dist(x)
as.matrix(d)[1, 2]
as.matrix(d)[2, 51]

# This distance is based on two dimensions and we need a distance approximation based on just one.

# Let’s start with the naive approach of simply removing one of the two dimensions. 
z <- x[, 1]
d_z <- dist(z)
# Here are the approximate distances versus the original distances:
ggplot() +
    geom_point(mapping = aes(x = as.matrix(d)[,1] %>% as.vector,
                             y = as.matrix(d_z)[,1] %>% as.vector)) +
    geom_abline(slope = 1, intercept = 0, color = 'red') +
    xlab('dist(x)') +
    ylab('dist(z)')
# The plot looks about the same if we use the second dimension. We obtain a general underestimation. This is to be expected because we are adding more positive quantities in the distance calculation as we increase the number of dimensions
# If instead we use an average
# then the underestimation goes away. We divide the distance by √2 to achieve the correction
ggplot() +
    geom_point(mapping = aes(x = as.matrix(d)[,1] %>% as.vector / sqrt(2),
                             y = as.matrix(d_z)[,1] %>% as.vector)) +
    geom_abline(slope = 1, intercept = 0, color = 'red') +
    xlab('dist(x)') +
    ylab('dist(z)')
# This actually works pretty well and we get an typical difference of:
sd(dist(x) - dist(z) * sqrt(2))
# Notice that if we instead plot the difference versus the average:
z <- cbind((x[,2] + x[,1])/2, x[,2] - x[,1])
# we can see how the distance between points is mostly explained by the first dimension: the average.
ggplot() +
    geom_point(mapping = aes(x = z[, 1],
                             y = z[, 2]))

# This means that we can ignore the second dimension and not lose too much information. If the line is completely flat, we lose no information at all. Using the first dimension of this transformed matrix we obtain an even better approximation:
sd((dist(x) - dist(z[,1]) *sqrt(2)))

# Later we learn that z[,1] is the first principal component of the matrix x.

# 34.5.3 Orthogonal transformations
# In our example, to achieve orthogonality, we multiply the first set of coefficients (first column of A) by √2 and the second by 1/√2, then we get the same exact distance if we use both dimensions:
z[,1] <- (x[,1] + x[,2]) / sqrt(2)
z[,2] <- (x[,2] - x[,1]) / sqrt(2)

d_z <- dist(z)
# Here are the approximate distances versus the original distances:
# This gives us a transformation that preserves the distance between any two points:
ggplot() +
    geom_point(mapping = aes(x = as.matrix(d)[,1] %>% as.vector,
                             y = as.matrix(d_z)[,1] %>% as.vector)) +
    geom_abline(slope = 1, intercept = 0, color = 'red') +
    xlab('dist(x)') +
    ylab('dist(z)')


# and an improved approximation if we use just the first dimension:
sd(dist(x) - dist(z[,1]))
# In this case Z is called an orthogonal rotation of X: it preserves the distances between rows.
# Note that by using the transformation above we can summarize the distance between any two pairs of twins with just one dimension. For example, one-dimensional data exploration of the first dimension of Z clearly shows that there are two groups, adults and children:
ggplot() +
    geom_histogram(mapping = aes(x = z[, 1]), bins=20, color = 'black')

# We successfully reduced the number of dimensions from two to one with very little loss of information.
# The reason we were able to do this is because the columns of X were very correlated:
cor(x[,1], x[,2])
# and the transformation produced uncorrelated columns with “independent” information in each column:
cor(z[,1], z[,2])


# One way this insight may be useful in a machine learning application is that we can reduce the complexity of a model by using just Z1 rather than both X1 and X2.


# 34.5.4 Principal Component Analysis
# In the computation above, the total variability in our data can be defined as the sum of the sum of squares of the columns. We assume the columns are centered, so this sum is equivalent to the sum of the variances of each column:
colMeans(x ^ 2)
