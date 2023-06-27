# 29-Smoothing.R
# To explain these concepts, we will focus first on a problem with just one predictor. Specifically, we try to estimate the time trend in the 2008 US popular vote poll margin (difference between Obama and McCain).
library(tidyverse)
library(dslabs)
data('polls_2008')


polls_2008 %>% 
    ggplot() +
    geom_point(aes(x = day, y = margin))

# 
# Yi =f(xi)+εi
#

# 29.1 Bin smoothing
span <- 7
fit_box <- with(polls_2008,
            ksmooth(day, margin, x.points = day, kernel='box', bandwidth=span))
fit_norm <- with(polls_2008,
                ksmooth(day, margin, x.points = day, kernel='normal', bandwidth=span))

polls_2008 %>% 
    mutate(smooth_box = fit_box$y) %>% 
    mutate(smooth_norm = fit_norm$y) %>% 
    ggplot(aes(day, margin)) +
    geom_point(size=3, alpha=0.5, color='grey') +
    geom_line(aes(day, smooth_box), color='red') +
    geom_line(aes(day, smooth_norm), color='steelblue') +
    geom_smooth(method='loess', formula='y ~ x')

# 29.2 Kernels
# The final result from the bin smoother is quite wiggly. One reason for this is that each time the window moves, two points change. We can attenuate this somewhat by taking weighted averages that give the center point more weight than far away points, with the two points at the edges receiving very little weight.
# You can think of the bin smoother approach as a weighted average:
#  f(x0) = Sigma w0(xi)Yi


# 29.3 Local weighted regression (loess)
# LOESS (locally estimated scatterplot smoothing) 
# Instead of assuming the function is approximately constant in a window, we assume the function is locally
# linear. We can consider larger window sizes with the linear assumption than with a constant. Instead of the one week window, we consider a larger one in which the trend is approximately linear. We start with a 3 week window and later consider and evaluate other options:
# E[Yi|Xi = xi] = β0 +β1(xi −x0) if |xi −x0| ≤ 21
# For every point x0, loess defines a window and fits a line within that window. Here is an example showing
# the fits for x0 =−125 and x0 =−55:

# The final result is a smoother fit than the bin smoother since we use larger sample sizes to estimate our local parameters:
total_days <- diff(range(polls_2008$day))
span <- 21 / total_days

fit_loess <- loess(margin ~ day, degree=2, span=span, data=polls_2008)
polls_2008 %>% 
    mutate(smooth=fit_loess$fitted) %>% 
    ggplot(aes(day, margin)) +
    geom_point(size=3, alpha=0.5, color='gray') +
    geom_line(aes(day, smooth), color='red')


# 29.3.1 Fitting parabolas
# Taylor’s Theorem also tells us that if you look at any mathematical function closely enough, it looks like a parabola. The theorem also states that you don’t have to look as closely when approximating with parabolas as you do when approximating with lines. This means we can make our windows even larger and fit parabolas instead of lines.

# This is actually the default procedure of the function loess. You may have noticed that when we showed the code for using loess, we set degree = 1. This tells loess to fit polynomials of degree 1, a fancy name for lines. If you read the help page for loess, you will see that the argument degree defaults to 2. By default, loess fits parabolas not lines. Here is a comparison of the fitting lines (red dashed) and fitting parabolas
total_days <- diff(range(polls_2008$day))
span <- 28 / total_days
fit_1 <- loess(margin ~ day, degree=1, span=span, data=polls_2008)
fit_2 <- loess(margin ~ day, span=span, data=polls_2008)

polls_2008 %>% 
    mutate(smooth_1 = fit_1$fitted,
           smooth_2 = fit_2$fitted) %>% 
    ggplot(aes(day, margin)) +
    geom_point(size=3, alpha=0.5, color='grey') +
    geom_line(aes(day, smooth_1), color='red', lty=2) +
    geom_line(aes(day, smooth_2), color='orange', lty=1)
# The degree = 2 gives us more wiggly results. We actually prefer degree = 1 as it is less prone to this kind of noise.


# 29.3.2 Beware of default smoothing parameters
# ggplot uses loess in its geom_smooth function:
polls_2008 %>% 
    ggplot(aes(day, margin)) +
    geom_point() +
    geom_smooth()
# But be careful with default parameters as they are rarely optimal. However, you can conveniently change them:
polls_2008 %>% 
    ggplot(aes(day, margin)) +
    geom_point() +
    geom_smooth(method='loess', formula = 'y ~ x', color='red', span=0.15,
                method.args = list(degree=1))

