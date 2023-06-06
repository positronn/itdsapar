# 19-linear-models.r
library(tidyverse)
library(Lahman)

Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(HR_per_game = HR / G,
          R_per_game = R / G) %>% 
    ggplot(aes(HR_per_game, R_per_game)) +
    geom_point(alpha = 0.5)

# The plot shows a strong association: teams with more HRs tend to score more runs. Now let’s examine the relationship between stolen bases and runs:
Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(SB_per_game = SB / G, R_per_game = R / G) %>% 
    ggplot(aes(SB_per_game, R_per_game)) +
    geom_point(alpha = 0.5)

# Here the relationship is not as clear. Finally, let’s examine the relationship between BB and runs:
Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(BB_per_game = BB / G, R_per_game = R / G) %>% 
    ggplot(aes(BB_per_game, R_per_game)) +
    geom_point(alpha = 0.5)


# In fact, it looks like BB and HRs are also associated:
Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(HR_per_game = HR / G, BB_per_game = BB / G) %>% 
    ggplot(aes(HR_per_game, BB_per_game)) +
    geom_point(alpha = 0.5)

# We know that HR cause runs because, as the name “home run”
# implies, when a player hits a HR they are guaranteed at least
# one run. Could it be that HRs also cause BB and this makes it
# appear as if BB cause runs? When this happens we say there is
# confounding, an important concept we will learn more about
# throughout this chapter.


# Can we use regression with these data? First, notice that the HR and Run data appear to be bivariate normal. We save the plot into the object p as we will use it again later.
p <- Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(HR_per_game = HR / G, R_per_game = R / G) %>% 
    ggplot(aes(HR_per_game, R_per_game)) +
    geom_point(alpha = 0.5)
p


# The qq-plots confirm that the normal approximation is useful here:
Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(z_HR = round((HR - mean(HR)) / sd(HR)),
           R_per_game = R / G) %>% 
    filter(z_HR %in% -2:3) %>% 
    ggplot() +
    stat_qq(aes(sample = R_per_game)) +
    facet_wrap(~ z_HR)


# Now we are ready to use linear regression to predict the number of runs a team will score if we know how many home runs the team hits. All we need to do is compute the five summary statistics:
summary_stats <- Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(HR_per_game = HR / G,
           R_per_game = R / G) %>% 
    summarize(avg_HR = mean(HR_per_game),
              s_HR = sd(HR_per_game),
              avg_R = mean(R_per_game),
              s_R = sd(R_per_game),
              r = cor(HR_per_game, R_per_game))

summary_stats

# and use the formulas given above to create the regression lines:
reg_line <- summary_stats %>%
    summarize(slope = r*s_R/s_HR, intercept = avg_R - slope*avg_HR)

p + geom_abline(intercept = reg_line$intercept, slope = reg_line$slope)

p + geom_smooth(method = "lm")


# 19.2 Confounding
# Previously, we noted a strong relationship between Runs and BB. If we find the regression line for predicting runs from bases on balls, we a get slope of:
bb_slope <- Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(BB_per_game = BB / G,
           R_per_game = R / G) %>% 
    lm(R_per_game ~ BB_per_game, data = .) %>% 
    .$coef %>% 
    .[2]

bb_slope

# So does this mean that if we go and hire low salary players with many BB, and who therefore increase the number of walks per game by 2, our team will score 1.5 more runs per game?
# We are again reminded that association is not causation. The data does provide strong evidence that a team with two more BB per game than the average team, scores 1.5 runs per game. But this does not mean that BB are the cause.
# Note that if we compute the regression line slope for singles we get:
singles_slope <- Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(Singles_per_game = (H - HR - X2B - X3B) / G,
           R_per_game = R / G) %>% 
    lm(R_per_game ~ Singles_per_game, data = .) %>% 
    .$coef %>% 
    .[2]

singles_slope    

# which is a lower value than what we obtain for BB.
# Also, notice that a single gets you to first base just like a BB. Those that know about baseball will tell you that with a single, runners on base have a better chance of scoring than with a BB. So how can BB be more predictive of runs? The reason this happen is because of confounding. Here we show the correlation between HR, BB, and singles:
Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(Singles = (H - HR - X2B - X3B) / G,
           BB = BB / G,
           HR = HR / G) %>% 
    summarise(cor(BB, HR), cor(Singles, HR), cor(BB, Singles))


# It turns out that pitchers, afraid of HRs, will sometimes avoid throwing strikes to HR hitters. As a result, HR hitters tend to have more BB and a team with many HR will also have more BB. Although it may appear that BB cause runs, it is actually the HR that cause most of these runs. We say that BB are confounded with HR. Nonetheless, could it be that BB still help? To find out, we somehow have to adjust for the HR effect. Regression can help with this as well.

# 19.2.1 Understanding confounding through stratification

# As we did when we stratified fathers by rounding to the closest inch, here we can stratify HR per game
# yo the closest ten. We filter out the strata with few points to avoid highly variable estimates:

dat <- Teams %>%
    filter(yearID %in% 1961:2001) %>% 
    mutate(HR_strata = round(HR/G, 1),
          BB_per_game = BB / G,
          R_per_game = R / G) %>%
    filter(HR_strata >= 0.4 & HR_strata <=1.2)


# and then make a scatterplot for each strata:
dat %>% 
    ggplot(aes(BB_per_game, R_per_game)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = 'lm', formula = 'y ~ x') +
    facet_wrap(~ HR_strata)


# Remember that the regression slope for predicting runs with BB was 0.7. Once we stratify by HR, these slopes are substantially reduced:
dat %>% 
    group_by(HR_strata) %>% 
    summarize(slope = cor(BB_per_game, R_per_game) * sd(R_per_game) / sd(BB_per_game))


# 19.3 Least Squared Estimates
# We have described how if data is bivariate normal then the conditional expectations
# follow the regression line. The fact that the conditional expectation is a line is
# not an extra assumption but rather a derived result. However, in practice it
# is common to explicitly write down a model that describes the relationship between
# two or more variables using a linear model.
# We note that “linear” here does not refer to lines exclusively, but rather
# to the fact that the conditional expectation is a linear combinations of known quantities. 


# 19.3.2 Least Squares Estimates (LSE)
# For linear models to be useful, we have to estimate the unknown βs. The standard approach in science is to find the values that minimize the distance of the fitted model to the data.
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

# Let’s write a function that computes the RSS for any pair of values β0 and β1.
rss <- function(beta0, beta1, data) {
    resid <- galton_heights$son - (beta0 + beta1 * galton_heights$father)
    return(sum(resid^2))
}


# So for any pair of values, we get an RSS. Here is a plot of the RSS as a function
# of β1 when we keep the β0 fixed at 25.
beta1 <- seq(0, 1, len = nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% 
    ggplot(aes(beta1, rss)) +
    geom_line() +
    geom_line(aes(beta1, rss))


# We can see a clear minimum for β1 at around 0.65. However, this minimum for β1 is
# for when β0 = 25, a value we arbitrarily picked. We don’t know if (25, 0.65) is the
# pair that minimizes the equation across all possible pairs.
# Trial and error is not going to work in this case. Instead, we will use
# calculus: take the partial derivatives, set them to 0 and solve for β1 and β2.
# Of course, if we have many parameters, these equations can get rather complex.
# But there are functions in R that do these calculations for us.



# 19.3.3 The lm function
# In R, we can obtain the least squares estimates using the the lm function. To fit the model
fit <- lm(son ~ father, data = galton_heights)
fit
summary(fit)

# To understand some of the information included in this summary we need to remember
# that the LSE are random variables. Mathematical statistics gives us some ideas of the
# distribution of these random variables

# 19.3.4 LSE are random variables
# The LSE is derived from the data y1, . . . , yN , which are a realization of random
# variables Y1, . . . , YN . This implies that our estimates are random variables. To see this,
# we can run a Monte Carlo simulation in which we assume the son and father height data
# defines a population, take a random sample of size N = 50 and compute the regression slope
# coefficient for each one:
B <- 1000
N <- 50
lse <- replicate(B, {
    sample_n(galton_heights, N, replace = TRUE) %>% 
        lm(son ~ father, data = .) %>% 
        .$coef
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])
library(gridExtra)
p1 <- lse %>%
    ggplot(aes(beta_0)) +
    geom_histogram(binwidth = 5, color = "black")

p2 <- lse %>%
    ggplot(aes(beta_1)) +
    geom_histogram(binwidth = 0.1, color = "black")
grid.arrange(p1, p2, ncol = 2)

# The reason these look normal is because the central limit theorem applies here as well: for large enough N, the least squares estimates will be approximately normal with expected value β0 and β1 respectively. The standard errors are a bit complicated to compute, but mathematical theory does allow us to compute them and they are included in the summary provided by the lm function. Here it is for one of our simulated data sets:
sample_n(galton_heights, N, replace = TRUE) %>%
    lm(son ~ father, data = .) %>%
    summary %>%
    .$coef

# You can see that the standard errors estimates reported by the summary are close to the standard errors from the simulation:
lse %>%
    summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))


# 19.3.5 Predicted values are random variables
# Once we fit our model, we can obtain prediction of Y by plugging in the estimates into the regression model. For example, if the father’s height is x, then our prediction Yˆ for the son’s height we will:
# Yˆ = βˆ 0 + βˆ 1 x
# When we plot Yˆ versus x, we see the regression line.
# Keep in mind that the prediction Yˆ is also a random variable and mathematical theory tells us what the standard errors are. If we assume the errors are normal, or have a large enough sample size, we can use theory to construct confidence intervals as well. In fact, the ggplot2 layer geom_smooth(method = "lm") that we previously used plots Yˆ and surrounds it by confidence intervals:
galton_heights %>% 
    ggplot(aes(son, father)) +
    geom_point() +
    geom_smooth(method = 'lm')

# The R function predict takes an lm object as input and returns the prediction. If requested, the standard errors and other information from which we can construct confidence intervals is provided:
fit <- galton_heights %>%
    lm(son ~ father, data = .)

Y_hat <- predict(fit, se.fit = TRUE)

names(Y_hat)


# 19.5 Linear regression in the tidyverse
# To see how we use the lm function in a more complex analysis, let’s go back to the baseball example. In a previous example, we estimated regression lines to predict runs for BB in different HR strata. We first constructed a data frame similar to this:
dat <- Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(HR = round(HR / G, 1),
           BB = BB / G,
           R = R / G) %>% 
    select(HR, BB, R) %>% 
    filter(HR >= 0.4 &
           HR <= 1.2)


dat %>% 
    group_by(HR) %>% 
    do(fit = lm(R ~ BB, data = .))


# 19.5.1 The broom package
# Our original task was to provide an estimate and confidence interval for the slope estimates of each strata. The broom package will make this quite easy.
# The broom package has three main functions, all of which extract information from the object returned by lm and return it in a tidyverse friendly data frame. These functions are tidy, glance and augment. The tidy function returns estimates and related information as a data frame:

library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)
tidy(fit, conf.int = TRUE)


# Because the outcome is a data frame, we can immediately use it with do to string together the commands that produce the table we are after. Because a data frame is returned, we can filter and select the rows and columns we want:
dat %>% 
    group_by(HR) %>% 
    reframe(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>% 
    filter(term == 'BB') %>% 
    select(HR, estimate, conf.low, conf.high)


df <- tibble(
    x = c(3, 2, 2, 2, 1),
    y = c(0, 2, 1, 1, 4),
    z1 = c("a", "a", "a", "b", "a"),
    z2 = c("c", "d", "d", "a", "c")
)



# A table like this can then be easily visualized with ggplot2:
dat %>% 
    group_by(HR) %>% 
    reframe(tidy(lm(R ~ BB, data = pick(everything())), conf.int = TRUE)) %>% 
    filter(term == 'BB') %>% 
    select(HR, estimate, conf.low, conf.high) %>% 
    ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
    geom_errorbar() +
    geom_point()

# Now we return to discussing our original task of determining if slopes changed. The plot we just made, using do and tidy, shows that the confidence intervals overlap, which provides a nice visual confirmation that our assumption that the slope does not change is safe.
# The other functions provided by broom, glance and augment, relate to model specific and observation specific outcomes respectively. Here, we can see the model fit summaries glance returns:
glance(fit)


#19.7 Case study: Moneyball (continued)
fit <- Teams %>%
    filter(yearID %in% 1961:2001) %>%
    mutate(BB = BB/G, HR = HR/G, R = R/G) %>%
    lm(R ~ BB + HR, data = .)

tidy(fit, conf.int = TRUE)

# When we fit the model with only one variable, the estimated slopes were 0.735 and 1.845 for BB and HR respectively. Note that when fitting the multivariate model both go down, with the BB effect decreasing much more.
# Now we want to construct a metric to pick players, we need to consider singles, doubles, and triples as well. Can we build a model that predicts runs based on all these outcomes?
# We now are going to take somewhat of a “leap of faith” and assume that these five variables are jointly normal. This means that if we pick any one of them, and hold the other four fixed, the relationship with the outcome is linear and the slope does not depend on the four values held constant. If this is true, then a linear model for our data is:
# Yi = β0 + β1xi,1 + β2xi,2 + β3xi,3 + β4xi,4 + β5xi,5 + εi
# with xi,1, xi,2, xi,3, xi,4, xi,5 representing BB, singles, doubles, triples, and HR respectively.
# Using lm, we can quickly find the LSE for the parameters using:

fit <- Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(BB = BB / G,
           singles = (H - X2B - X3B - HR) / G,
           doubles = X2B / G,
           triples = X3B / G,
           HR = HR / G,
           R = R / G) %>% 
    lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

# To see how well our metric actually predicts runs, we can predict the number of runs for each team in 2002 using the function predict, then make a plot:
Teams %>% 
    filter(yearID %in% 2002) %>% 
    mutate(BB = BB / G,
           singles = (H - X2B-X3B-HR) / G,
           doubles = X2B / G,
           triples = X3B / G,
           HR = HR / G,
           R = R / G) %>% 
    mutate(R_hat = predict(fit, newdata = .)) %>% 
    ggplot(aes(R_hat, R, label = teamID)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, color  = 'black') +
    geom_text(nudge_x = 0.1, cex = 2)

# Our model does quite a good job as demonstrated by the fact that points from the observed versus predicted plot fall close to the identity line.
# So instead of using batting average, or just number of HR, as a measure of picking players, we can use our fitted model to form a metric that relates more directly to run production. Specifically, to define a metric for player A, we imagine a team made up of players just like player A and use our fitted regression model to predict how many runs this team would produce. 




# 
# To make the per-game team rate comparable to the per-plate-appearance player rate, we compute the average number of team plate appearances per game:
pa_per_game <- Batting %>% 
    filter(yearID == 2002) %>% 
    group_by(teamID) %>% 
    summarize(pa_per_game = sum(AB + BB) / max(G)) %>% 
    pull(pa_per_game) %>% 
    mean()
pa_per_game

# We compute the per-plate-appearance rates for players available in 2002 on data from 1999-2001. To avoid small sample artifacts, we filter players with few plate appearances. Here is the entire calculation in one line:
players <- Batting %>% 
    filter(yearID %in% 1999:2001) %>% 
    group_by(playerID) %>% 
    mutate(PA = BB + AB) %>% 
    summarize(G = sum(PA) / pa_per_game,
              BB = sum(BB) / G,
              singles = sum(H - X2B - X3B - HR) / G,
              doubles = sum(X2B) / G,
              triples = sum(X3B) / G,
              HR = sum(HR) / G,
              AVG = sum(H) / sum(AB),
              PA = sum(PA)) %>% 
    filter(PA >= 300) %>% 
    select(-G) %>% 
    mutate(R_hat = predict(fit, newdata = .))
players

players %>% 
    ggplot(mapping = aes(x = R_hat)) +
    geom_histogram(binwidth = 0.5, color = 'black')

# 19.7.1 Adding salary and position information
players <- Salaries %>% 
    filter(yearID == 2002) %>% 
    select(playerID, salary) %>% 
    right_join(players, by = 'playerID') %>% 
    tibble()
players


position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")

(
    tmp_tab <- Appearances %>% 
        filter(yearID == 2002) %>% 
        group_by(playerID) %>% 
        summarize_at(position_names, sum) %>% 
        ungroup()
)

(
    tmp_tab <- Appearances %>% 
        filter(yearID == 2002) %>% 
        group_by(playerID) %>% 
        summarise(across(position_names, ~ sum(.x, na.rm = TRUE))) %>% 
        ungroup()
)


(
    players <- tibble(playerID = tmp_tab$playerID,
                      POS = position_names[pos]) %>% 
        mutate(POS = str_to_upper(str_remove(POS, 'G_'))) %>% 
        filter(POS != 'P') %>% 
        right_join(players, by = 'playerID') %>% 
        filter(!is.na(POS) & !is.na(salary))
)
players


# Finally, we add their first and last name:
players <- Lahman::People %>% 
    select(playerID, nameFirst, nameLast, debut) %>% 
    mutate(debut = as.Date(debut)) %>% 
    right_join(players, by = 'playerID')

players %>% 
    select(nameFirst, nameLast, POS, salary, R_hat) %>% 
    arrange(desc(R_hat)) %>% 
    top_n(10) %>% 
    tibble()

# Notice the very high salaries for most players. In fact, we see that, on average, players with a higher metric have higher salaries:
players %>% 
    ggplot(aes(salary, R_hat, color = POS)) +
    geom_point() +
    scale_x_log10()

# We do see some low cost players with very high metrics. These will be great for our team. Unfortunately, many of these are likely young players that have not yet been able to negotiate a salary and are unavailable. For example, the lowest earner in our top 10 list, Albert Pujols, was a rookie in 2001 and could not negotiate with other teams. 
library(lubridate)

players %>% 
    filter(year(debut) < 1998) %>% 
    ggplot(aes(salary, R_hat, color = POS)) +
    geom_point() +
    scale_x_log10()

# We can search for good deals by looking at players that produce many more runs than others with similar salaries. We can use this table to decide what players to pick and keep our total salary below the 40 million dollars Billy Beane had to work with. This can be done using what computer scientists called linear programming.
library(reshape2)
library(lpSolve)

players <- players %>% 
    filter(year(debut) < 1998)
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep('==', npos), '<=')
constraint_limit <- c(rep(1, npos), 50 * 10 ^ 6)
lp_solution <- lp('max', players$R_hat,
                   constraint_matrix, constraint_dir, constraint_limit,
                   all.int = TRUE)

# This algorithm chooses these 9 players:
our_team <- players %>%
    filter(lp_solution$solution == 1) %>%
    arrange(desc(R_hat))

our_team %>% 
    select(nameFirst, nameLast, POS, salary, R_hat)

# prints only 8....


# We see that most of these players have above average BB and HR rates, while the same is not true for singles:
my_scale <- function(x) (x - median(x)) / mad(x)
players %>% 
    mutate(BB = my_scale(BB),
           singles = my_scale(singles),
           doubles = my_scale(doubles),
           triples = my_scale(triples),
           HR = my_scale(HR),
           AVG = my_scale(AVG),
           R_hat = my_scale(R_hat)) %>% 
    filter(playerID %in% our_team$playerID) %>% 
    select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>% 
    arrange(desc(R_hat)) %>% 
    tibble()

# 19.8 The regression fallacy
library(Lahman)
player_info <- Fielding %>% 
    group_by(playerID) %>% 
    arrange(desc(G)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    left_join(People, by = 'playerID') %>% 
    select(playerID, nameFirst, nameLast, POS)

ROY <- AwardsPlayers %>% 
    filter(awardID == 'Rookie of the Year') %>% 
    left_join(player_info, by = 'playerID') %>% 
    rename(rookie_year = yearID) %>% 
    right_join(Batting, by = 'playerID') %>% 
    mutate(AVG = H / AB) %>% 
    filter(POS != 'P')

# We also will keep only the rookie and sophomore seasons and remove players that did not play sophomore seasons:
ROY <- ROY %>% 
    filter(yearID == rookie_year |
           yearID == rookie_year + 1) %>% 
    group_by(playerID) %>% 
    mutate(rookie = if_else(yearID == min(yearID), 'rookie', 'sophomore')) %>% 
    filter(n() == 2) %>% 
    ungroup() %>% 
    select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

ROY <- ROY %>% 
    pivot_wider(names_from = rookie, values_from = AVG) %>% 
    arrange(desc(rookie))

ROY

#  just by eyeballing, we see the sophomore slump. In fact, the proportion of players that have a lower batting average their sophomore year is:
ROY %>% 
    group_by(is_reg = sophomore - rookie <= 0) %>% 
    count() %>% 
    mutate(perc = n / sum(.$n))

# So is it “jitters” or “jinx”? To answer this question, let’s turn our attention to all players that played the 2013 and 2014 seasons and batted more than 130 times (minimum to win Rookie of the Year). We perform similar operations to what we did above:
two_years <- Batting %>% 
    filter(yearID %in% 2013:2014) %>% 
    group_by(playerID, yearID) %>% 
    filter(sum(AB) >= 130) %>% 
    summarize(AVG = sum(H) / sum(AB)) %>% 
    ungroup() %>% 
    pivot_wider(names_from = yearID, values_from = AVG) %>% 
    filter(!is.na(`2013`) & !is.na(`2014`)) %>% 
    left_join(player_info, by = 'playerID') %>% 
    filter(POS != 'P') %>% 
    select(-POS) %>% 
    arrange(desc(`2013`)) %>% 
    select(nameFirst, nameLast, `2013`, `2014`)

two_years

# But these are not rookies! Also, look at what happens to the worst performers of 2013:
arrange(two_years, `2013`)

# Their batting averages go up! Is this some sort of reverse sophomore slump? It is not. There is no such thing as the sophomore slump. This is all explained with a simple statistical fact: the correlation for performance in two separate years is high, but not perfect:
two_years %>% 
    ggplot() +
    geom_point(mapping = aes(x = `2013`, y = `2014`))

# The correlation is:
summarise(two_years, cor(`2013`, `2014`))

# Because the correlation is not perfect, regression tells us that, on average, expect high performers from 2013 to do a bit worse in 2014. It’s not a jinx; it’s just due to chance. The ROY are selected from the top values of X so it is expected that Y will regress to the mean.

two_years %>% 
    ggplot(mapping = aes(x = `2013`, y = `2014`)) +
    geom_point() +
    geom_smooth(formula = 'y ~ x', method= 'lm') +
    geom_abline()


# 19.9 Measurement error models
library(dslabs)
falling_object <- rfalling_object()

#The assistants hand the data to Galileo and this is what he sees:
falling_object %>% 
    ggplot(aes(time, observed_distance)) +
    geom_point() +
    ylab('Distance in meters') +
    xlab('Time in seconds')

# Galileo does not know the exact equation, but by looking at the plot above, he deduces that the position should follow a parabola, which we can write like this:
# f(x) = β0 +β1x+β2x2

# The data does not fall exactly on a parabola. Galileo knows this is due to measurement error. His helpers
# make mistakes when measuring the distance. To account for this, he models the data with:
# Yi =β0 +β1xi +β2x2i +εi,i=1,...,n

fit <- falling_object %>% 
    mutate(time_sq = time ^ 2) %>% 
    lm(observed_distance ~ time + time_sq, data = .)
tidy(fit)

# Let’s check if the estimated parabola fits the data. The broom function augment lets us do this easily:
augment(fit) %>% 
    ggplot() +
    geom_point(aes(time, observed_distance)) +
    geom_line(aes(time, .fitted), col = 'blue')

tidy(fit, conf.int = TRUE)
