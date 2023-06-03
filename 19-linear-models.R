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
