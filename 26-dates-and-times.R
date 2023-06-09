# Parsing Dates and Times
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")

polls_us_election_2016$startdate %>% 
    head
class(polls_us_election_2016$startdate)

as.numeric(polls_us_election_2016$startdate) %>%
    head()

# It turns them into days since the epoch.The as.Date function can convert a character into a date. So to see that the epoch is day 0 we can type
as.Date("1970-01-01") %>% as.numeric

# Plotting functions, such as those in ggplot, are aware of the date format. This means that, for example, a scatterplot can use the numeric representation to decide on the position of the point, but include the string in the labels:
polls_us_election_2016 %>% 
    filter(pollster == 'Ipsos' &
           state == 'U.S.') %>% 
    ggplot(mapping = aes(startdate, rawpoll_trump)) +
    geom_line()


# 26.2 The lubridate package
library(lubridate)
set.seed(2002)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

# The functions year, month and day extract those values:
tibble(date = dates,
       month = month(dates),
       day = day(dates),
       year = year(dates))
month(dates, label = TRUE)

# Another useful set of functions are the parsers that convert strings into dates. The function ymd assumes the dates are in the format YYYY-MM-DD and tries to parse as best as possible.
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4", "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
x
ymd(x)


x <- "09/01/02"
# The ymd function assumes the first entry is the year, the second is the month, and the third is the day, so it
# converts it to:
ymd(x)


# The mdy function assumes the first entry is the month, then the day, then the year:
mdy(x)

# The lubridate package provides a function for every possibility:
ydm(x)
myd(x)
dmy(x)
dym(x)

# The lubridate package is also useful for dealing with times. In R base, you can get the current time typing Sys.time(). The lubridate package provides a slightly more advanced function, now, that permits you to define the time zone:
now()
now('GMT')


now() %>% hour()
now() %>% minute()
now() %>% second()


# It also includes a function to parse strings into times:
x <- c("12:34:56")
hms(x)

# as well as parsers for time objects that include dates:
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)

# Themake_date function can be used to quickly create a date object. It takes three arguments: year, month, day, hour, minute, seconds, and time zone defaulting to the epoch values on UTC time. So create an date object representing, for example, July 6, 2019 we write:
make_date(1970, 7, 6)
# To make a vector of January 1 for the 80s we write:
make_date(1980:1989)


# Another very useful function is the round_date. It can be used to round dates to nearest year, quarter, month, week, day, hour, minutes or seconds. So if we want to group all the polls by week of the year we can do the following:
polls_us_election_2016 %>% 
    mutate(week = round_date(startdate, 'week')) %>% 
    group_by(week) %>% 
    summarize(margin = mean(rawpoll_clinton - rawpoll_trump)) %>% 
    ggplot() +
    geom_point(mapping = aes(x = week, y = margin))
