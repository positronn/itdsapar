# 25.1 The stringr package
library(tidyverse)
library(stringr)
library(dslabs)

data(reported_heights)

class(reported_heights$height)
# If we try to parse it into numbers, we get a warning:
x <- as.numeric(reported_heights$height)
# Although most values appear to be height in inches as requested:
head(x)
# we do end up with many NAs:
sum(is.na(x))

# We can see some of the entries that are not successfully converted by using filter to keep only the entries resulting in NAs:
reported_heights %>% 
    mutate(new_height = as.numeric(height)) %>% 
    filter(is.na(new_height)) %>% 
    head(n = 10)

not_inches <- function(x, smallest = 50, tallest = 84){
    inches <- suppressWarnings(as.numeric(x))
    ind <- is.na(inches) | inches < smallest | inches > tallest
    ind
}

problems <- reported_heights %>% 
    filter(not_inches(height)) %>% 
    pull(height)

length(problems)

# 25.4 How to escape when defining strings
# To define strings in R, we can use either double quotes:
s <- "Hello!"
# or single quotes:
s <- 'Hello!'
# Make sure you choose the correct single quote since using the back quote will give you an error:
s <- `Hello`
s <- '10"'
s
s <- "10\""
s
# In R, the function cat lets us see what the string actually looks like:
cat(s)
s <- "5'"
cat(s)
s <- '5\'10"'
cat(s)

s <- "5'10\""
cat(s)


# 25.5 Regular expressions


# 25.5.1 Strings are a regexp
# Technically any string is is a regex, perhaps the simplest example is a single character. So the comma , used in the next code example is a simple example of searching with regex.
pattern <- ","
str_subset(reported_heights$height, 'cm')


# 25.5.2 Special characters
# Now let’s consider a slightly more complicated example. Which of the following strings contain the pattern cm or inches?
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)

str_detect(s, "cm") | str_detect(s, "inches")
# above equivalent to:
str_detect(s, "cm|inches")

# Another special character that will be useful for identifying feet and inches values is \d which means any digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9. The backlash is used to distinguish it from the character d. In R, we have to escape the backslash \ so we actually have to use \\d to represent digits. Here is an example:
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"
str_detect(s, pattern)

# We take this opportunity to introduce the str_view function, which is helpful for troubleshooting as it shows us the first match for each string:
str_view_all(s, pattern)


# 25.5.3 Character classes
# Character classes are used to define a series of characters that can be matched. We define character classes with square brackets []. So, for example, if we want the pattern to match only if we have a 5 or a 6, we use the regex [56]:
str_view(s, "[56]")

# Suppose we want to match values between 4 and 7. A common way to define character classes is with ranges. So, for example, [0-9] is equivalent to \\d. The pattern we want is therefore [4-7].
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")
str_view_all(s, "[4-7]")


# 25.5.4 Anchors
# What if we want a match when we have exactly 1 digit? This will be useful in our case study since feet are never more than 1 digit so a restriction will help us. One way to do this with regex is by using anchors, which let us define patterns that must start or end at a specific place. The two most common anchors are ˆ and $ which represent the beginning and end of a string respectively. So the pattern ˆ\\d$ is read as “start of the string followed by one digit followed by end of string”.
pattern <- "^\\d$"
yes <- c('1', '5', '9')
no <- c('12', '123', ' 1', 'a4', 'b')
s <- c(yes, no)
str_view_all(s, pattern)
# The 1 does not match because it does not start with the digit but rather with a space, which is actually not easy to see.

# 25.5.5 Quantifiers
# For the inches part, we can have one or two digits. This can be specified in regex with quantifiers. This is done by following the pattern with curly brackets containing the number of times the previous entry can be repeated. We use an example to illustrate. The pattern for one or two digits is:
pattern <- "^\\d{1,2}$"
yes <- c('1', '4', '9', '12')
no <- c('123', 'a4', 'b')
s <- c(yes, no)
str_view_all(s, pattern)
# In this case, 123 does not match, but 12 does. So to look for our feet and inches pattern, we can add the symbols for feet ' and inches " after the digits.
# With what we have learned, we can now construct an example for the pattern x'y\" with x feet and y inches.
pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"", "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)

# For now, we are permitting the inches to be 12 or larger. We will add a restriction later as the regex for this is a bit more complex than we are ready to show.
# 25.5.6 White space \s
# Another problem we have are spaces. For example, our pattern does not match 5' 4" because there is a space between ' and 4 which our pattern does not permit. Spaces are characters and R does not ignore them:
identical("Hi", "Hi ")
# In regex, \s represents white space. To find patterns like 5' 4, we can change our pattern to:
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)

# However, this will not match the patterns with no space. So do we need more than one regex pattern? It turns out we can use a quantifier for this as well.
# 25.5.7 Quantifiers: *, ?, +
# We want the pattern to permit spaces but not require them. Even if there are several spaces, like in this example 5' 4, we still want it to match. There is a quantifier for exactly this purpose. In regex, the character * means zero or more instances of the previous character. Here is an example:
yes <- c('AB', 'A1B', 'A11B', 'A111B', 'A1111B')
no <- c('A2B', 'A21B')
str_detect(yes, 'A1*B')
str_detect(no, 'A1*B')

# The above matches the first string which has zero 1s and all the strings with one or more 1. We can then improve our pattern by adding the * after the space character \s.
tibble(string = c('AB', 'A1B', 'A11B', 'A111B', 'A1111B'),
       none_or_more = str_detect(yes, 'A1*B'),
       nore_or_once = str_detect(yes, 'A1?B'),
       once_or_more = str_detect(yes, 'A1+B'))


# 25.5.8 Not
# To specify patterns that we do not want to detect, we can use the ˆ symbol but only inside square brackets. Remember that outside the square bracket ˆ means the start of the string. So, for example, if we want to detect digits that are preceded by anything except a letter we can do the following:
pattern <- "[^a-zA-Z]\\d"
yes <- c('.3', '+2', '-0', '4')
no <- c('A3', 'B2', 'CO', 'E4')
str_detect(yes, pattern)
str_detect(no, pattern)

# Another way to generate a pattern that searches for everything except is to use the upper case of the special character. For example \\D means anything other than a digit, \\S means anything except a space, and so on.


# 25.5.9 Groups
# Groups are a powerful aspect of regex that permits the extraction of values. Groups are defined using parentheses. They don’t affect the pattern matching per-se. Instead, it permits tools to identify specific parts of the pattern so we can extract them.
# We want to change heights written like 5.6 to 5'6.
# To avoid changing patterns such as 70.2, we will require that the first digit be between 4 and 7 [4-7] and
# that the second be none or more digits \\d*. Let’s start by defining a simple pattern that matches this:
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"
# We encapsulate the part of the pattern that matches the parts we want to keep for later use. Adding groups does not affect the detection, since it only signals that we want to save what is captured by the groups. Note that both patterns return the same result when using str_detect:
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)
str_match(s, pattern_with_groups)
# Notice that the second and third columns contains feet and inches respectively. The first column is the part of the string matching the pattern. If no match occurred, we see an NA.
# Now we can understand the difference between the functions str_extract and str_match: str_extract extracts only strings that match a pattern, not the values defined by groups:
str_extract(s, pattern_with_groups)


str_detect(s, pattern_with_groups)
str_extract(s, pattern_with_groups)
str_match(s, pattern_with_groups)

# 25.6 Search and replace with regex
# Earlier we defined the object problems containing the strings that do not appear to be in inches. We can see that not too many of our problematic strings match the pattern:
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))

# To see why this is, we show some examples that expose why we don’t have more matches:
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)


# An initial problem we see immediately is that some students wrote out the words “feet” and “inches”. We can see the entries that did this with the str_subset function:
str_subset(problems, "inches")
# We also see that some entries used two single quotes '' instead of a double quote ".
str_subset(problems, "''")
# To correct this, we can replace the different ways of representing inches and feet with a uniform symbol. We will use ' for feet, whereas for inches we will simply not use a symbol since some entries were of the form x'y. Now, if we no longer use the inches symbol, we have to change our pattern accordingly:
pattern <- "^[4-7]'\\d{1,2}$"
# If we do this replacement before the matching, we get many more matches:

problems %>% 
    str_replace('feet|ft|foot', "'") %>% 
    str_replace("inches|in|''|\"", "") %>% 
    str_detect(pattern) %>% 
    sum()

# For now, we improve our pattern by adding \\s* in front of and after the feet symbol ' to permit space between the feet symbol and the numbers. Now we match a few more entries:
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% 
    str_replace('feet|ft|foot', "'") %>% 
    str_replace("inches|in|''|\"", "") %>% 
    str_detect(pattern) %>% 
    sum()

# 25.6.1 Search and replace using groups
# Another powerful aspect of groups is that you can refer to the extracted values in a regex when searching and replacing.
# The regex special character for the i-th group is \\i. So \\1 is the value extracted from the first group, \\2 the value from the second and so on. As a simple example, note that the following code will replace a comma with period, but only if it is between two digits:
pattern_with_groups <- "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")

# We can use this to convert cases in our reported heights.
# We are now ready to define a pattern that helps us convert all the x.y, x,y and x y to our preferred format.
# We need to adapt pattern_with_groups to be bit more flexible and capture all the cases.
pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

str_subset(problems, pattern_with_groups) %>%
    head()
# and will be able to perform the search and replace:
str_subset(problems, pattern_with_groups) %>% 
    str_replace(pattern_with_groups, "\\1'\\2") %>%
    head()


# 25.7 Testing and improving
# Developing the right regex on the first try is often difficult. Trial and error is a common approach to finding the regex pattern that satisfies all desired conditions. In the previous sections, we have developed a powerful string processing technique that can help us catch many of the problematic entries. Here we will test our approach, search for further problems, and tweak our approach for possible improvements. Let’s write a function that captures all the entries that can’t be converted into numbers remembering that some are in centimeters (we will deal with those later):
not_inches_or_cm <- function(x, smallest = 50, tallest = 84) {
    inches <- suppressWarnings(as.numeric(x))
    ind <- !is.na(inches) &
        ((inches >= smallest & inches <= tallest) |
             (inches / 2.54 >= smallest & inches / 2.54 <=tallest))
    !ind
}

problems <- reported_heights %>% 
    filter(not_inches_or_cm(height)) %>% 
    pull(height)

length(problems)



# Let’s see what proportion of these fit our pattern after the processing steps we developed above:
converted <- problems %>% 
    str_replace("feet|foot|ft", "'") %>%
    str_replace("inches|in|''|\"", "") %>%
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

converted[!index]

# For case 1, if we add a '0 after the first digit, for example, convert all 6 to 6'0, then our previously defined
# pattern will match. This can be done using groups:
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")


str_replace(s, "^([56])'?$", "\\1'0")
pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"


yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")
# We will later check if the entries are meters using their numeric values. We will come back to the case study after introducing two widely used function in string processing that will come in handy when developing our final solution for the self reported heights.

# 25.8 Trimming
# We pause temporarily to introduce a In general, spaces at the start or end of the string are uninformative. These can be particularly deceptive because sometimes they can be hard to see:
s <- "Hi "
cat(s)
identical(s, "Hi")
str_trim("5 ' 9 ")


# 25.9 Changing lettercase
s <- c("Five feet eight inches")
str_to_lower(s)


# 25.10 Case study 2: self reported heights (continued)
convert_format <- function(s) {
    s %>% 
        str_replace("feet|foot|ft", "'") %>%
        str_replace_all("inches|in|''|\"|cm|and", "") %>%
        str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>%
        str_replace("^([56])'?$", "\\1'0") %>%
        str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>%
        str_trim()
}


words_to_numbers <- function(s) {
    s %>% 
        str_to_lower() %>% 
        str_replace_all('zero', '0') %>% 
        str_replace_all("one", "1") %>%
        str_replace_all("two", "2") %>%
        str_replace_all("three", "3") %>%
        str_replace_all("four", "4") %>%
        str_replace_all("five", "5") %>%
        str_replace_all("six", "6") %>%
        str_replace_all("seven", "7") %>%
        str_replace_all("eight", "8") %>%
        str_replace_all("nine", "9") %>%
        str_replace_all("ten", "10") %>%
        str_replace_all("eleven", "11")
}

converted <- problems %>% 
    words_to_numbers() %>% 
    convert_format()

remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]
# apart from the cases reported as meters, which we will fix below, they all seem to be cases that are impossible to fix.


# 25.10.1 The extract function
s <- c("5'10", "6'1")
tab <- data.frame(x = s)

tab %>%
    separate(x, c("feet", "inches"), sep = "'")

tab %>% 
    extract(x, c('feet', 'inches'), regex = "(\\d)'(\\d{1,2})")

s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)

tab %>% 
    separate(x, c('feet', 'inches'), sep = "'", fill = 'right')

tab %>% 
    extract(x, c('feet', 'inches'), regex = "(\\d)'(\\d{1,2})")


# 25.10.2 Putting it all together
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"
smallest <- 50
tallest <- 84
new_heights <- reported_heights %>%
    mutate(original = height,
           height = words_to_numbers(height) %>% convert_format()) %>% 
    extract(height, c('feet', 'inches'), regex = pattern, remove = FALSE) %>% 
    mutate_at(c('height', 'feet', 'inches'), as.numeric) %>% 
    mutate(guess = 12 * feet + inches) %>% 
    mutate(height = case_when(
        !is.na(height) & between(height, smallest, tallest) ~ height, #inches
        !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
        !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54,#meters
        !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
            TRUE ~ as.numeric(NA)
    )) %>% 
    select(-guess)


new_heights %>% 
    filter(not_inches(original)) %>% 
    select(original, height) %>% 
    arrange(height) %>% 
    View()
    
    


