# DPUK - Linear models training - Day 2 (Tuesday)
# Author: Umberto Noe, Tom Booth


# Intro to R and RStudio
# ======================

# What is R
# What is RStudio
# The RStudio panes
# Updating RStudio (if you have permission)


# R as a calculator
# =================

# What will R print?
1 + 2
5 - 3
2 * 3
1/2

2^3
9^(1/2)
9^0.5

1 + 3 * 10
(1 + 3) * 10

sqrt(4)

# Getting help in R
?sqrt
help(sqrt)

# Logical
TRUE
FALSE
!TRUE
!FALSE
2 == 3
2 == 2
2 != 3
2 != 2
2 > 3 # also >= available
2 < 3 # also <= available

# What will R print?

!(2 == 2)
!(2 == 3)
!(2 > 3)
2 <= 3


# Vectors
# =======

# Create a vector with the c (short for combine) function
c(1, 3, 5)
c(10, 11, 12, 13, 14, 15)

# sequence from 1 to 10
seq(10, 15)
seq(10, 15, by = 1)

# from:to is a short for seq(from, to, by = 1)
10:15

# you can combine vectors
c(1:4, 10)

# in steps of 2
seq(1, 10, by = 2)

# five equally spaced numbers
seq(0, 10, length.out = 5)


# Assign variables / objects
# ==========================

# Variable = named cell of the computer's memory that can store different values

# Assign = store the result of a computation into a variable

# To assign the result of a computation to a variable, use the assignment 
# operator <-

# What will R print?
a <- 1:5
a
b <- seq(2, 10, by = 2)
b
c <- a*b
c


# Basic functions
# ===============

# What will R print?

a
sum(a)
min(a)
max(a)
range(a)
mean(a)
sd(a)
var(a)


# Subsetting / indexing
# =====================

# Specify the elements you want within square brackets []
# positive integer = keep
# negative integer = exclude
# TRUE = keep, FALSE = exclude

# Numeric subsetting

x <- seq(10, 100, by = 10)
x
x[4]
x[c(3, 5, 8)]
x[c(-2, -4)]
x[-c(2, 4)]
x
x <- x[-c(2, 4)]
x
x[c(3, 5)] <- c(-200, 900)
x

# Logical subsetting

x <- seq(10, 100, by = 10)
x
(x > 30)
x[(x > 30)]
!(x > 30)
x[!(x > 30)]

# What values will R return?
a <- 2 * (1:10)
a
a[(a == 10)]
a[!(a == 10)]
a[(a <= 10)]

# which values in left vector are found in the right vector
c(2, 5, 90) %in% a

# & = and, | = or
a[(a > 10) & (a < 16)]
a[(a < 10) | (a > 16)]
a[(a > 10) & !(a < 16)]


# Missing values
# ==============

# NA = not available
# Any computation involving a not available value will return NA

NA + 1
2 * NA
mean(c(4, 6, NA, 8))
sum(c(4, 6, NA, 8))

mean(c(4, 6, NA, 8), na.rm = TRUE)
sum(c(4, 6, NA, 8), na.rm = TRUE)

# or remove the values yourself

a <- c(4, 6, NA, 8, NA, 9:15)
a

a[!is.na(a)]
a

# to remove the NAs forever, overwrite a
a <- a[!is.na(a)]

a <- c(4, 6, NA, 8, NA, 9:15)
a <- na.omit(a)
a


# Dataframes / tibbles / data tables
# ==================================

library(tidyverse)

tbl_name <- tibble(
    col_name = 1:10,
    another_col_name = 5 * (1:10)
)
tbl_name

# Used to store values related to the same units. Each row is one unit
# and each column has the value of that column for that unit.

children <- tibble(
    child_id = 1:10,
    age = c(12, 10, 8, 11, 10, 13, 11, 16, 9, 7),
    height = c(120.56, 99.13, 78.11, 107.68, 100.74, 131.86, 108.02, 158.25, 
               90.25, 70.72)
)
children

# print top n rows, default n = 6
head(children)
head(children, n = 4)

# Extract a column as vector
children$age

mean(children$age)


# Take this data %>%
#   some computation

# %>% is called the pipe operator and says "then do"

children %>%
    pull(age)

children %>%
    pull(age) %>%
    mean()

# Dataframe[Rows, Columns] if empty, keep all

children[children$age > 10, ]

# Alternative
children %>%
    filter(age > 10)

# Summarise

children %>%
    select(age, height) %>%
    summarise(M_Age = mean(age), SD_Age = sd(age),
              M_Height = mean(height), SD_Height = sd(height))

# Mutate

children

children %>%
    mutate(height_in_m = height / 100)


# Reading data
# ============

# Imagine a study into income disparity for workers in a local authority. 
# We might carry out interviews and find that there is a link between the level of 
# education and an employee's income. Those with more formal education seem to be 
# better paid. Now we wouldn't have time to interview everyone who works for 
# the local authority so we would have to interview a sample, say 10%.
# We will use the riverview data (see below) to examine whether education level 
# is related to income among the employees working for the city of Riverview, 
# a hypothetical midwestern city in the US.

# The riverview data come from Lewis-Beck and Lewis-Beck (2015) and contain  
# data collected from a random sample of n = 32 employees working for the 
# city of Riverview, a hypothetical midwestern city in the US. 

# The attributes include:
#   + education: Years of formal education
#   + income: Annual income (in thousands of U.S. dollars)
#   + seniority: Years of seniority
#   + gender: Employee’s gender
#   + male: Dummy coded gender variable (0 = Female, 1 = Male)
#   + party: Political party affiliation

library(tidyverse)
riverview <- read_csv("SUBSTITUTE PATH TO RIVERVIEW DATA")
head(riverview)

ggplot(riverview)

ggplot(riverview, aes(x = education, y = income))

ggplot(riverview, aes(x = education, y = income)) +
    geom_point()

ggplot(riverview, aes(x = education, y = income)) +
    geom_point(alpha = 0.5)

ggplot(riverview, aes(x = education, y = income)) +
    geom_point(alpha = 0.5) +
    labs(x = "Education (in years)", 
         y = "Income (in thousands of U.S. dollars)")

ggplot(data = riverview, aes(x = education, y = income)) +
    geom_jitter(width = 0.1, height = 0, shape = 1) +
    labs(x = "Education (in years)", 
         y = "Income (in thousands of U.S. dollars)",
         title = "Income vs Education")


# What will R print?

ggplot(riverview, aes(x = party, y = education)) +
    geom_boxplot()

ggplot(riverview, aes(x = income)) +
    geom_density()

ggplot(riverview, aes(x = income, color = party)) +
    geom_density()

ggplot(riverview, aes(x = income)) +
    geom_density() +
    facet_grid(~ party)


#### GUIDED PRACTICE ####

# 1. Locate the path to the riverview data on the shared folder. 
# Read the data into R using that path.

library(tidyverse)
riverview <- read_csv("SUBSTITUTE PATH TO RIVERVIEW DATA")
head(riverview)

ggplot(riverview, aes(x = education, y = income)) +
    geom_point(alpha = 0.5) +
    labs(x = "Education (in years)", 
         y = "Income (in thousands of U.S. dollars)")

riverview %>%
    select(education, income) %>%
    cor()


# 2. Outcome variable = income, explanatory variable = education. 
# Fit a linear model that uses the explanatory variable to predict the outcome.

mdl <- lm(income ~ education, data = riverview)
summary(mdl)
coef(mdl)


# 3. Write down an interpretation of the model results. This typically includes a
# few sentences about the coefficients.

# We can interpret the estimated intercept as follows:
# The estimated average income associated to zero years of formal education is $11,321.

# For the estimated slope we might write:
# The estimated increase in average income associated to a one year increase in 
# education is $2,651.


# 4. Create a plot of the line. 
# Hint: geom_abline(intercept = ?, slope = ?)

betas <- coef(mdl)
betas

ggplot(riverview, aes(x = education, y = income)) +
    geom_point(alpha = 0.5) + 
    geom_abline(intercept = betas[1], slope = betas[2], color = 'blue') +
    labs(x = "Education (in years)", 
         y = "Income (in thousands of U.S. dollars)")

ggplot(riverview, aes(x = education, y = income)) +
    geom_point(alpha = 0.5) + 
    geom_smooth(method = "lm", se = FALSE)


#### PRACTICE ####

# 1. Locate the path to the DPUK data file on the shared folder. 
# Read the data into R using that path.

# 2. Choose a numeric variable to be the outcome, and another to be the 
# explanatory variable. Fit a linear model that uses the explanatory variable 
# to predict the outcome.

# 3. Write down an interpretation of the model results. This typically includes a
# few sentences about the coefficients.

# 4. Create a plot of the line. 
# Hint: geom_abline(intercept = ?, slope = ?)

