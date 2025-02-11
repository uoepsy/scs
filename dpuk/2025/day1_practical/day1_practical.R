# DPUK - 1 - Linear models training
# Author: Umberto Noe, Josiah King, Tom Booth


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

# Numeric indexing:
#   Specify the elements you want within square brackets []
#   positive integer = keep
#   negative integer = exclude

# Logical indexing:
#   TRUE = keep, FALSE = exclude
#   Must provide as many TRUE/FALSE(s) as the length of the variable.

# Numeric indexing

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

# Logical indexing

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

# install.packages("tidyverse")
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

# Imagine a study into the recovery of stroke patients. 
# We have 32 patients who suffered strokes. The severity of the stroke was recorded, along with whether the left or right side of the brain was affected. 
# 2 weeks following the stroke, patients recovery was measured with the "Timed Up & Go" test - a measure of physical functioning. We also have data on the number of hours spent in physiotherapy over the 2 week period. 

# The attributes include:
#   physio: Hours spent in physiotherapy over 2 weeks
#   TUG: Timed Up & Go test (lower scores indicate better physical functioning)
#   NIHSS: NIH Stroke Severity Scale (higher values indicate more severe stroke)
#   side: Side of the brain affected
#   Leftside: Dummy coded variable (0 = Right-side, 1 = Left-side)
#   hosp: Hospital patient was admitted at

library(tidyverse)
stroke <- read_csv("stroke.csv")
head(stroke)

ggplot(stroke)

ggplot(stroke, aes(x = physio, y = TUG))

ggplot(stroke, aes(x = physio, y = TUG)) +
    geom_point()

ggplot(stroke, aes(x = physio, y = TUG)) +
    geom_point(alpha = 0.5)

ggplot(stroke, aes(x = physio, y = TUG)) +
    geom_point(alpha = 0.5) +
    labs(x = "Physiotherapy (Hours)", 
       y = "Timed Up & Go Test (seconds)")

ggplot(stroke, aes(x = physio, y = TUG)) +
    geom_jitter(width = 0.1, height = 0, shape = 1) +
    labs(x = "Physiotherapy (Hours)", 
       y = "Timed Up & Go Test (seconds)",
       title = "Physical Functioning vs Physiotherapy")


# What will R print?

ggplot(stroke, aes(x = side, y = NIHSS)) +
    geom_boxplot()

ggplot(stroke, aes(x = TUG)) +
    geom_density()

ggplot(stroke, aes(x = TUG, color = side)) +
    geom_density()

ggplot(stroke, aes(x = TUG)) +
    geom_density() +
    facet_grid(~ side)


# Guided practice
# ===============

# 1. Locate the path to the stroke data on the shared folder. 
# Read the data into R using that path and perform some exploratory data analysis.

# 2. Outcome variable = TUG, explanatory variable = physio. 
# Fit a linear model that uses the explanatory variable to predict the outcome.

# 3. Write down an interpretation of the model results. This typically includes a
# few sentences about the coefficients.

# 4. Create a plot of the fitted line. 
#    Hint: geom_abline(intercept = ?, slope = ?)





# DPUK data
# ================

# 1. Locate the path to the DPUK data file on the shared folder. 
# Read the data into R using that path.

# 2. Choose a numeric variable to be the outcome, and another to be the 
# explanatory variable. Fit a linear model that uses the explanatory variable 
# to predict the outcome.

# 3. Write down an interpretation of the model results. This typically includes a
# few sentences about the coefficients.

# 4. Create a plot of the line. 
# Hint: geom_abline(intercept = ?, slope = ?)

