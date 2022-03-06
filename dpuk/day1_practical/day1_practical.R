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

1 + 3 * 10
(1 + 3) * 10

2^3
9^(1/2)
9^0.5

# Getting help in R
sqrt(4)
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


# Vectors
# =======

# Create a vector with the c (short for combine) function
c(1, 3, 5)
c(1, 2, 3, 4, 5, 6)
# sequence from 1 to 10
seq(1, 10)
seq(1, 10, by = 1)
# from:to is a short for seq(from, to, by = 1)
1:6
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

# to remove NA forever, overwrite a
a <- a[!is.na(a)]

a <- c(4, 6, NA, 8, NA, 9:15)
a <- na.omit(a)
a


# Dataframes / tibbles / data tables
# ==========

# Different measurement vectors related to same individuals

library(tidyverse)

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
