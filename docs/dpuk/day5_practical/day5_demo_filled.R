# DPUK Spring Academy 2025
# Day 5: Is my model any good?
# Edinburgh Stats Team


# 1) SETUP AND DATA


# Install any required packages with:
#   install.packages("car")
#   install.packages("performance")


# Load required libraries

library(tidyverse)     # For data manipulation and visualisation
library(car)           # For diagnostic functions
library(performance)   # For model diagnostics
library(psych)         # For descriptive statistics


# Set seed to ensure random data are reproducible
set.seed(42)


# --------
# Create simulated data similar to the lecture
n <- 100
salary_data <- tibble(
    id = paste0("ID", 101:(100+n)),
    serv = runif(n, 2, 6),
    perf = sample(1:7, n, replace = TRUE)
)

# Generate salary with a known relationship plus some noise
salary_data$salary <- 30 + 12 * salary_data$serv + 10 * salary_data$perf +
    rnorm(n, 0, 15)
salary_data$salary <- round(salary_data$salary, 2)
# -------


# 2) EXPLORING THE DATA


# Let's look at the first few rows
head(salary_data)

# Summary statistics
summary(salary_data)

# More detailed descriptive statistics
salary_data |>
    dplyr::select(salary, serv, perf) |> 
    describe()

# Scatterplot of salary vs years of service
ggplot(salary_data, aes(x = serv, y = salary)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Years of Service", y = "Salary (thousands £)",
       title = "Relationship between Salary and Years of Service")

# Scatterplot of salary vs performance rating
ggplot(salary_data, aes(x = perf, y = salary)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Performance Rating", y = "Salary (thousands £)",
       title = "Relationship between Salary and Performance Rating")

# Correlation matrix
salary_data |>
    dplyr::select(salary, serv, perf) |> 
    cor()

salary_data |>
    select(-id) |>
    pairs.panels()


# 3) FITTING LINEAR MODELS


# Let's start with a simple model predicting salary from years of service and performance
m1 <- lm(salary ~ serv + perf, data = salary_data)
summary(m1)


# 4) MODEL ASSUMPTIONS


# Remember the LINE mnemonic from the lecture:
# 1. L - Linearity
# 2. I - Independence
# 3. N - Normality
# 4. E - Equal variance (also called Homoscedasticity)


# 1. Linearity
# fitted values = yhat
# residual = y - yhat

plot(m1, which = 1)


# 2. Independence
# 
# Typically, this is checked by reading the study design rather than by doing specific statistical tests or plots


# 3. Normality of Residuals

# Histogram of residuals
hist(resid(m1), 
     main = "Histogram of Residuals", 
     xlab = "Residuals")

# QQ plot
qqnorm(resid(m1))
qqline(resid(m1))

# Using the built-in plot function
plot(m1, which = 2)


# 4. Equal Variance (Homoscedasticity)


# Plot residuals vs predicted values
plot(m1, which = 1)
plot(m1, which = 3)

# 1. residuals -> |residuals|
# 2. |residuals| -> sqrt(|residuals|)


# All diagnostic plots at once

par(mfrow = c(2, 2))
plot(m1)
par(mfrow = c(1, 1))

# All diagnostic plots at once, using the performance package
check_model(m1)


# 5) MULTI-COLLINEARITY


# Calculate Variance Inflation Factor (VIF)
vif(m1)
vif(m1, type = 'predictor')

# Interpretation:
#   VIF close to 1: No multicollinearity
#   VIF > 5: Moderate multicollinearity
#   VIF > 10: Severe multicollinearity

# VIF for interactions will be high - don't worry too much about that
# VIF for categorical variables will also be high - don't worry too much


# 6) MODEL DIAGNOSTICS: INFLUENTIAL CASES


# Cook's Distance
cooks_d <- cooks.distance(m1)

# Plot Cook's distance
plot(cooks_d, type = "h", 
     main = "Cook's Distance", 
     ylab = "Cook's distance", 
     xlab = "Observation")

# Find influential cases using the commonly used threshold 4/(n-k-1)
influential_threshold <- 4 / ( nrow(salary_data) - length(coef(m1)) )
influential_threshold

abline(h = influential_threshold, col = "red")

influential_cases <- which(cooks_d > influential_threshold)
influential_cases

# data[rows, ]
salary_data[influential_cases, ]


# Other Influence Measures

infl <- influence.measures(m1)
infl
summary(infl)

# DFBETAS: Effect on coefficients
dfbetas <- dfbetas(m1)
head(dfbetas)

# DFFITS: Effect on fitted values
dffits <- dffits(m1)
head(dffits)

# COVRATIO: Effect on precision of estimates
covratio <- covratio(m1)
head(covratio)


# 7) WHAT IF WE VIOLATE ASSUMPTIONS?


# Non-linear relationship


# --------
# Let's create a new variable that has a non-linear relationship with salary
salary_data$serv_squared <- salary_data$serv^2
salary_data$new_salary <- 30 + 5 * salary_data$serv + 10 * salary_data$serv_squared + 
  5 * salary_data$perf + rnorm(n, 0, 15)
# --------


# Scatterplot
ggplot(salary_data, aes(x = serv, y = new_salary)) +
    geom_point()

ggplot(salary_data, aes(x = perf, y = new_salary)) +
    geom_point()

# Fit a linear model ignoring the non-linearity
m2 <- lm(new_salary ~ serv + perf, data = salary_data)
summary(m2)

# Check assumptions
par(mfrow = c(2, 2))
plot(m2)
par(mfrow = c(1, 1))

# Fix the model by adding the squared term
m3 <- lm(new_salary ~ serv + I(serv^2) + perf, data = salary_data)
summary(m3)

# Check if assumptions are now met
par(mfrow = c(2, 2))
plot(m3)
par(mfrow = c(1, 1))

# Compare the two models
anova(m2, m3)


# Box-Cox transformation


# -------
# Let's create a new dataset with a non-normal outcome variable
n_new <- 100
data_bc <- data.frame(
    x1 = runif(n_new, 0, 10),
    x2 = runif(n_new, 0, 10)
)

data_bc$y <- 5 + 2 * data_bc$x1 + 3 * data_bc$x2 * rchisq(n_new, 6)
# -------


# Top six rows of the dataset
head(data_bc)

# Now fit a model
prob_model <- lm(y ~ x1 + x2, data = data_bc)
summary(prob_model)

# Check assumptions
par(mfrow = c(2, 2))
plot(prob_model)
par(mfrow = c(1, 1))

check_model(prob_model)

ggplot(data_bc, aes(y)) +
    geom_histogram()


# install.packages("forecast")

# Box-Cox transform of outcome
library(forecast)
best_lambda <- BoxCox.lambda(data_bc$y, method = 'loglik')
data_bc$ynew <- BoxCox(data_bc$y, lambda = best_lambda)

ok_model <- lm(ynew ~ x1 + x2, data = data_bc)
summary(ok_model)

par(mfrow = c(2, 2))
plot(ok_model)
par(mfrow = c(1, 1))

vif(ok_model)



# With your own model

par(mfrow = c(2, 2))
plot(m1)
par(mfrow = c(1, 1))

check_model(m1)

infl <- influence.measures(m1)
infl
summary(infl)
