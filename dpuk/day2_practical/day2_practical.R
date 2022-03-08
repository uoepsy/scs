# DPUK - Linear models training - Wednesday
# Author: Umberto Noe, Tom Booth

# Library imports
# ===============

library(tidyverse)
library(psych)


# Riverview data
# ==============

riverview <- read_csv("riverview.csv")
head(riverview)
summary(riverview)

riverview %>%
    select(party) %>%
    table()

riverview$party <- 
    factor(riverview$party, 
           levels = c("Independent", "Republican", "Democrat"))

riverview %>%
    select(male) %>%
    table()

riverview$male <- 
    factor(riverview$male, 
           levels = c(0, 1),
           labels = c("NotMale", "Male"))

riverview %>%
    select(gender) %>%
    table()

riverview$gender <- 
    factor(riverview$gender, 
           levels = c("female", "male"))

head(riverview)

# Summary statistics, but omit factors
describe(riverview, omit = TRUE)

# Pairwise scatterplots
riverview %>%
    select(education, income, seniority) %>%
    pairs()

riverview %>%
    select(education, income, seniority) %>%
    pairs.panels()

# Correlations
riverview %>%
    select(education, income, seniority) %>%
    cor()

# Alternatively
# =============

# Find the numeric variables
names(riverview)
num <- c("education", "income", "seniority")
describe(riverview[, num])

# Pairwise scatterplots and correlations
pairs(riverview[, num])
pairs.panels(riverview[, num])

# Correlations
cor(riverview[, num])


# Practice 1: DPUK data
# =====================

# 1. Choose:
#   (a) an outcome variable
#   (b) one or two numeric explanatory variables
#   (c) one categorical variable

# 2. Convert the categorical variable to a factor and tidy up the levels of the 
# factor.


# Riverview modelling
# ===================

# Model 0: intercept-only model

mdl0 <- lm(income ~ 1, data = riverview)
summary(mdl0)

ggplot(riverview, aes(x = education, y = income)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 53.742, slope = 0, color = 'blue') +
    labs(x = "Education (in years)", 
         y = "Income (in thousands of U.S. dollars)")

riverview <- riverview %>%
    mutate(income_hat_mdl0 = predict(mdl0, newdata = riverview))
riverview


# Model 1: income ~ education

mdl1 <- lm(income ~ education, data = riverview)
summary(mdl1)

ggplot(riverview, aes(x = education, y = income)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 11.3214, slope = 2.6513, color = 'blue') +
    labs(x = "Education (in years)", 
         y = "Income (in thousands of U.S. dollars)")

riverview <- riverview %>%
    mutate(income_hat_mdl1 = predict(mdl1, newdata = riverview))
riverview


# Model 2: income ~ education + seniority
# =======================================

mdl2 <- lm(income ~ education + seniority, data = riverview)
summary(mdl2)

riverview <- riverview %>%
    mutate(income_hat_mdl2 = predict(mdl2, newdata = riverview))
riverview


# Model comparison
# ================

anova(mdl0, mdl1, mdl2)


# Residuals and residual sum of squares
# =====================================

riverview <- riverview %>%
    mutate(resid0 = income - income_hat_mdl0,
           resid1 = income - income_hat_mdl1,
           resid2 = income - income_hat_mdl2)
riverview

rss <- riverview %>%
    summarise(RSS0 = sum(resid0^2),
              RSS1 = sum(resid1^2),
              RSS2 = sum(resid2^2))
rss

# In R, you get the residual sum of squares with the deviance() function
deviance(mdl0)
deviance(mdl1)
deviance(mdl2)


# Practice 2: Riverview with categorical predictor
# ==============================================

# 1. Mean-centre education for better interpretability.


# 2. Fit a model that uses mean-centred education and party as predictors of income


# Consider the following output

predict(fit1, newdata = tibble(education_mc = 0, party = 'Independent'))
predict(fit1, newdata = tibble(education_mc = 0, party = 'Republican'))
predict(fit1, newdata = tibble(education_mc = 0, party = 'Democrat'))

# 3. What do the estimated betas from (2) represent? 


# Practice 3: DPUK data
# =====================

# 1. Using the outcome variable and the predictors from Practice 1, fit a linear
# model that predicts the outcome using those explanatory variables.

# 2. Interpret the model coefficients, t-statistic, and p-values.

# 3. How good is the model fit?

# 4. Drop one or two predictors from the model, and test whether the addition of 
# those predictors leads to a significant reduction in residual sum of squares.
