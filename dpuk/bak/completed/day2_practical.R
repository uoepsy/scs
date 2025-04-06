# DPUK - Linear models training - Wednesday
# Author: Umberto Noe, Josiah King, Tom Booth


# Library imports
# ===============

library(tidyverse)
library(psych)


# Riverview data
# ==============

riverview <- read_csv("S:\\Spring_Academy_2022 - Spring Academy 2022\\LinearModels\\riverview.csv")
head(riverview)

riverview %>%
    select(party) %>%
    table()

riverview$party <- 
    factor(riverview$party, 
           levels = c("Independent", "Republican", "Democrat"))

riverview %>%
    select(gender) %>%
    table()

riverview$gender <- 
    factor(riverview$gender, 
           levels = c("female", "male"))

riverview %>%
    select(male) %>%
    table()

head(riverview)

# Summary statistics, but omit factors
describe(riverview, omit = TRUE)

# Summary statistics, but omit factors
riverview %>%
    select(-male) %>%
    describe(omit = TRUE)

# Pairwise scatterplots
riverview %>%
    select(education, income, seniority) %>%
    pairs()

riverview %>%
    select(education, income, seniority) %>%
    pairs.panels()

# Remove clutter
riverview %>%
    select(education, income, seniority) %>%
    pairs.panels(ellipses = FALSE, smooth = FALSE)

# Correlations
riverview %>%
    select(education, income, seniority) %>%
    cor()


# *** Optional - extra material ***
# Alternatively
# =================================

# Find the numeric variables
names(riverview)
num <- c("education", "income", "seniority")
describe(riverview[, num])

# Pairwise scatterplots and correlations
pairs(riverview[, num])
pairs.panels(riverview[, num])

# Correlations
cor(riverview[, num])
# ***


x <- c(0, 2, 1, 1, 2, 0)
fx <- factor(x, 
             levels = c(0, 1, 2), 
             labels = c("Unlikely", "Likely", "Very Likely"))
x
fx

# Practice 1: DPUK data
# =====================

# 1. Choose:
#   (a) an outcome variable
#   (b) two numeric explanatory variables
#   (c) one categorical variable

# 2. Convert the categorical variable to a factor and tidy up the levels of the 
# factor.

# 3. Summarise the variables chosen in step (1) either visually or via descriptive 
# statistics.


# Discussion
# ==========

# During the practical

# 1. Alcohol SYN12_ALCGLASSWK, Mental health SYN10_CESDTOT
# Depression SYN10_CESDDEPRESS
# Cognition - Verbal SYN11_VFLUTOT

dpuk <- read_csv("S:\\Spring_Academy_2022 - Spring Academy 2022\\synth1.0 datasets - SUB\\DPUK_synth1.2_sub.csv")
head(dpuk)
names(dpuk) <- str_replace(names(dpuk), "_0_0", "")
names(dpuk)

dpuk$SYN10_CESDDEPRESS <- factor(dpuk$SYN10_CESDDEPRESS,
                                 levels = c(0, 1),
                                 labels = c("No", "Yes"))
dpuk$SYN10_CESDDEPRESS


dpuk %>%
  select(SYN10_CESDDEPRESS) %>%
  table()

dpuk %>%
  select(SYN12_ALCGLASSWK, SYN10_CESDTOT, SYN11_VFLUTOT) %>%
  pairs.panels()

dpuk %>%
  select(SYN12_ALCGLASSWK, SYN10_CESDTOT, SYN11_VFLUTOT) %>%
  describe()

dpuk %>%
  select(SYN12_ALCGLASSWK, SYN10_CESDTOT, SYN11_VFLUTOT) %>%
  cor(use = "pairwise.complete.obs")


# Riverview modelling
# ===================

# Model 0: intercept-only model

mdl0 <- lm(income ~ 1, data = riverview)
summary(mdl0)

ggplot(riverview, aes(x = education, y = income)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 53.742, slope = 0, color = 'blue') +
    xlim(0, 30) +
    labs(x = "Education (in years)", 
         y = "Income (in thousands of U.S. dollars)")

riverview <- riverview %>%
    mutate(income_hat_mdl0 = predict(mdl0, newdata = riverview))
riverview


# Model 1: income ~ education

mdl1 <- lm(income ~ 1 + education, data = riverview)
summary(mdl1)

ggplot(riverview, aes(x = education, y = income)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 11.3214, slope = 2.6513, color = 'blue') +
    xlim(0, 30) +
    ylim(0, 90) +
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


# *** Optional - extra material ***
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
# ***


# Practice 2: Riverview with categorical predictor
# ================================================

# 1. Mean-centre education for better interpretability.

# riverview <- riverview %>%
#   mutate(education_mc = education - mean(education))

riverview <- riverview %>%
  mutate(education_mc = scale(education, center = TRUE, scale = FALSE))

# 2. Fit a model that uses mean-centred education and gender as predictors of income.

fit2a <- lm(income ~ education_mc + gender, data = riverview)
summary(fit2a)

# 3. Is there a significant difference in pay between males and females after accounting for 
# education?

# 4. Is the difference still significant if you also control for years of seniority?

fit2b <- lm(income ~ education_mc + seniority + gender, data = riverview)
summary(fit2b)

fit2c <- lm(income ~ education_mc + 
              scale(seniority, center = TRUE, scale=FALSE) + 
              gender, data = riverview)
summary(fit2c)


riverview %>%
  mutate(seniority_mc = seniority - mean(seniority))

fit2d <- lm(income ~ education_mc + seniority_mc + gender, data = riverview)
summary(fit2d)


# Discussion
# ==========

# During the practical


# Practice 3: DPUK data
# =====================

# 1. Do any of the numeric predictors from Practice 1 need mean-centering or 
# standardisation to help with interpretation?

# 2. Fit a linear model that predicts your outcome variable using one of the numeric
# predictors and the categorical one.

#   + Interpret the model coefficients, t-statistic, and p-values.

#   + How good is the model fit?

# 3. Is there a significant difference across the categories after accounting 
# for the numerical predictor?

# 4. Is the difference significant when also controlling for the other numeric predictor?

# 5. Does the inclusion of both covariates lead to a significant reduction in 
# residual sum of squares?


# Discussion
# ==========

# During the practical
