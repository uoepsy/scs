# DPUK - 2 - Linear models training
# Author: Umberto Noe, Josiah King, Tom Booth


# Library imports
# ===============

library(tidyverse)
library(psych)


# Stroke data
# ==============

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

stroke <- read_csv("stroke.csv")
head(stroke)

stroke %>%
    select(hosp) %>%
    table()

stroke$hosp <- 
    factor(stroke$hosp, 
           levels = c("HospitalA","HospitalB","HospitalC"))

stroke %>%
    select(side) %>%
    table()

stroke$side <- 
    factor(stroke$side, 
           levels = c("Left","Right"))

stroke %>%
    select(Leftside) %>%
    table()

head(stroke)

# Summary statistics, but omit factors
describe(stroke, omit = TRUE)

# Summary statistics, but omit factors
stroke %>%
    select(-Leftside) %>%
    describe(omit = TRUE)

# Pairwise scatterplots
stroke %>%
    select(physio, TUG, NIHSS) %>%
    pairs()

stroke %>%
  select(physio, TUG, NIHSS) %>%
    pairs.panels()

# Remove clutter
stroke %>%
  select(physio, TUG, NIHSS) %>%
    pairs.panels(ellipses = FALSE, smooth = FALSE)

# Correlations
stroke %>%
  select(physio, TUG, NIHSS) %>%
    cor()


# *** Optional - extra material ***
# Alternatively
# =================================

# Find the numeric variables
names(stroke)
num <- c("physio", "TUG", "NIHSS")
describe(stroke[, num])

# Pairwise scatterplots and correlations
pairs(stroke[, num])
pairs.panels(stroke[, num])

# Correlations
cor(stroke[, num])
# ***


# Stroke modelling
# ===================

# Model 0: intercept-only model

mdl0 <- lm(TUG ~ 1, data = stroke)
summary(mdl0)

ggplot(stroke, aes(x = physio, y = TUG)) +
    geom_point(alpha = 0.5) +
    geom_abline(intercept = 53.742, slope = 0, color = 'blue') +
    labs(x = "Physiotherapy (Hours)", 
         y = "Timed Up & Go Test (seconds)")

stroke <- stroke %>%
    mutate(income_hat_mdl0 = predict(mdl0, newdata = stroke))
stroke


# Model 1: TUG ~ physio

mdl1 <- lm(TUG ~ physio, data = stroke)
summary(mdl1)

ggplot(stroke, aes(x = physio, y = TUG)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 74.9525, slope = -2.6513, color = 'blue') +
  labs(x = "Physiotherapy (Hours)", 
       y = "Timed Up & Go Test (seconds)")

stroke <- stroke %>%
  mutate(income_hat_mdl1 = predict(mdl1, newdata = stroke))
stroke

# Model 2: TUG ~ NIHSS + physio
# =======================================

mdl2 <- lm(TUG ~ NIHSS + physio, data = stroke)
summary(mdl2)

stroke <- stroke %>%
  mutate(income_hat_mdl2 = predict(mdl2, newdata = stroke))
stroke



# Model comparison
# ================

anova(mdl0, mdl1, mdl2)


# *** Optional - extra material ***
# Residuals and residual sum of squares
# =====================================

stroke <- stroke %>%
    mutate(resid0 = TUG - income_hat_mdl0,
           resid1 = TUG - income_hat_mdl1,
           resid2 = TUG - income_hat_mdl2)
stroke

rss <- stroke %>%
    summarise(RSS0 = sum(resid0^2),
              RSS1 = sum(resid1^2),
              RSS2 = sum(resid2^2))
rss

# In R, you get the residual sum of squares with the deviance() function
deviance(mdl0)
deviance(mdl1)
deviance(mdl2)
# ***


# With categorical predictor
# =======================================

# Suppose we didn't have the exact "hours of physio therapy" recorded, but just a note from
# the consultant about whether patients were meeting the recommended 45 mins a day (or 10.5 hours over 2 weeks).
stroke$min_physio <- ifelse(stroke$physio > 10.5, "Y", "N") 

# 1. Mean-centre NIHSS for better interpretability.

# 2. Fit a model that uses mean-centred NIHSS and completing recommended physio as predictors of income.

# 3. Is there a significant difference in physical functioning (TUG) between those who did and did not meet the recommended physio hours, after accounting for stroke severity?

# 4. Is the difference still significant if you also control for the side of the brain affecred?

# 5. Discussion


# DPUK data
# =====================

# 1. Choose:
#   (a) an outcome variable
#   (b) two numeric explanatory variables
#   (c) one categorical variable

# 2. Convert the categorical variable to a factor and tidy up the levels of the 
# factor.

# 3. Summarise the variables chosen in step (1) either visually or via descriptive 
# statistics.


# Discussion and small break


# 4. Do any of the numeric predictors from Practice 1 need mean-centering or 
# standardisation to help with interpretation?

# 5. Fit a linear model that predicts your outcome variable using one of the numeric
# predictors and the categorical one.

#   - Interpret the model coefficients, t-statistic, and p-values.

#   - How good is the model fit?

# 6. Is there a significant difference across the categories after accounting 
# for the numerical predictor?

# 7. Is the difference significant when also controlling for the other numeric predictor?

# 8. Does the inclusion of both covariates lead to a significant reduction in 
# residual sum of squares?


# Discussion

