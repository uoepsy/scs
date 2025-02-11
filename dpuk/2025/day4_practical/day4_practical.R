# DPUK - 4 - Linear models training
# Author: Josiah King, Umberto Noe, Tom Booth


# Library imports
# ===============
library(tidyverse)
library(sjPlot)
library(psych)
library(car)


# Data
# ===============

# Sample of 100 R users from Edinburgh & Glasgow, measured age, height, 
# average daily R usage (minutes) and rating of how much 
# they like statistics (0-100)

df <- read_csv("sillydata.csv")

df <- df %>%
    mutate(row = 1:nrow(df)) %>%
    relocate(row, .before = name)

ggplot(df, aes(x = avg_Rusage, y = statpref, color = location)) +
    geom_point()


# Model
# ==========

# This data was generated with "liking statistics" being predicted only 
# by average R usage, being based in Edinburgh, and their interaction.  
# The model which *should* fit the way the data has been generated:
# lm(statpref ~ location * avg_Rusage, data = df)

# A * B
# A + B + A:B

mod <- lm(statpref ~ location * avg_Rusage, data = df)

summary(mod)

# Assumptions, a helpful mnemonic:
# 
#   L - Linearity
#   I - Independence
#   N - Normality
#   E - Equal variance (Homoscedasticity)

par(mfrow=c(2,2))
plot(mod)
par(mfrow=c(1,1))

hist(resid(mod))

# One by one
plot(mod, which = 1)
plot(mod, which = 2)
plot(mod, which = 3)
plot(mod, which = 4)
plot(mod, which = 5)

# Extra =======
crPlots(mod) # Only works for models without an interaction!
crPlots(lm(statpref ~ location + avg_Rusage, data = df))

residualPlot(mod)

library(performance)
check_model(mod)
# ====== Extra


# Pause


# VIF
# ==========

# VIF for a given predictor is computed by performing a linear regression of 
# that predictor on the remaining predictors. Then, the R^2 from that model 
# is obtained, and VIF = 1 / (1 - R^2)

vif(mod)

df$avg_RusageC <- df$avg_Rusage - mean(df$avg_Rusage)
mod_c <- lm(statpref ~ location * avg_RusageC, data = df)

vif(mod_c)

vif(mod, type = 'predictor')
vif(mod_c, type = 'predictor')


# What if we left out important terms?
# ===============

# We are making models of the world. The residuals are the "leftovers"
# if we leave something important out of our model, the residuals take this up. 
# in the ideal scenario, our residuals are just random noise.
# keep an eye for systematic patterns in the residuals

# miss out an interaction:
mod_noint <- lm(statpref ~ location + avg_Rusage, data = df)

par(mfrow=c(2,2))
plot(mod_noint)
par(mfrow=c(1,1))

# see residuals vs fitted is curved!
plot(mod_noint, which = 1)


# miss out an important predictor
mod_noloc <- lm(statpref ~ avg_Rusage, data = df)

par(mfrow=c(2,2))
plot(mod_noloc)
par(mfrow=c(1,1))

# note you can almost *see* the two groups here, 
# because we made this such a strong predictor
plot(mod_noloc, which=2)


# Pause


# "influencers"
# ===============

# Slides

mod <- lm(statpref ~ location * avg_Rusage, data = df)
par(mfrow=c(2,2))
plot(mod)
par(mfrow=c(1,1))

ggplot(df, aes(x=avg_Rusage, y=statpref, color=location))+
    geom_point() +
    geom_label(aes(label = row))

# A few get highlighted
influence.measures(mod)
summary(influence.measures(mod))

# Shows the DFBETAs for each model variable, DFFITs, covariance ratios, 
# Cook's distances and hat values (the diagonal elements of the hat matrix)

# 4 / (n - k - 1)
4 / (100 - 3 - 1)

df %>%
    mutate(CooksD = cooks.distance(mod)) %>%
    filter(CooksD > 4 / (100 - 3 - 1))

df_smaller <- df[c(-34, -82), ]

# Why is Umberto not an influencer? 
ggplot(df, aes(x=avg_Rusage, y=statpref, color=location))+
    geom_point()+
    geom_smooth(method=lm)+
    geom_label(aes(label = row))


# =====================
# Open Discussion on content from all days. 
# =====================


# =====================
# DPUK data
# =====================

# 1. Choose an outcome variable

# 2. Think about your theory about what variables are likely to inform scores on your outcome variable. 

# 3. Fit a model that represents 2. 

# 4. Create diagnostic plots. 

# 5. Discussion

