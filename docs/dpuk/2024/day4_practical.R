# DPUK - 4 - Linear models training
# Author: Josiah King, Umberto Noe, Tom Booth


# Library imports
# ===============
library(tidyverse)
library(sjPlot)
library(psych)


# Data
# ===============

# Sample of 100 R users from Edinburgh & Glasgow, measured age, height, 
# average daily R usage (minutes) and rating of how much 
# they like statistics (0-100)

df <- read_csv("day4_practical/sillydata.csv")

# This data was generated with "liking statistics" being predicted only 
# by average R usage, being based in Edinburgh, and their interaction.  
# The model which *should* fit the way the data has been generated:
# lm(score ~ location * avg_Rusage, data = df)
mod <- lm(statpref ~ location * avg_Rusage, data = df)
par(mfrow=c(2,2))
plot(mod)
par(mfrow=c(1,1))


# we are making models of the world. The residuals are the "leftovers"
# if we leave something important out of our model, the residuals take this up. 
# in the ideal scenario, our residuals are just random noise.
# keep an eye for systematic patterns in the residuals


# miss out an interaction:
mod_noint <- lm(statpref ~ location + avg_Rusage, data = df)
# see residuals vs fitted is curved!
plot(mod_noint, which = 1)

# miss out an important predictor
mod_noloc <- lm(statpref ~ avg_Rusage, data = df)
# note you can almost *see* the two groups here, 
# because we made this such a strong predictor
plot(mod_noloc, which=2)



# "influencers"
mod <- lm(statpref ~ location * avg_Rusage, data = df)
plot(mod)

ggplot(df,aes(x=avg_Rusage,y=statpref,col=location))+
    geom_point()

# a few get highlighted
influence.measures(mod)
plot(mod, which=4)
df[c(34,82,86),]

# Why is Umberto not an influencer? 
ggplot(df,aes(x=avg_Rusage,y=statpref,col=location))+
    geom_point()+
    geom_smooth(method=lm)+
    geom_label(aes(label = name))


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

