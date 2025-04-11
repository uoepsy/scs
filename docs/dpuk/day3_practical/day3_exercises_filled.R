# DPUK Spring Academy 2025
# Day 3: Multiple Regression Analysis
# Edinburgh Stats Team


# RESEARCH CONTEXT AND QUESTIONS


# You are part of a multidisciplinary research team at a regional stroke 
# rehabilitation centre that collaborates with three hospitals (A, B, and C).
# You're analysing data from a pilot study of 32 stroke patients to determine 
# which factors most influence physical recovery outcomes.
#
# The primary outcome measure is the Timed Up & Go (TUG) test, which assesses
# functional mobility. Lower TUG scores indicate better physical functioning.
#
# Research Questions:
# 1. How much variance in recovery outcomes can be explained by physiotherapy 
#    duration and stroke severity?
# 2. Does the side of the brain affected (left vs. right) influence recovery 
#    trajectory after controlling for other factors?
# 3. Are there differences in patient outcomes between the three collaborating hospitals?


# 1) SETUP AND EXPLORATORY DATA ANALYSIS

# install.packages("psych")

# Load required libraries
# tidyverse: collection of packages for data manipulation and visualisation
# psych: useful for descriptive statistics
library(tidyverse)
library(psych)

# Load the stroke dataset
stroke <- read_csv("https://uoepsy.github.io/scs/dpuk/day3_practical/stroke.csv")

# Variable descriptions:
# - physio: Hours of physiotherapy received over the course of 2 weeks
# - TUG: Timed Up & Go test score (seconds) - our outcome variable
# - NIHSS: NIH Stroke Scale score (higher = more severe stroke)
# - side: Side of brain affected by stroke ("Left" or "Right")
# - Leftside: Binary indicator for left side (1 = Left, 0 = Right)
# - hosp: Hospital where treatment occurred (A, B, or C)

# Inspect the data
head(stroke, n = 10)  # View the first 6 rows of the dataset
str(stroke)           # Check the variable types
glimpse(stroke)
summary(stroke)

# Convert categorical variables to factors with appropriate levels
# This ensures correct reference categories and interpretation
stroke$side <- factor(stroke$side, levels = c("Right", "Left"))
stroke$hosp <- factor(stroke$hosp)

glimpse(stroke)

levels(stroke$side)
levels(stroke$hosp)

# Check distribution of categorical variables
table(stroke$side)
table(stroke$hosp)

stroke |>
    count(side) |>
    mutate(perc = 100 * n / sum(n))

stroke |>
    count(hosp) |>
    mutate(perc = 100 * n / sum(n))

# Summary statistics for key numeric variables
# The function describe() from the psych package provides mean, SD, range, etc. 
# to understand distributions
stroke |>
  select(physio, TUG, NIHSS) |>
  describe()


# Data Visualisation: Understanding distributions and relationships


# Visualising distributions of individual variables

# Distribution of TUG scores (our outcome variable)
p1 <- ggplot(stroke, aes(x = TUG)) +
  geom_histogram() +
  labs(title = "Distribution of TUG Scores",
       x = "Timed Up & Go Test (seconds)",
       y = "Frequency")

# Distribution of physiotherapy hours (predictor variable)
p2 <- ggplot(stroke, aes(x = physio)) +
    geom_histogram() +
    labs(title = "Distribution of \n Physiotherapy Hours",
         x = "Physiotherapy Hours",
         y = "Frequency")

# Distribution of NIHSS scores (stroke severity, predictor variable)
p3 <- ggplot(stroke, aes(x = NIHSS)) +
    geom_histogram() +
    labs(title = "Distribution of Stroke Severity Scores",
         x = "NIH Stroke Severity Scale",
         y = "Frequency")

library(patchwork)
(p1 | p2) / p3

# Visualising relationships between variables


# Research Question 1: Relationship between physiotherapy and TUG
ggplot(stroke, aes(x = physio, y = TUG)) +
    geom_point() +
    labs(x = "Physiotherapy Hours",
         y = "TUG Score (seconds)")

# Research Question 1: Relationship between stroke severity and TUG
ggplot(stroke, aes(x = NIHSS, y = TUG)) +
    geom_point() +
    labs(x = "NIH Stroke Severity Scale",
         y = "TUG Score (seconds)")

# Research Question 2: TUG scores by side of brain affected
ggplot(stroke, aes(x = side, y = TUG, fill = side)) +
  geom_boxplot() +
  labs(x = "Side of Brain",
       y = "TUG Score (seconds)")

# Research Question 3: TUG scores by hospital
ggplot(stroke, aes(x = hosp, y = TUG, fill = hosp)) +
    geom_boxplot() +
    geom_jitter(width = 0.1) +
    labs(x = "Hospital",
         y = "TUG Score (seconds)")


# 2) DATA PREPARATION


# Mean-center NIHSS for better interpretability of coefficients
# This transforms the intercept to represent the predicted TUG for a patient
# with average stroke severity, rather than "zero severity"
stroke <- stroke |>
  mutate(NIHSS_mc = NIHSS - mean(NIHSS))

# Verify the centering worked correctly
mean(stroke$NIHSS)    # Original mean
mean(stroke$NIHSS_mc) # Should be very close to zero


# stroke$NIHSS_mc <- stroke$NIHSS - mean(stroke$NIHSS)


# Mean-centered variable
#   X_mc = X - mean(X)

# Z-scored variable
#   X_z = X_mc / sd(X)


# 3) MODEL BUILDING


# Model 0: Intercept-only model (baseline)
# This represents the null hypothesis that none of our predictors matter
mdl0 <- lm(TUG ~ 1, data = stroke)
summary(mdl0)

# RESEARCH QUESTION 1
# How much variance in recovery outcomes can be explained by 
# physiotherapy duration and stroke severity?

# Model 1: Simple linear regression with physiotherapy hours only
mdl1 <- lm(TUG ~ physio, data = stroke)
summary(mdl1)

# Model 2: Multiple regression with physiotherapy and mean-centred stroke severity
mdl2 <- lm(TUG ~ physio + NIHSS_mc, data = stroke)
summary(mdl2)


# RESEARCH QUESTION 2
# Does the side of the brain affected influence recovery 
# after controlling for other factors?

# Model 3: Add side of brain affected
mdl3 <- lm(TUG ~ physio + NIHSS_mc + side, data = stroke)
summary(mdl3)


# RESEARCH QUESTION 3
# Are there differences in patient outcomes between hospitals?

# Model 4: Full model with hospital variable added
mdl4 <- lm(TUG ~ physio + NIHSS_mc + side + hosp, data = stroke)
summary(mdl4)


# 4) MODEL COMPARISON


# RESEARCH QUESTION 1
# Compare variance explained by different factors using ANOVA tests

# Model comparisons for Research Question 1
# Test if adding physiotherapy significantly improves over intercept-only model
anova(mdl0, mdl1)


# Test if adding stroke severity significantly improves over physiotherapy-only model
anova(mdl1, mdl2)


# RESEARCH QUESTION 2
# Test if adding side of brain significantly improves the model
anova(mdl2, mdl3)


# RESEARCH QUESTION 3
# Test if adding hospital significantly improves the model
anova(mdl3, mdl4)


# Hint: extracting adjusted R-squared values for a model
summary(mdl0)$adj.r.squared


# Compare adjusted R-squared values across all models
# This shows the proportion of variance explained by each successive model
r2_values <- tibble(
  Model = c("Intercept only", "Physio only", "Physio + NIHSS_mc", 
            "Physio + NIHSS_mc + Side", "Full model"),
  Adj_R_Squared = c(summary(mdl0)$adj.r.squared,
                    summary(mdl1)$adj.r.squared,
                    summary(mdl2)$adj.r.squared,
                    summary(mdl3)$adj.r.squared,
                    summary(mdl4)$adj.r.squared)
)
r2_values

# Compare all models using ANOVA for a comprehensive view
anova(mdl0, mdl1, mdl2, mdl3, mdl4)

best_model <- mdl3


# 5) INTERPRETATION OF BEST MODEL


# Get detailed summary of the best model
summary(best_model)

# Coefficient interpretations:


# Calculate predicted values and residuals for model diagnostics
stroke <- stroke |>
  mutate(predicted_TUG = predict(best_model),
         residuals = TUG - predicted_TUG)

# Plot observed vs predicted values to visualise model fit
ggplot(stroke, aes(x = predicted_TUG, y = TUG)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Model Fit: Observed vs Predicted TUG Scores",
       x = "Predicted TUG Score",
       y = "Observed TUG Score")

