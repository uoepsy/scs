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


# Load required libraries
# tidyverse: collection of packages for data manipulation and visualisation
# psych: useful for descriptive statistics
library(___)
library(___)

# Load the stroke dataset
stroke <- read_csv("___")

# Variable descriptions:
# - physio: Hours of physiotherapy received over the course of 2 weeks
# - TUG: Timed Up & Go test score (seconds) - our outcome variable
# - NIHSS: NIH Stroke Scale score (higher = more severe stroke)
# - side: Side of brain affected by stroke ("Left" or "Right")
# - Leftside: Binary indicator for left side (1 = Left, 0 = Right)
# - hosp: Hospital where treatment occurred (A, B, or C)

# Inspect the data
___(stroke)  # View the first 6 rows of the dataset
___(stroke)  # Check the variable types

# Convert categorical variables to factors with appropriate levels
# This ensures correct reference categories and interpretation
___
___

# Check distribution of categorical variables
table(stroke$___)
table(stroke$___)

# Summary statistics for key numeric variables
# The function describe() from the psych package provides mean, SD, range, etc. 
# to understand distributions
stroke |>
  select(___, ___, ___) |>
  describe()


# Data Visualisation: Understanding distributions and relationships


# Visualising distributions of individual variables

# Distribution of TUG scores (our outcome variable)
ggplot(stroke, aes(x = ___)) +
  ___() +
  labs(___)

# Distribution of physiotherapy hours (predictor variable)
ggplot(stroke, aes(x = ___)) +
  ___() +
  labs(___)

# Distribution of NIHSS scores (stroke severity, predictor variable)
ggplot(stroke, aes(x = ___)) +
  ___() +
  labs(___)


# Visualising relationships between variables


# Research Question 1: Relationship between physiotherapy and TUG
ggplot(stroke, aes(x = ____, y = ___)) +
  ___() +
  labs(___)

# Research Question 1: Relationship between stroke severity and TUG
ggplot(stroke, aes(x = ___, y = ___)) +
  ___() +
  labs(___)

# Research Question 2: TUG scores by side of brain affected
ggplot(stroke, aes(x = ___, y = ___, fill = ___)) +
  ___() +
  labs()

# Research Question 3: TUG scores by hospital
ggplot(stroke, aes(x = ___, y = ___, fill = ___)) +
  ___() +
  labs(___)


# 2) DATA PREPARATION


# Mean-center NIHSS for better interpretability of coefficients
# This transforms the intercept to represent the predicted TUG for a patient
# with average stroke severity, rather than "zero severity"
___ <- ___ |>
  mutate(NIHSS_mc = ___ - ____)

# Verify the centering worked correctly
mean(stroke$NIHSS)    # Original mean
mean(stroke$NIHSS_mc) # Should be very close to zero


# 3) MODEL BUILDING


# Model 0: Intercept-only model (baseline)
# This represents the null hypothesis that none of our predictors matter
mdl0 <- lm(TUG ~ ___, data = ____)
summary(mdl0)

# RESEARCH QUESTION 1
# How much variance in recovery outcomes can be explained by 
# physiotherapy duration and stroke severity?

# Model 1: Simple linear regression with physiotherapy hours only
mdl1 <- lm(TUG ~ ___, data = ___)
summary(mdl1)

# Model 2: Multiple regression with physiotherapy and mean-centred stroke severity
mdl2 <- lm(TUG ~ ___ + ___, data = ___)
summary(mdl2)


# RESEARCH QUESTION 2
# Does the side of the brain affected influence recovery 
# after controlling for other factors?

# Model 3: Add side of brain affected
mdl3 <- lm(TUG ~ ___ + ___ + ___, data = ___)
summary(mdl3)


# RESEARCH QUESTION 3
# Are there differences in patient outcomes between hospitals?

# Model 4: Full model with hospital variable added
mdl4 <- lm(TUG ~ ___ + ___ + ___ + ___, data = ___)
summary(mdl4)


# 4) MODEL COMPARISON


# RESEARCH QUESTION 1
# Compare variance explained by different factors using ANOVA tests

# Model comparisons for Research Question 1
# Test if adding physiotherapy significantly improves over intercept-only model
anova(___, ___)


# Test if adding stroke severity significantly improves over physiotherapy-only model
anova(___, ___)


# RESEARCH QUESTION 2
# Test if adding side of brain significantly improves the model
anova(___, ___)


# RESEARCH QUESTION 3
# Test if adding hospital significantly improves the model
anova(___, ___)


# Hint: extracting adjusted R-squared values for a model
summary(mdl0)$adj.r.squared


# Compare adjusted R-squared values across all models
# This shows the proportion of variance explained by each successive model
r2_values <- tibble(
  Model = c("Intercept only", "Physio only", "Physio + NIHSS_mc", 
            "Physio + NIHSS_mc + Side", "Full model"),
  Adj_R_Squared = c(___)
)
r2_values

# Compare all models using ANOVA for a comprehensive view
anova(___, ___, ___, ___, ___)

best_model <- ___


# 5) INTERPRETATION OF BEST MODEL


# Get detailed summary of the best model
summary(best_model)

# Coefficient interpretations:


# Calculate predicted values and residuals for model diagnostics
stroke <- stroke |>
  mutate(predicted_TUG = predict(___),
         residuals = ___ - ___)

# Plot observed vs predicted values to visualise model fit
ggplot(stroke, aes(x = ___, y = ___)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Model Fit: Observed vs Predicted TUG Scores",
       x = "Predicted TUG Score",
       y = "Observed TUG Score")

