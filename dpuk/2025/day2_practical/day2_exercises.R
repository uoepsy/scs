# Day 2 Exercises: Linear Models Tutorial

# We will use data from a study that investigated the recovery of stroke patients. 
# Data were collected from 32 patients who experienced strokes, including the severity 
# of the stroke and whether the left or right side of the brain was affected. 
# Two weeks post-stroke, recovery was assessed using the "Timed Up & Go" (TUG) test, 
# a measure of physical functioning. 
# Additionally, the dataset includes the number of hours patients spent in physiotherapy 
# during the two-week period.

# The stroke.csv dataset contains information on patients' stroke recovery:
#   - physio: Hours of physiotherapy over 2 weeks.
#   - TUG: Timed Up & Go test (lower = better physical functioning).
#   - NIHSS: NIH Stroke Severity Scale (higher = more severe stroke).
#   - side: Side of the brain affected (e.g., Left, Right).
#   - Leftside: Dummy variable (0 = Right, 1 = Left).
#   - hosp: Hospital of admission.

# RESEARCH QUESTIONS:
# Q1) Does spending more hours in physiotherapy improve physical functioning (TUG) in stroke patients?
# Q2) Is there a difference in TUG for patients with left-side vs right-side stroke?


# INSTRUCTIONS:
# Fill in the blanks ___ where indicated. You can add new lines, rename variables as you see fit. 


# 1) READ DATA

# a) Load tidyverse and read stroke.csv
#    Fill in the URL or a local path to stroke.csv

library(___)
stroke <- read_csv("___")  # fill URL or local path


# 2) EXPLORATORY PLOTS

# a) Use ggplot to create a scatterplot of 'TUG' vs. 'physio'
# b) Add a line through the scatterplot
# c) Label axes and add a title

ggplot(___, aes(x = ___, y = ___)) +
  geom___() +
  geom___(method = "lm") +
  labs(x = "___", 
       y = "___",
       title = "___")


# BREAK 1
# Discuss whether the relationship looks linear.


# 3) DESCRIPTIVE STATISTICS

# a) Get summary stats (mean, sd) for TUG and physio
# b) Summarise them in a readable format

stroke |>
  summarise(
    M_physio = mean(___),
    SD_physio = sd(___),
    M_TUG = mean(___),
    SD_TUG = sd(___)
  )


# 4) FIT A LINEAR MODEL

# a) Fit a linear model with TUG as DV and physio as IV
# b) Assign to an object named mdl1
mdl1 <- lm(___ ~ ___, data = ___)

# c) View the summary
summary(mdl1)


# 5) INTERPRET THE COEFFICIENTS

# a) Write an interpretation of the intercept (mdl1$coefficients[1]) as a comment

# b) Write an interpretation of the slope (mdl1$coefficients[2]) as a comment


# 6) MAP INTERCEPT & COEF TO PLOT

# a) Extract numeric intercept & slope from your model
intercept <- coef(mdl1)[1]
slope <- coef(mdl1)[2]

# b) Reproduce scatterplot with geom_abline
ggplot(stroke, aes(x = ___, y = ___)) +
  geom___() +
  geom_abline(intercept = intercept, slope = slope, color = "red") +
  labs(x = "___", 
       y = "___",
       title = "___")


# BREAK 2
# Reflect on model fit. Are residuals large or small?


# 7) LM WITH A BINARY X

# Fit a linear model with TUG as DV and Leftside as IV
mdl2 <- lm(___ ~ ___, data = ___)
summary(mdl2)


# 8) INTERPRETATION

# a) Write an interpretation of the intercept

# b) Write an interpretation of the slope


# 9) PLOT

# Compute mean TUG by for each value of Leftside
stroke |>
  group_by(___) |>
  summarise(___ = mean(___))

# Visualise relationship using boxplot
ggplot(stroke, aes(x = ___, y = ___)) +
  geom___() +
  labs(x = "___", 
       y = "___",
       title = "___")
