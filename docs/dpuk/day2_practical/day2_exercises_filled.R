# DPUK Spring Academy 2025
# Day 2: Linear Models Tutorial
# Edinburgh Stats Team


# We will use data from a study that investigated the recovery of stroke patients. 
# Data were collected from 32 patients who experienced strokes, including the severity 
# of the stroke and whether the left or right side of the brain was affected. 
# Two weeks post-stroke, recovery was assessed using the "Timed Up & Go" (TUG) test, 
# a measure of physical functioning. 
# Additionally, the dataset includes the number of hours patients spent in physiotherapy 
# during the two-week period.

# https://uoepsy.github.io/scs/dpuk/day1_practical/pets_seattle.csv

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
# Fill in the blanks ___ where indicated. You can add new lines, rename variables, etc.


# 1) READ DATA

# Load tidyverse and read stroke.csv
# Fill in the URL or a local path to stroke.csv

library(tidyverse)
stroke <- read_csv("https://uoepsy.github.io/scs/dpuk/day2_practical/stroke.csv")  # fill URL or local path

library(readr)
stroke <- read_csv("dpuk/stroke.csv")
View(stroke)

dim(stroke)

# ifelse(test, if_true, if_false)
stroke$leftsidenew <- ifelse(stroke$side == "Left", 1, 0)
stroke$leftsidenew <- NULL


# 2) EXPLORATORY PLOTS

# a) Use ggplot to create a scatterplot of 'TUG' vs. 'physio'
# b) Add a line through the scatterplot
# c) Label axes and add a title

ggplot(stroke, aes(x = physio, y = TUG)) +
  geom_point() +
  geom_smooth(method = "lm", colour = "red", se = FALSE) +
  labs(x = "Physiotherapy (hours)", 
       y = "Timed Up & Go (TUG, secs)",
       title = "Relationship between TUG and Physiotherapy hours")


ggplot(stroke, aes(x = physio)) +
    geom_histogram() +
    labs(x = "Physiotherapy (hours)", y = "Frequency")

ggplot(stroke, aes(x = TUG)) +
    geom_histogram() +
    labs(x = "Timed Up & Go (TUG, secs)", y = "Frequency")


# BREAK 1


# 3) DESCRIPTIVE STATISTICS

# Get summary stats (mean, sd) for TUG and physio

stroke |>
  summarise(
    n = n(),
    M_physio = mean(physio),
    SD_physio = sd(physio),
    M_TUG = mean(TUG),
    SD_TUG = sd(TUG),
    r = cor(physio, TUG)
  )



# 4) FIT A LINEAR MODEL

# a) Fit a linear model with TUG as DV and physio as IV
# b) Assign to an object named mdl1
mdl1 <- lm(TUG ~ physio, data = stroke)

# c) View the summary
summary(mdl1)


# 5) INTERPRET THE COEFFICIENTS

coef(mdl1)
summary(mdl1)$coefficients
mdl1$coefficients

# a) Write an interpretation of the intercept (mdl1$coefficients[1]) as a comment
# If a patient receives 0 hours of physiotherapy, their expected TUG is about 74.95 seconds.


# b) Write an interpretation of the slope (mdl1$coefficients[2]) as a comment
# For each additional hour of physiotherapy, TUG decreases by about 2.65 seconds on average.


# 6) MAP INTERCEPT & SLOPE TO PLOT

# a) Extract numeric intercept & slope from your model
int <- coef(mdl1)[1]
sl <- coef(mdl1)[2]

# b) Reproduce scatterplot with geom_abline
ggplot(stroke, aes(x = physio, y = TUG)) +
    geom_point() +
    geom_abline(intercept = int, slope = sl, color = "red") +
    labs(x = "Physiotherapy (hours)", 
         y = "Timed Up & Go (secs)",
         title = "TUG vs Physiotherapy: Fitted Line")


# BREAK 2


# 7) LM WITH A BINARY X

ggplot(stroke, aes(x = Leftside)) +
    geom_bar()

# Fit a linear model with TUG as DV and Leftside as IV
mdl2 <- lm(TUG ~ Leftside, data = stroke)
summary(mdl2)


# 8) INTERPRETATION

# a) Write an interpretation of the intercept

# The expected TUG score for patients with right-side stroke is 59.92 seconds.

# b) Write an interpretation of the slope

# Patients with left-side stroke have TUG times about 11 seconds shorter, on average, compared to right-side stroke patients.


# 9) PLOT

# Compute mean TUG by for each value of Leftside
stroke |>
  group_by(Leftside) |>
  summarise(M_TUG = mean(TUG))

# Visualise relationship using boxplot
ggplot(stroke, aes(x = factor(Leftside), y = TUG)) +
    geom_boxplot() +
    labs(x = "Side (0 = Right, 1 = Left)", 
         y = "TUG (secs)",
         title = "TUG by Stroke Side")
