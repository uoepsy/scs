# DPUK Spring Academy 2025
# Day 4: Interactions
# Edinburgh Stats Team


# Helpful centering reminders:
# In the model Y ~ X + Z + X:Z
#   if you replace X with (X - mean(X)), you get effect of Z at the average of X 
#   if you replace X with (X - A), you get effects of Z when X equals A


# RESEARCH CONTEXT AND QUESTIONS

# Instead of working with specific research questions here, we are going to just play
# around with two types of interaction, in order to get a better understanding of the 
# structure of these models.  

# We have data on a sample of 150 stroke patients assessed using the ACE-R (neuropsych test used 
# to identify cognitive impairment). Also measured patients' age, 
# years of education, birthweight, lesion volume. 


# Library imports
# ===============
library(tidyverse)
library(sjPlot)
library(psych)

# ============================
# SETUP AND EXPLORATORY DATA ANALYSIS
# ============================

# 1) Read in the data
## explore, and take notes

cogap <- read_csv("https://uoepsy.github.io/scs/dpuk/data/acer.csv")

# using the psych package, try:  
describe(cogap, omit=TRUE)
pairs.panels(cogap)


# ============================
# CONTINUOUS * CATEGORICAL
# ============================

# 2) Plot the association between age and ACER. Can you somehow visually separate the people who are apoe4 positive vs negative?  





# 3) Relevel the apoe4 variable so that "negative" is the reference level
# if a character variable is passed to factor(), you can specify the levels in the order you want
# data <- data |> 
#   mutate(
#     variable = factor(variable, levels = c("B","A"))
#   )




# 4) Fit this model:  
mod0 <- lm(acer ~ age + apoe4status, data = cogap)

coef(mod0)

# Here is a plot of the data.  
# we can use geom_abline() to manually add some lines. 
# From the coefficients above, can you fill in the blanks?  
ggplot(data = cogap, aes(x = age, y = acer, col = apoe4status)) +
  geom_point() + 
  geom_abline(aes(intercept = ____, slope = ____, col = "negative")) +
  geom_abline(aes(intercept = ____, slope = ____, col = "positive"))


# 5) Try one of the (many) helper packages in R: sjPlot - 
plot_model(mod0, type="eff", terms=c("age","apoe4status"), show.data = TRUE)


# 6) Fit a model that includes an interaction between age and apoe4




# 7) Fill in the blanks from the interaction model, then check your understanding with sjPlot  
ggplot(data = cogap, aes(x = age, y = acer, col = apoe4status)) +
  geom_point() + 
  geom_abline(aes(intercept = ____, slope = ____, col = "negative")) +
  geom_abline(aes(intercept = ____, slope = ____, col = "positive"))



# 8) What do the first 3 coefficients represent?  
# it will often help to map them to parts of the plot
ggplot(data = cogap, aes(x = age, y = acer, col = apoe4status)) +
  geom_point() + 
  geom_abline(aes(intercept = ____, slope = ____, col = "negative")) +
  geom_abline(aes(intercept = ____, slope = ____, col = "positive")) +
  xlim(0,100)


# 9) re-center the age variable on something more meaningful (choice is yours!)
cogap <- cogap |> 
  mutate(
    ageC = ____
  )

# 10) and refit your model with the centered age variable. 
# before you look - what do you expect to happen to the coefficients? which ones?  
mod1_c <- lm(acer ~ ageC * apoe4status, data = cogap)

coef(mod1_c)

# 11) What does the interaction coefficient represent? 
# (again, map it to the plot!)






# 12) just thinking - what will happen to the coefficients 
# if we switch the reference level?






# =====================
# CONTINUOUS * CONTINUOUS
# =====================
# Lets pick two other explanatory variables for this example. Education and Lesion load.  

# 13) Here is a model with no interaction.  
mod2 <- lm(acer ~ yrs_educ + lesionload_ml, data = cogap)

# What is the estimated association between lesion size and acer, for people who have 
# a) 0 years of education?

# b) 10 years of education?

# c) 15 years of education?



# 14) add the interaction between lesion size and education years  





# 15) how does the association between lesion size and cognitive score (acer) 
# differ depending on years of education?  
# hint (a plot will probably help, and you can go straight to something like sjPlot to save time)






# 16) Interpret each coefficient  


