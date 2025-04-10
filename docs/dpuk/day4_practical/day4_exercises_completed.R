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

ggplot(data = cogap, aes(x = age, y = acer)) + 
  geom_point(alpha = .4, size = 3)+
  facet_wrap(~apoe4status) +
  theme_light()



# 3) Relevel the apoe4 variable so that "negative" is the reference level
# if a character variable is passed to factor(), you can specify the levels in the order you want
# data <- data |> 
#   mutate(
#     variable = factor(variable, levels = c("B","A"))
#   )
cogap <- cogap |>
  mutate(
    apoe4status = factor(apoe4status, 
                         levels = c("negative","positive"),
                         labels = c("Neg","Pos"))
  )

summary(cogap)


# 4) Fit this model:  
mod0 <- lm(acer ~ age + apoe4status, data = cogap)

coef(mod0)

# Here is a plot of the data.  
# we can use geom_abline() to manually add some lines. 
# From the coefficients above, can you fill in the blanks?  
ggplot(data = cogap, aes(x = age, y = acer, col = apoe4status)) +
  geom_point() + 
  geom_abline(aes(intercept = 102.295, slope = -0.1076517, col = "Neg")) +
  geom_abline(aes(intercept = 102.295-4.32, slope = -0.1076517, col = "Pos"))


# 5) Try one of the (many) helper packages in R: sjPlot - 
plot_model(mod0, type="pred", terms=c("age","apoe4status"), 
           show.data = TRUE)


# 6) Fit a model that includes an interaction between age and apoe4
mod1 <- lm(acer ~ age + apoe4status + age:apoe4status, data = cogap)
# OR
mod1 <- lm(acer ~ age * apoe4status, data = cogap)

summary(mod1)
coef(mod1)

# 7) Fill in the blanks from the interaction model, then check your understanding with sjPlot  
ggplot(data = cogap, aes(x = age, y = acer, col = apoe4status)) +
  geom_point() + 
  geom_abline(aes(intercept = 98.93667, slope = -0.05989865, col = "Neg")) +
  geom_abline(aes(intercept = 98.93667+1.07452248, slope = -0.05989865 -0.07596960, col = "Pos"))

plot_model(mod1, type="pred", terms=c("age","apoe4status"), 
           show.data = TRUE)



# 8) What do the first 3 coefficients represent?  
# it will often help to map them to parts of the plot
ggplot(data = cogap, aes(x = age, y = acer, col = apoe4status)) +
  geom_point() + 
  geom_abline(aes(intercept = 98.93667, slope = -0.05989865, col = "Neg")) +
  geom_abline(aes(intercept = 98.93667+1.07452248, slope = -0.05989865 -0.07596960, col = "Pos")) + 
  xlim(0,100) + ylim(80,100)+
  geom_vline(xintercept=0)

coef(mod1)
# intercept = 
# the estimate ACER for someone of age 0, who is apoe4 negative
# the height of the red line, where age = 0

# age coef = 
# the estimate change in ACER, as apoe4 negative people increase in age by 1 year
# the slope of the red line

# apoe4statusPos coef = 
# the estimated difference in ACER between apoe4 neg and pos, specifically for people who are 0 years old
# difference in height of blue to red line, where age = 0


# 9) re-center the age variable on something more meaningful (choice is yours!)
cogap <- cogap |> 
  mutate(
    ageC = age - 50
  )

coef(mod1)

# 10) and refit your model with the centered age variable. 
# before you look - what do you expect to happen to the coefficients? which ones?  
mod1_c <- lm(acer ~ ageC * apoe4status, data = cogap)

coef(mod1_c)
# intercept = will be lower (now estimated at age 50)
# age coef = slope just the same
# apoe4pos coef = will different. will now be negative
# age:apoe4pos interaction = just the same


# 11) What does the interaction coefficient represent? 
# (again, map it to the plot!)

# interaction coef: 
# for apoe4 negative, every year of age is associated with a decrease of 0.06 in ACER. 
# for every year of age, ACER decreases by a further 0.076 for apoe4 positive compared to negative 



# 12) just thinking - what will happen to the coefficients 
# if we switch the reference level?
mod1_c <- lm(acer ~ ageC * apoe4status, data = cogap)
coef(mod1_c)

# intercept - different, will now be lower
# ageC coef - different, will be more steep (lower)
# apoe4Neg coef:  +2.72395732
# interaction: +0.07596960


cogap <- cogap |>
  mutate(
    apoe4status = fct_relevel(apoe4status, "Pos")
  )
mod1_d <- lm(acer ~ ageC * apoe4status, data = cogap)
coef(mod1_d)



# =====================
# CONTINUOUS * CONTINUOUS
# =====================
# Lets pick two other explanatory variables for this example. Education and Lesion load.  

# 13) Here is a model with no interaction.  
mod2 <- lm(acer ~1 +  yrs_educ + lesionload_ml, data = cogap)
summary(mod2)
# What is the estimated association between lesion size and acer, for people who have 
# a) 0 years of education?
acer ~ -0.15061*lesion_load
# b) 10 years of education?
acer ~ -0.15061*lesion_load
# c) 15 years of education?
acer ~ -0.15061*lesion_load

plot_model(mod2, type="pred", 
           terms=c("lesionload_ml","yrs_educ [0, 10, 15]"))



# 14) add the interaction between lesion size and education years  
mod3 <- lm(acer ~1 +  yrs_educ * lesionload_ml, data = cogap)

plot_model(mod3, type="pred", 
           terms=c("lesionload_ml","yrs_educ [0, 10, 15]"))


# 15) how does the association between lesion size and cognitive score (acer) 
# differ depending on years of education?  
# hint (a plot will probably help, and you can go straight to something like sjPlot to save time)




# 16) Interpret each coefficient  
coef(mod3)

# intercept = estimated ACER for someone with 0 yrs educ, and 0ml of lesions
# yrs_educ coef = estimated change in ACER associated with 1 additional year of education, for people who have 0ml of lesions
# lesionload coef = estimated change in ACER associated with 1ml additional lesions, for people who 0 years of educ
# yrseduc:lesionload = estimated change in ACER associated with 1ml additional lesions, is decreased by 0.02 for every additional year of education

plot_model(mod3, type="pred", 
           terms=c("lesionload_ml","yrs_educ [15, 19, 24]"))

plot_model(mod3, type="pred", 
           terms=c("yrs_educ","lesionload_ml [8, 40, 65]"))
