# DPUK - Linear models training - Day 4 (Thursday)
# Author: Josiah King, Umberto Noe, Tom Booth

# Library imports
# ===============
library(tidyverse)
library(sjPlot)
library(psych)

# ACER data
# ==============
# sample of 150 stroke patients assessed using the ACE-R (neuropsych test used to identify cognitive impairment). Also measured patients' age, years of education, birthweight, lesion volume. 
cogap <- read_csv("acer.csv")
describe(cogap, omit=TRUE)

cogap <- cogap %>%
    mutate(
        apoe4status = factor(cogap$apoe4status, levels = c("negative","positive"))
    )



# ==============
# CONTINUOUS * CATEGORICAL
# ==============

# Models with no interactions are fitting parallel lines for each category
mod0 <- lm(acer ~ age + apoe4status, data = cogap)

plot_model(mod0, type="eff", terms=c("age","apoe4status"))

# coefficients are "holding other predictors constant"  
summary(mod0)

ggplot(cogap, aes(x = age, y = acer, col = apoe4status)) + 
    geom_point() +
    facet_wrap(~apoe4status)

# Adding an interaction allows the lines to be non-parallel

# A*B is shorthand for A+B+A*B
# These are the same:  
# acer ~ age * apoe4status 
# acer ~ age + apoe4status + age:apoe4status
mod1 <- lm(acer ~ age + apoe4status + age:apoe4status, data = cogap)

plot_model(mod1, type="eff", terms=c("age","apoe4status"))
# also available "int" for interaction:
# plot_model(mod1, type="int")
summary(mod1)

# Interpretation  
# ==============


## Conditional effects 
## 
# The interaction A:B changes the interpretation of coefficients A and B 
coef(mod1)
plot_model(mod1, type="eff", terms=c("age [0:100]","apoe4status"))
    

# Mean centering changes where "zero" is. 
cogap$ageC <- scale(cogap$age, center = TRUE, scale = FALSE)
# and changes our coefficients:  
mod1_c <- lm(acer ~ ageC * apoe4status, data = cogap)
coef(mod1_c)
plot_model(mod1_c, type="eff", terms=c("ageC","apoe4status"))

## The interaction 
##
# The interaction term is an "adjustment"
coef(mod1)

cogapNeg <- cogap %>% filter(apoe4status == "negative")
cogapPos <- cogap %>% filter(apoe4status == "positive")
mod1neg <- lm(acer ~ age, data = cogapNeg)
coef(mod1neg)
mod1pos <- lm(acer ~ age, data = cogapPos)
coef(mod1pos)

coef(mod1)


# QUESTION 
# What happens to the interaction term, if we ...
# 1. swap the reference level for the factor
# 2. re-fit the model?
cogap$apoe4status <- 
    factor(cogap$apoe4status, 
           levels = c("positive","negative"))

mod1relevel <- lm(acer ~ age * apoe4status, data = cogap)

summary(mod1relevel)



##### TODO MOVE TO END
# =====================
# Practice 1: DPUK data
# =====================

# 1. Choose:
#   (a) an outcome variable
#   (b) one numeric explanatory variables
#   (c) one categorical variable (make sure you convert the categorical variable to a factor and tidy up the levels of the factor).

# 2. Fit a model with a continuous * categorical interaction

#   + Interpret the coefficients, t-static and p-values
#   + How good is the model fit?  

# Discussion





# =====================
# BREAK :)
# =====================




# =====================
# CONTINUOUS * CONTINUOUS
# =====================

# Lets pick two other explanatory variables for this example. Education and Lesion load.  

# 3D plot! (demonstration only)
# plt1_cloud

# Models with no interactions are fitting flat surfaces across the continuous predictor
mod2 <- lm(acer ~ yrs_educ + lesionload_ml, data = cogap)
summary(mod2)


# 3D example (demonstration only)
# plt2_surf
# plt3_surfcloud

# 2D approximation
plot_model(mod2, type="eff", terms=c("lesionload_ml","yrs_educ"))

# Adding an interaction allows the surfaces to twist!
mod3<- lm(acer ~ yrs_educ * lesionload_ml, data = cogap)

# 3D example (demonstration only)
# plt4_surfint

# 2D approx
plot_model(mod3, type="eff", terms=c("lesionload_ml","yrs_educ"))

# Interpretation
summary(mod3)

# Same logic as continuous * categorical

# The coefficients for education and lesionload are *conditional upon the other variable being zero*.

# The interaction is the adjustment
coef(mod3)
plot_model(mod3, type="eff", terms=c("lesionload_ml","yrs_educ [20, 21]"))
plot_model(mod3, type="eff", terms=c("lesionload_ml","yrs_educ [20, 21, 22]"))
plot_model(mod3, type="eff", terms=c("lesionload_ml","yrs_educ [20:24]"))



# =====================
# Practice 2: DPUK data
# =====================

# 1. Choose:
#   (a) an outcome variable
#   (b) two numeric explanatory variables

# 2. Fit a model with a continuous * continuous interaction, and interpret the coefficients. 

# Discussion



# =====================
# Practice 3: DPUK data
# =====================

# 1. Fit a model with an interaction AND some other predictor variable(s). 
# here's an example:
# mod_p3 <- lm(SYN11_MMSE ~ SYN10_CESDDEPRESS + SYN01_AGEATASS * SYN01_SEX, data = dpuk) 
# summary(mod_p3) 

# 2. Interpret

# Discussion

