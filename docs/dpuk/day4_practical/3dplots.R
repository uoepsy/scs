library(tidyverse)
library(plotly)
cogap <- read_csv("https://uoepsy.github.io/scs/dpuk/data/acer.csv")


plt1_cloud <- plot_ly(cogap, x=~yrs_educ, y=~lesionload_ml, marker=list(colorscale='Plasma',color=~yrs_educ),z=~acer, type="scatter3d",mode="markers") 

steps=100
ll <- with(cogap, seq(min(lesionload_ml),max(lesionload_ml),length=steps))
ye <- with(cogap, seq(min(yrs_educ),max(yrs_educ),length=steps))

mod2 <- lm(acer ~ yrs_educ + lesionload_ml, data = cogap)
acr <- matrix(predict(mod2, expand.grid(lesionload_ml = ll, yrs_educ = ye)), steps, steps)
plt2_surf <- plot_ly(cogap, x=~yrs_educ, y=~lesionload_ml, z=~acer, 
                     type="scatter3d",mode="markers",color=NA) %>%
    add_trace(x = ye, y = ll, z = acr, 
              type="surface",colorscale="Viridis")
plt3_surfcloud <- plot_ly(cogap, 
                          x=~yrs_educ, y=~lesionload_ml, z=~acer, 
                          type="scatter3d",mode="markers") %>%
    add_trace(x = ye, y = ll, z = acr, 
              type="surface",colorscale="Viridis")


mod3 <- lm(acer ~ yrs_educ * lesionload_ml, data = cogap)
acr <- matrix(predict(mod3, expand.grid(lesionload_ml = ll, yrs_educ = ye)), steps, steps)

plt4_surfint <- plot_ly(cogap, 
                        x=~yrs_educ, y=~lesionload_ml, z=~acer, 
                        type="scatter3d",mode="markers",color=NA) %>%
    add_trace(x = ye, y = ll, z = acr,
              type="surface",colorscale="Viridis")

rm(list=ls()[!grepl("plt",ls())])
