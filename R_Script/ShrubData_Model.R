# Modeling the effects of Climate and Herbivory on Shrub Growth
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-11-23

install.packages("dplR")           
install.packages("reshape2")        
install.packages("dplyr")           
install.packages("lme4")           
install.packages("nlme") 

library(dplR)
library(reshape2)
library(dplyr)
library(lme4)
library(nlme)

# Develop mixed effects models 
  
  #Model Structure
    #lme(fixed, data, random, correlation, weights, subset, method,
    # na.action, control, contrasts = NULL, keep.data = TRUE)

model_BAI <- lme(BAI ~ summ.rain + MooseDensity, data = ShrubCCH_Data, random = ~ 1,
                 na.action=na.pass)

summary(model_BAI)
