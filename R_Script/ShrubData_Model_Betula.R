# Modeling the effects of Climate and Herbivory on Betula Shrub Growth
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-11-23

install.packages("lme4")           
install.packages("nlme") 
install.packages("gamlss")
install.packages("ggplot2")

library(lme4)
library(nlme)
library(gamlss)
library(regclass)
library(tidyverse)
library(caret)
library(dplR)
library(reshape2)
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)
library(corrplot)
library(RColorBrewer)

# DEVELOP MIXED EFFECTS MODELS ####

# Assess the range and mean values for the residual BAI response variable so you can interperet the effect size

mean(sd_bena_cch$resid) # = 0.1398543
min(sd_bena_cch$resid) # = -3.6971
max(sd_bena_cch$resid) # = 4.042966

hist(sd_bena_cch$resid)

# Review the fixed effects which will be used in the models and insure no NA values are present 
summary(sd_bena_cch)

# Model Variables Key

str(sd_bena_cch)

#Dependent/Response Variable:
# resid

#Random Effects:
# ~1 | Section/Shrub ID
# This indicates that shrub is nested within section

#Fixed Effects/Explanatory Variables
#C1 = iem.summ.temp
#C2 = iem.temp
#C3 = iem.summ.rain

#H1 = MooseDensity
#H2 = HareIndex
#H3 = PropMoose
#H4 = PropHare
#H5 = PropPtarmagin

#X1 = Elevation
#X2 = Slope
#X3 = Y_Cord
#X4 = DistToRoad

# NULL MODEL #### 
null_model_b = lme(resid ~ 1, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

# CLIMATE MODELS #### 

# Average summer temperature model
C1_model_b = lme(resid ~ iem.summ.temp, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
               method = "ML")

anova(null_model_b, C1_model_b)

summary(C1_model_b)

# Average annual temperature model
C2_model_b = lme(resid ~ iem.temp, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
               method = "ML")

anova(null_model_b, C1_model_b, C2_model_b)

summary(C2_model_b)

# Summer percipitation model
C3_model_b = lme(resid ~ iem.summ.rain, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
               method = "ML")

anova(null_model_b, C1_model_b, C2_model_b, C3_model_b)

summary(C3_model_b)

# Climate models with mean summer temperature & mean summer percipitation model
C1C3_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

anova(C1_model_b, C3_model_b, C1C3_model_b)

summary(C1C3_model_b)

# Climate model with an interaction term between summer temperature & mean summer percipitation model
#This model is used to assess wether growth is greater in years when it is both warmer and wetter
#We see that as mean precipitation increases, shrub growth increases more for every degree c increase

C1C3_int_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain , data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                   method = "ML")

anova(C1_model_b, C1C3_model_b, C1C3_int_model_b)

summary(C1C3_int_model_b)

#Plots the Satadardize Residuals plot for the core climate model
plot(C1C3_int_model_b)


# HERBIVORE MODELS#### 

# Temporal Moose Density model
H1_model_b <- lme(resid ~ MooseDensity, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

anova(null_model_b, H1_model_b)

summary(H1_model_b)

# Temporal Hare Index, 1-3 scale representing different amplitudes of hare population peaks
H2_model_b <- lme(resid ~ HareIndex, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

anova(null_model_b, H2_model_b )

summary(H2_model_b )

      # Tests to see if making the hare index a factorial variable will change the outcome
      H2_fac_model_b <- lme(resid ~ as.factor(HareIndex), data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                          method = "ML")
      
      anova(null_model_b, H2_fac_model_b)
      
      anova(H2_model_b, H2_fac_model_b)
      
      summary(H2_fac_model_b)

# Spatial browsing pressure by Moose model 
H3_model_b <- lme(resid ~ PropMoose, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

anova(null_model_b, H3_model_b)

summary(H3_model_b)

# Spatial browsing pressure by Hare model 
H4_model_b <- lme(resid ~ PropHare, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

anova(null_model_b, H4_model_b)

summary(H4_model_b)

# Spatial browsing pressure by Ptarmagin model 
H5_model_b <- lme(resid ~ PropPtarmagin, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

anova(null_model_b, H5_model_b)

summary(H5_model_b)

# Comparison of all base herbivore models
anova(H1_model_b, H2_model_b, H3_model_b, H4_model_b, H5_model_b)

# Herbivore model with temporal moose & hare index data
H1H2_model_b <- lme(resid ~ MooseDensity + HareIndex, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_b, H2_model_b, H1H2_model_b)

summary(H1H2_model_b)

# Test to see if there is any interactive effect between hare density and moose density
H1xH2_model_b <- lme(resid ~ MooseDensity * HareIndex, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                   method = "ML")

anova(H2_model_b, H1H2_model_b, H1xH2_model_b)

summary(H1xH2_model_b)

# Herbivore model with spatial browsing intensity by moose & hare index data
H3H2_model_b <- lme(resid ~ PropMoose + HareIndex, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(H2_model_b, H1H2_model_b, H3H2_model_b)

summary(H3H2_model_b)


# CLIMATE & HERBIVORE MODELS ####

# Models the interactive effects of mean summer temperature and presepitiation and temporal hare index
CH2_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                  HareIndex, 
                data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

anova(H2_model_b, C1C3_int_model_b, CH2_model_b)

summary(CH2_model_b)

ggplot(data = sd_bena_cch, 
       aes(x = resid,
           y = HareIndex)) + 
  geom_point(size = 2) +
  geom_smooth(method= "lm") +
  xlab("lnBai Residuals") + ylab("Hare Index") +
  scale_y_continuous (breaks=seq(1, 3, 1))


# Models the interactive effects of mean summer temperature and presepitiation, temporal hare index and temporal moose density
CH1H2_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                    MooseDensity + HareIndex,
                  data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(CH2_model_b, CH1H2_model_b)

summary(CH1H2_model_b)

# CLIMATE & HERBIVORE MODELS WITH INTERACTION TERMS ####

# Models the interactive effects of mean summer temperature and presepitiation and temporal hare index 
#including an interaction term between summer rain and hare index
CH2_int_rain_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                           HareIndex + iem.summ.rain * HareIndex,
                         data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                         method = "ML")

anova(C1C3_int_model_b, CH2_model_b, CH2_int_rain_model_b)

summary(CH2_int_rain_model_b)

# Models the interactive effects of mean summer temperature and presepitiation and temporal hare index 
#including an interaction term between summer temp and hare index
CH2_int_temp_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                           HareIndex + iem.summ.temp * HareIndex,
                         data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                         method = "ML")

anova(C1C3_int_model_b, CH2_model_b, CH2_int_temp_model_b)

# Models the interactive effects of mean summer temperature and presepitiation and spatial browing intensity 
#including an interaction term between summer temp and spatial browing intensity 
CH3_int_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                      PropMoose + iem.summ.temp * PropMoose,
                    data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")

anova(C1C3_int_model_b, CH3_int_model_b)

# Models the interactive effects of mean summer temperature and presepitiation and temporal moose denisty
#including an interaction term between summer temp and temporal moose denisty
CH1_int_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                      MooseDensity + iem.summ.temp * MooseDensity,
                    data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")

anova(C1C3_int_model_b, CH1_int_model_b)


# CALCULATE VARIANCE INFLATION FACTOR (VIF) FOR BEST MODEL ####

# Create the function used to calculate the VIFs:

vif.lme <- function (fit) {
  
  ## Adapted from rms::vif
  
  v <- vcov(fit)
  nam <- names(fixef(fit))
  
  ## exclude intercepts
  
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }

# Create a model without the climatic interaction term

C1C3H2_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain +
                     HareIndex, 
                   data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                   method = "ML")

#Calculate the VIFs for the model without climatic interaction terms 
#Note that values for VIF greater than two should be investigated further

vif.lme(C1C3H2_model_b)
