# Modeling the effects of Climate and Herbivory on Shrub Growth
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

mean(sd_final_cch$resid) # = 0.1251182
min(sd_final_cch$resid) # = -3.69711
max(sd_final_cch$resid) # = 4.042966

hist(sd_final_cch$resid, xlab = "Residuals", main = "")

# Review the fixed effects which will be used in the models and insure no NA values are present 
summary(sd_final_cch)

# Model Variables Key

str(sd_final_cch)

#Dependent/Response Variable:
  # resid

#Random Effects:
  # ~1 | Section/Shrub ID
  # This indicates that shrub is nested within section

#Fixed Effects/Explanatory Variables
  #C1 = iem.summ.temp
  #C2 = iem.summ.rain

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
null_model = lme(resid ~ 1, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

# CLIMATE MODELS #### 

# Average summer temperature model
C1_model = lme(resid ~ iem.summ.temp, data = sd_final_cch, random = ~ 1|Section/ShrubID,
              method = "ML")

  anova(null_model, C1_model)

  summary(C1_model)

# Summer percipitation model
C2_model = lme(resid ~ iem.summ.rain, data = sd_final_cch, random = ~ 1|Section/ShrubID,
               method = "ML")

  anova(null_model, C3_model)
  
  summary(C3_model)
  

# Climate models with mean summer temperature & mean summer percipitation model
C1C2_model = lme(resid ~ iem.summ.temp + iem.summ.rain, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                            na.action=na.pass, method = "ML", control = lmeControl(opt = "optim"))

anova(null_model, C1_model, C2_model, C3_model)

summary(C1C3_model)

# Climate model with an interaction term between summer temperature & mean summer percipitation model
  #This model is used to assess wether growth is greater in years when it is both warmer and wetter
  #We see that as mean precipitation increases, shrub growth increases more for every degree c increase

core_climate = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain , data = sd_final_cch, random = ~ 1|Section/ShrubID,
                   method = "ML")

anova(C1C3_model, core_climate)

summary(core_climate)


C2C3_climate = lme(resid ~ iem.temp + iem.summ.rain + iem.temp * iem.summ.rain , data = sd_final_cch, random = ~ 1|Section/ShrubID,
                   method = "ML")

anova(core_climate, C2C3_climate)

summary(core_climate)

#Plots the Satadardize Residuals plot for the core climate model
plot(core_climate)


# HERBIVORE MODELS#### 

# Temporal Moose Density model
H1_model <- lme(resid ~ MooseDensity, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

  anova(null_model, H1_model)
  
  summary(H1_model)

# Temporal Hare Index, 1-3 scale representing different amplitudes of hare population peaks
core_herb <- lme(resid ~ HareIndex, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

  anova(null_model, core_herb )
  
  summary(core_herb )

      # Tests to see if making the hare index a factorial variable will change the outcome
      H2_fac_model <- lme(resid ~ as.factor(HareIndex), data = sd_final_cch, random = ~ 1|Section/ShrubID,
                      method = "ML")
      
          anova(null_model, H2_fac_model)
          
          anova(H2_model, H2_fac_model)
          
          summary(H2_fac_model)

# Spatial browsing pressure by Moose model 
H3_model <- lme(resid ~ PropMoose, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

  anova(null_model, H3_model)
  
  summary(H3_model)

# Spatial browsing pressure by Hare model 
H4_model <- lme(resid ~ PropHare, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

  anova(null_model, H4_model)
  
  summary(H4_model)

# Spatial browsing pressure by Ptarmagin model 
H5_model <- lme(resid ~ PropPtarmagin, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

  anova(null_model, H5_model)
  
  summary(H5_model)

# Comparison of all base herbivore models
anova(H1_model, H2_model, H3_model, H4_model, H5_model)

# Herbivore model with temporal moose & hare index data
H1H2_model <- lme(resid ~ MooseDensity + HareIndex, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

  anova(H2_model, H1H2_model)

  summary(H1H2_model)
  
          # Test to see if there is any interactive effect between hare density and moose density
          H1xH2_model <- lme(resid ~ MooseDensity * HareIndex, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                             method = "ML")
          
          anova(H2_model, H1xH2_model)
          
          summary(H1xH2_model)

# Herbivore model with spatial browsing intensity by moose & hare index data
H3H2_model <- lme(resid ~ PropMoose + HareIndex, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")

  anova(H2_model, H3H2_model)
  
  summary(H3H2_model)
  
# HERBIVORE MODELS SITE LEVEL #### 
  
  # Temporal Moose Density model
  H1_model <- lme(resid ~ MooseDensity, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(null_model, H1_model)
  
  summary(H1_model)
  
  # Temporal Hare Index, 1-3 scale representing different amplitudes of hare population peaks
  core_herb <- lme(resid ~ HareIndex, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                   method = "ML")
  
  anova(null_model, core_herb )
  
  summary(core_herb )
  
  # Tests to see if making the hare index a factorial variable will change the outcome
  H2_fac_model <- lme(resid ~ as.factor(HareIndex), data = sd_final_cch, random = ~ 1|Section/ShrubID,
                      method = "ML")
  
  anova(null_model, H2_fac_model)
  
  anova(H2_model, H2_fac_model)
  
  summary(H2_fac_model)
  
  # Spatial browsing pressure by Moose model 
  H3_model <- lme(resid ~ PropMoose_S, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(null_model, H3_model)
  
  summary(H3_model)
  
  # Spatial browsing pressure by Hare model 
  H4_model <- lme(resid ~ PropHare_S, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(null_model, H4_model)
  
  summary(H4_model)
  
  # Spatial browsing pressure by Ptarmagin model 
  H5_model <- lme(resid ~ PropPtarmagin_S, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(null_model, H5_model)
  
  summary(H5_model)
  
  # Comparison of all base herbivore models
  anova(H1_model, H2_model, H3_model, H4_model, H5_model)
  
  # Herbivore model with temporal moose & hare index data
  H1H2_model <- lme(resid ~ MooseDensity + HareIndex, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(H2_model, H1H2_model)
  
  summary(H1H2_model)
  
  # Test to see if there is any interactive effect between hare density and moose density
  H1xH2_model <- lme(resid ~ MooseDensity * HareIndex, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                     method = "ML")
  
  anova(H2_model, H1xH2_model)
  
  summary(H1xH2_model)
  
  # Herbivore model with spatial browsing intensity by moose & hare index data
  H3H2_model <- lme(resid ~ PropMoose + HareIndex, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(H2_model, H3H2_model)
  
  summary(H3H2_model)

# CLIMATE & HERBIVORE MODELS ####

# Models the interactive effects of mean summer temperature and presepitiation and temporal hare index
CH2_model = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                  HareIndex, 
                  data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")

  anova(core_climate, core_herb, CH2_model)
  
  summary(CH2_model)
  
  ggplot(data = sd_final_cch, 
         aes(x = resid,
             y = HareIndex,
             colour = Genus)) + 
    geom_point(size = 2) +
    geom_smooth(method= "lm") +
    xlab("Residuals") + ylab("Hare Index") +
    scale_y_continuous (breaks=seq(1, 3, 1))
  
  ggplot(data = sd_final_cch, 
         aes(x = resid,
             y = MooseDensity,
             colour=Genus)) + 
    geom_point(size = 2) +
    geom_smooth(method= "lm") +
    xlab("Residuals") + ylab("Moose Density (moose/mile²)") 
  
  ggplot(data = sd_final_cch, 
         aes(x = resid,
             y = iem.summ.temp,
             colour=Genus)) + 
    geom_point(size = 2) +
    geom_smooth(method= "lm") +
    xlab("Residuals") + ylab("Mean Summer Temperature (°C)")
  
  ggplot(data = sd_final_cch, 
         aes(x = resid,
             y = iem.summ.rain,
             colour=Genus)) + 
    geom_point(size = 2) +
    geom_smooth(method= "lm") +
    xlab("Residuals") + ylab("Mean Summer Precipitation (mm)")
  
  
# Models the interactive effects of mean summer temperature and presepitiation, temporal hare index and temporal moose density
CH1H2_model = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                  MooseDensity + HareIndex,
                  data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")

  anova(CH2_model, CH1H2_model)
  
  summary(CH1H2_model)

# CLIMATE & HERBIVORE MODELS WITH INTERACTION TERMS ####

# Models the interactive effects of mean summer temperature and presepitiation and temporal hare index 
#including an interaction term between summer rain and hare index
CH2_int_rain_model = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                       HareIndex + iem.summ.rain * HareIndex,
                       data = sd_final_cch, random = ~ 1|Section/ShrubID,
                       method = "ML")

  anova(core_climate, CH2_model, CH2_int_rain_model)
  
  summary(CH2_int_rain_model)
  
# Models the interactive effects of mean summer temperature and presepitiation and temporal hare index 
#including an interaction term between summer temp and hare index
CH2_int_temp_model = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                      HareIndex + iem.summ.temp * HareIndex,
                      data = sd_final_cch, random = ~ 1|Section/ShrubID,
                      method = "ML")
  
  anova(core_climate, CH2_model, CH2_int_temp_model)
  
    # Models the interactive effects of mean summer temperature and presepitiation and spatial browing intensity 
    #including an interaction term between summer temp and spatial browing intensity 
    CH3_int_model = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                            PropMoose + iem.summ.temp * PropMoose,
                            data = sd_final_cch, random = ~ 1|Section/ShrubID,
                            method = "ML")
      
      anova(core_climate, CH3_int_model)
      
      # Models the interactive effects of mean summer temperature and presepitiation and temporal moose denisty
      #including an interaction term between summer temp and temporal moose denisty
    CH1_int_model = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                            MooseDensity + iem.summ.temp * MooseDensity,
                            data = sd_final_cch, random = ~ 1|Section/ShrubID,
                            method = "ML")
    
      anova(core_climate, CH1_int_model)


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

C1C3H2_model = lme(resid ~ iem.summ.temp + iem.summ.rain +
                  HareIndex, 
                  data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")

#Calculate the VIFs for the model without climatic interaction terms 
#Note that values for VIF greater than two should be investigated further

vif.lme(C1C3H2_model)
      

# LIKELIHOOD RATIO TEST BASED BACKWARD SELECTION ####

# Stepwise regression using mean summer temperature rather than mean annual temperature

lme.full.s = lme(resid ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose + PropHare + PropPtarmagin, 
               data = sd_final_cch, random = ~ 1|Section/ShrubID,
               method = "ML")

summary(lme.full.s)

drop1(lme.full.s, test = "Chisq")

drop1(update(lme.full.s, ~ . -PropPtarmagin), test = "Chisq")

drop1(update(lme.full.s, ~ . -PropPtarmagin -PropHare), test = "Chisq")

drop1(update(lme.full.s, ~ . -PropPtarmagin -PropHare -MooseDensity), test = "Chisq")

drop1(update(lme.full.s, ~ . -PropPtarmagin -PropHare -MooseDensity -PropMoose), test = "Chisq")

# Stepwise regression using mean annual temperature rather than mean summer temperature

# utlize the code "control=lmeControl(opt = "optim")" in the model as an optimizing algorithm because this model
#produces the following error:
  
  #Error in lme.formula(fixed = resid ~ iem.temp + iem.summ.rain + HareIndex +  : 
  #nlminb problem, convergence error code = 1
  #message = false convergence (8)


lme.full = lme(resid ~ iem.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose + PropHare + PropPtarmagin, 
               data = sd_final_cch, random = ~ 1|Section/ShrubID,
               method = "ML", control = lmeControl (opt = "optim"))

summary(lme.full)

drop1(lme.full, test = "Chisq")

drop1(update(lme.full, ~ . -MooseDensity), test = "Chisq")

drop1(update(lme.full, ~ . -MooseDensity -PropPtarmagin), test = "Chisq")

drop1(update(lme.full, ~ . -MooseDensity -PropPtarmagin -PropHare), test = "Chisq")

drop1(update(lme.full, ~ . -MooseDensity -PropPtarmagin -PropHare -PropMoose), test = "Chisq")



lme.full_sec = lme(resid ~ iem.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, 
               data = sd_final_cch, random = ~ 1|Section/ShrubID,
               method = "ML", control = lmeControl (opt = "optim"))

summary(lme.full_sec)

drop1(lme.full_sec, test = "Chisq")

drop1(update(lme.full_sec, ~ . -MooseDensity), test = "Chisq")

drop1(update(lme.full_sec, ~ . -MooseDensity -PropMoose_S), test = "Chisq")

drop1(update(lme.full_sec, ~ . -MooseDensity -PropMoose_S -PropHare_S), test = "Chisq")

drop1(update(lme.full_sec, ~ . -MooseDensity -PropMoose_S -PropHare_S -PropPtarmagin_S), test = "Chisq")


anova(lme.full.s, CH2_model)
