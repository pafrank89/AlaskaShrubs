# Modeling the effects of Climate and Herbivory on Shrub Growth
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-11-23

install.packages("lme4")           
install.packages("nlme") 
install.packages("gamlss")
install.packages("ggplot2")
install.packages("piecewiseSEM")
install.packages("adespatial")
install.packages("emmeans")
install.packages("effects")
install.packages("sjPlot")
install.packages("stargazer")

library(lme4)
library(nlme)
library(gamlss)
library(regclass)
library(tidyverse)
library(caret)
library(leaps)
library(reshape2)
library(ggplot2)
library(ggeffects)
library(PerformanceAnalytics)
library(corrplot)
library(RColorBrewer)
library(sjstats)
library(adespatial)
library(piecewiseSEM)
library(emmeans)
library(dplyr)
library(cowplot)
library(sjPlot) 
library(sjmisc) 
library(effects)
library(rlang)
library(stargazer)

sessionInfo()

old.packages()

update.packages(ask = FALSE)

packageVersion("rlang")

update.packages("rlang")

  
# DEVELOP MIXED EFFECTS MODELS ####

# Assess the range and mean values for the residual BAI response variable so you can interperet the effect size

mean(sd_final_cch$resid) # = 0.1251182
min(sd_final_cch$resid) # = -3.69711
max(sd_final_cch$resid) # = 4.042966

hist(sd_final_cch$resid, xlab = "Residuals", main = "")


#Assess the range of resid values across sites, to show the need for random effects
boxplot(sd_final_cch$resid ~ sd_final_cch$Section, 
        xlab = "Site", ylab = "Residuals", main = "")

(colour_plot <- ggplot(sd_final_cch, aes(x = iem.summ.rain.10, y = resid, colour = Section)) +
    geom_point(size = 2) +
    theme_classic() +
    theme(legend.position = "none"))

(split_plot <- ggplot(aes(iem.summ.rain.10, resid), data = sd_final_cch) + 
    geom_point() + 
    facet_wrap(~ Section) + # create a facet for each mountain range
    xlab("Summer Precip") + 
    ylab("Residuals"))

(mm_plot <- ggplot(sd_final_cch, aes(x = iem.summ.temp, y = resid, colour = ShrubID)) +
    facet_wrap(~Section, nrow=3) +  
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(sd_final_cch, pred = predict(C1_model)), aes(y = pred), size = 1) + 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines")))

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


  C1_modelrand = lmer(resid ~ iem.summ.temp + (1 + iem.summ.temp|Section/ShrubID), data = sd_final_cch)
      
        anova(null_model, C1_model)
      
        summary(C1_model)

# Summer percipitation model
C2_model = lme(resid ~ iem.summ.rain.10, data = sd_final_cch, random = ~ 1|Section/ShrubID,
               method = "ML")

  anova(null_model, C2_model)
  
  summary(C3_model)
  

# Climate models with mean summer temperature & mean summer percipitation model
C1C2_model = lme(resid ~ iem.summ.temp + iem.summ.rain, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

anova(C1_model, C1C2_model)

anova(C2_model, C1C2_model)

summary(C1C2_model)

# Climate model with an interaction term between summer temperature & mean summer percipitation model
  #This model is used to assess wether growth is greater in years when it is both warmer and wetter
  #We see that as mean precipitation increases, shrub growth increases more for every degree c increase

C1xC2_model = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain , data = sd_final_cch, random = ~ 1|Section/ShrubID,
                   method = "ML")

anova(C1C2_model, C1xC2_model)

summary(C1xC2_model)


#Plots the Satadardize Residuals plot for the core climate model
plot(C1xC2_model)

sjPlot :: plot_model(C1xC2_model, axis.labels=c("Interaction", "Mean Summer Pecip", "Mean Summer Temp"),
                     show.values=TRUE, show.p=TRUE,
                     title="Effect of Mean Summer Temp & Precip on Shrub Growth")

sjPlot::tab_model(C1xC2_model, show.re.var= TRUE, 
                  pred.labels =c("(Intercept)", "Mean Summer Temperature", "Mean Summer Precipitation", "Interaction MST & MSP"),
                  dv.labels= "Effects of Summer Temperature & Precipitation on Shrub Growth")

# Optimal Climate Model = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain , data = sd_final_cch, random = ~ 1|Section/ShrubID, method = "ML")


# HERBIVORE MODELS#### 

# Temporal Moose Density model
H1_model <- lme(resid ~ MooseDensity, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

  anova(null_model, H1_model)
  
  summary(H1_model)

# Temporal Hare Index, 1-3 scale representing different amplitudes of hare population peaks
H2_model <- lme(resid ~ HareIndex, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

  anova(null_model, H2_model)
  
  summary(H2_model)

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

# Herbivore model with temporal moose & hare index data
H2H1_model <- lme(resid ~  HareIndex + MooseDensity, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

  anova(H2_model, H2H1_model)

  summary(H2H1_model)


# Herbivore model with spatial browsing intensity by moose & hare index data
H2H3_model <- lme(resid ~ HareIndex + PropMoose, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")

  anova(H2_model, H2H3_model)
  
  summary(H2H3_model)
  
# Herbivore model with spatial browsing intensity by hare & hare index data
H2H4_model <- lme(resid ~ HareIndex + PropHare, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(H2_model, H2H4_model)
  
  summary(H2H4_model)
  
# Herbivore model with spatial browsing intensity by Ptarmagin & hare index data
H2H5_model <- lme(resid ~ HareIndex + PropPtarmagin, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(H2_model, H2H5_model)
  
  summary(H2H5_model)
  
# HERBIVORE MODELS SITE LEVEL #### 
  
# Spatial browsing pressure by Moose model 
H3_model_s <- lme(resid ~ PropMoose_S, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")
  
  anova(null_model, H3_model_s)
  
  summary(H3_model_s)
  
# Spatial browsing pressure by Hare model 
H4_model_s <- lme(resid ~ PropHare_S, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")
  
  anova(null_model, H4_model_s)
  
  summary(H4_model_s)
  
# Spatial browsing pressure by Ptarmagin model 
H5_model_s <- lme(resid ~ PropPtarmagin_S, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")
  
  anova(null_model, H5_model_s)
  
  summary(H5_model_s)
  


# CLIMATE & HERBIVORE MODELS ####

# Models the interactive effects of mean summer temperature and presepitiation and temporal moose density
CH1_model = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                  MooseDensity, 
                  data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(C1xC2_model, CH1_model)
  
  summary(CH1_model)  

# Models the interactive effects of mean summer temperature and presepitiation and temporal hare index
CH2_model = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                  HareIndex, 
                  data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")

  anova(C1xC2_model, CH2_model)
  
  anova(C1xC2_model)
  anova(CH2_model)
  
#Check model assumptions
  plot(CH2_model)
  
  qqnorm(resid(CH2_model))
  qqline(resid(CH2_model))
  
  sim.lme = simulate(C1xC2_model, nsim = 1000, m2 = CH2_model, method = "ML")
  plot(sim.lme)
  
  summary(CH2_model)
  
  sjPlot :: plot_model(CH2_model, axis.labels=c("Interaction MST & MSP", "Hare Index", "Mean Summer Pecip", "Mean Summer Temp"),
                       show.values=TRUE, show.p=TRUE,
                       title="Effect on Shrub Growth")
  
  sjPlot::tab_model(CH2_model, show.re.var= TRUE, 
                    pred.labels =c("(Intercept)", "Mean Summer Temperature", "Mean Summer Precipitation", "Interaction MST & MSP"),
                    dv.labels= "Effects of Summer Temperature & Precipitation on Shrub Growth")
  
  stargazer(CH2_model, type = "text")
  
# Models the interactive effects of mean summer temperature and presepitiation and temporal hare index
CH3_model = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                PropMoose, 
                data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")
  
  anova(C1xC2_model, CH3_model)
  
  summary(CH3_model)
  
# Models the interactive effects of mean summer temperature and presepitiation and temporal hare index
CH4_model = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                PropHare, 
                data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")
  
  anova(C1xC2_model, CH4_model)
  
  summary(CH4_model)
  
# Models the interactive effects of mean summer temperature and presepitiation and temporal hare index
CH5_model = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                PropPtarmagin, 
                data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")
  
  anova(C1xC2_model, CH5_model)
  
  summary(CH5_model)
  
# Models the interactive effects of mean summer temperature and presepitiation, temporal hare index and temporal moose density
CH1H2_model = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                  MooseDensity * HareIndex,
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
      

# LIKELIHOOD RATIO TEST BASED ON FORWARD AND BACKWARD SELECTION ####

# Forward stepwise regression for Betula 

add1(null_model_b, scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . + HareIndex), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose + PropHare + PropPtarmagin, test = "Chisq")

add1(update(null_model_b, ~ . + HareIndex + iem.summ.rain), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose + PropHare + PropPtarmagin, test = "Chisq")

add1(update(null_model_b, ~ . + HareIndex + iem.summ.rain + MooseDensity), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose + PropHare + PropPtarmagin, test = "Chisq")

summary(update(null_model_b, ~ . + HareIndex + iem.summ.rain + MooseDensity))

# Forward stepwise regression for Salix

add1(null_model_s, scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_s, ~ . + iem.summ.temp), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose + PropHare + PropPtarmagin, test = "Chisq")

add1(update(null_model_s, ~ . + iem.summ.temp + MooseDensity), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose + PropHare + PropPtarmagin, test = "Chisq")

add1(update(null_model_s, ~ . + iem.summ.temp + MooseDensity + iem.summ.rain), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose + PropHare + PropPtarmagin, test = "Chisq")

summary(update(null_model_s, ~ . + iem.summ.temp + MooseDensity + iem.summ.rain))


# Backward stepwise regression 

lme_full = lme(resid ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose + PropHare + PropPtarmagin, 
               data = sd_final_cch, random = ~ 1|Section/ShrubID,
               method = "ML")

summary(lme_full)

piecewiseSEM::rsquared(lme_full)

drop1(lme_full, test = "Chisq")

drop1(update(lme_full, ~ . -PropPtarmagin), test = "Chisq")

drop1(update(lme_full, ~ . -PropPtarmagin -PropHare), test = "Chisq")

drop1(update(lme_full, ~ . -PropPtarmagin -PropHare -MooseDensity), test = "Chisq")

drop1(update(lme_full, ~ . -PropPtarmagin -PropHare -MooseDensity -PropMoose), test = "Chisq")

summary(update(lme_full, ~ . -PropPtarmagin -PropHare -MooseDensity -PropMoose))

# AIC BASED FORWARD SELECTION ####

AIC_forward = stepAIC(null_model, direction = c("forward"), 
                      scope = (~ iem.summ.temp + iem.summ.rain 
                               + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S + ...))

# R2 BASED FORWARD MODEL SELECTION ####

y = select(sd_final_cch, resid)
Y = data.matrix(y)

x = y = select(sd_final_cch, iem.summ.temp, iem.summ.rain, 
               MooseDensity, HareIndex, PropMoose_S, PropHare_S, PropPtarmagin_S)
X = data.matrix(x)


forward.sel(Y, X, K = 7)

# PLOTS ####
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
