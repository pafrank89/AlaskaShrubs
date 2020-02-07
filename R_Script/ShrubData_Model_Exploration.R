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
install.packages("MuMIn")

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
library(MuMIn)

sessionInfo()
old.packages()
update.packages(ask = FALSE)
packageVersion("")
update.packages("")

# OPTIMAL MODEL - FORWARD SELECTION NO INTERACTIONS####
Fwd_Betula <- lme(resid ~ HareIndex + iem.summ.rain + MooseDensity, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "RMEL")

Fwd_Salix <- lme(resid ~ iem.summ.temp + MooseDensity + iem.summ.rain, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                 method = "RMEL")

# OPTIMAL MODEL - BACKWARD SELECTION NO INTERACTIONS####
Bkw_Betula <- lme(resid ~ HareIndex + iem.summ.rain + MooseDensity, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "RMEL")

Bkw_Salix <- lme(resid ~ iem.summ.temp + MooseDensity + iem.summ.rain, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                 method = "RMEL")

# OPTIMAL MODEL - FORWARD SELECTION WITH CLIMATE INTERACTION####
Fwd_Int_Betula <- lme(resid ~ HareIndex + iem.summ.temp:iem.summ.rain + MooseDensity, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "RMEL")

Fwd_Int_Salix <- lme(resid ~ iem.summ.temp:iem.summ.rain, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                 method = "RMEL")

# OPTIMAL MODEL - BACKWARD SELECTION WITH CLIMATE INTERACTION####
Bkw_Betula <- lme(resid ~ HareIndex + iem.summ.temp:iem.summ.rain + MooseDensity, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "RMEL")

Bkw_Salix <- lme(resid ~ iem.summ.temp:iem.summ.rain, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                 method = "RMEL")


# LIKELIHOOD RATIO TEST BASED ON FORWARD AND BACKWARD SELECTION ####

#Forward stepwise regression for Betula 

add1(null_model_b, scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . + HareIndex), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . + HareIndex + iem.summ.rain), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . + HareIndex + iem.summ.rain + MooseDensity), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

summary(update(null_model_b, ~ . + HareIndex + iem.summ.rain + MooseDensity))

# Backward stepwise regression Betula

lme_full_b = lme(resid ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S +
                   iem.summ.temp:iem.summ.rain, 
                 data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

summary(lme_full_b)

piecewiseSEM::rsquared(lme_full_b)

drop1(lme_full_b, test = "Chisq")

drop1(update(lme_full_b, ~ . -PropHare_S), test = "Chisq")

drop1(update(lme_full_b, ~ . -PropHare_S -PropMoose_S), test = "Chisq")

drop1(update(lme_full_b, ~ . -PropMoose_S -PropHare_S -PropPtarmagin_S), test = "Chisq")

drop1(update(lme_full_b, ~ . -PropMoose_S -PropHare_S -PropPtarmagin_S -iem.summ.temp), test = "Chisq")

summary(update(lme_full_b, ~ . -PropMoose_S -PropHare_S -PropPtarmagin_S -iem.summ.temp))

# Forward stepwise regression for Salix

add1(null_model_s, scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_s, ~ . + iem.summ.temp), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_s, ~ . + iem.summ.temp + MooseDensity), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_s, ~ . + iem.summ.temp + MooseDensity + iem.summ.rain), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

summary(update(null_model_s, ~ . + iem.summ.temp + MooseDensity + iem.summ.rain))

# Backward stepwise regression for Salix

lme_full_s = lme(resid ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, 
               data = sd_salix_cch, random = ~ 1|Section/ShrubID,
               method = "ML")

summary(lme_full_s)

piecewiseSEM::rsquared(lme_full_s)

drop1(lme_full_s, test = "Chisq")

drop1(update(lme_full_s, ~ . -PropMoose_S), test = "Chisq")

drop1(update(lme_full_s, ~ . -PropMoose_S -PropHare_S), test = "Chisq")

drop1(update(lme_full_s, ~ . -PropMoose_S -PropHare_S -PropPtarmagin_S), test = "Chisq")

drop1(update(lme_full_s, ~ . -PropMoose_S -PropHare_S -PropPtarmagin_S -HareIndex), test = "Chisq")

summary(update(lme_full_s, ~ . -PropMoose_S -PropHare_S -PropPtarmagin_S -HareIndex))


# TESTING VARIABLE INTERACTIONS IN GLOBAL MODELS ####

add1(lme_full_b, scope = .~. + .^2, test= "Chisq")

add1(lme_full_s, scope = .~. + .^2, test= "Chisq")
# LIKELIHOOD RATIO TEST BASED WITH INTERACTION TERMS ####

#Forward stepwise regression for Betula 

add1(null_model_b, scope = ~ iem.summ.temp:iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . + HareIndex), scope = ~ iem.summ.temp:iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . + HareIndex + iem.summ.temp:iem.summ.rain), scope = ~ iem.summ.temp:iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . + HareIndex + iem.summ.temp:iem.summ.rain + MooseDensity), scope = ~ iem.summ.temp:iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

summary(update(null_model_b, ~ . + HareIndex + iem.summ.temp:iem.summ.rain + MooseDensity))

# Backward stepwise regression Betula

lme_full_I_b = lme(resid ~ iem.summ.temp:iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S,
                 data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

summary(lme_full_I_b)

piecewiseSEM::rsquared(lme_full_I_b)

drop1(lme_full_I_b, test = "Chisq")

drop1(update(lme_full_I_b, ~ . -PropHare_S), test = "Chisq")

drop1(update(lme_full_I_b, ~ . -PropHare_S -PropPtarmagin_S), test = "Chisq")

drop1(update(lme_full_I_b, ~ . -PropHare_S -PropPtarmagin_S -PropMoose_S), test = "Chisq")

summary(update(lme_full_I_b, ~ . -PropHare_S -PropPtarmagin_S -PropMoose_S))

# Forward stepwise regression for Salix

add1(null_model_s, scope = ~ iem.summ.temp:iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_s, ~ . + iem.summ.temp:iem.summ.rain), scope = ~ iem.summ.temp:iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

summary(update(null_model_s, ~ . + iem.summ.temp:iem.summ.rain))

# Backward stepwise regression for Salix

lme_full_I_s = lme(resid ~ iem.summ.temp:iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, 
                 data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

summary(lme_full_I_s)

piecewiseSEM::rsquared(lme_full_I_s)

drop1(lme_full_I_s, test = "Chisq")

drop1(update(lme_full_I_s, ~ . -PropMoose_S), test = "Chisq")

drop1(update(lme_full_I_s, ~ . -PropMoose_S -PropPtarmagin_S), test = "Chisq")

drop1(update(lme_full_I_s, ~ . -PropMoose_S -PropPtarmagin_S -PropHare_S), test = "Chisq")

drop1(update(lme_full_I_s, ~ . -PropMoose_S -PropPtarmagin_S -PropHare_S -HareIndex), test = "Chisq")

drop1(update(lme_full_I_s, ~ . -PropMoose_S -PropPtarmagin_S -PropHare_S -HareIndex -MooseDensity), test = "Chisq")

summary(update(lme_full_I_s, ~ . -PropMoose_S -PropHare_S -PropPtarmagin_S -HareIndex -MooseDensity))

# AIC BASED FORWARD SELECTION ####

AIC_forward = stepAIC(null_model_b, direction = c("forward"), 
                      scope = (~ iem.summ.temp + iem.summ.rain +
                                 iem.summ.temp * iem.summ.rain +
                                 iem.summ.temp * HareIndex +
                                 iem.summ.rain * HareIndex +
                                 MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S + ...))

AIC_forward = stepAIC(null_model_s, direction = c("forward"), 
                      scope = (~ iem.summ.temp + iem.summ.rain +
                                 iem.summ.temp * iem.summ.rain +
                                 iem.summ.temp * PropPtarmagin_S +
                                 iem.summ.rain * PropPtarmagin_S +
                                 MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S + ...))

# MODEL AVERAGING FROM GLOBAL MODEL ####

# Betula Model Averaging
lme_global_b = lme(resid ~ iem.summ.temp + iem.summ.rain +
                  iem.summ.temp * iem.summ.rain +
                  iem.summ.temp * HareIndex +
                  iem.summ.rain * HareIndex +
                  MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S,
                  data = sd_bena_cch, random = ~ 1|Section/ShrubID, method = "ML")

dredge_b = dredge(lme_global_b)

model.sel_b = model.sel(dredge_b, fit = TRUE)

write.table(model.sel_b, "/Users/peterfrank/Documents/Master's Thesis/DataAnalysis/AlaskaShrubs/R_Data/model.sel_b.txt", sep = "\t")

mod.avg_b = model.avg(model.sel_b)

importance(mod.avg_b)

pred_avg_b = predict(mod.avg_b)

summary(mod.avg_b)

summary(pred_avg_b)

# Salix Model Averaging
lme_global_s = lme(resid ~ iem.summ.temp + iem.summ.rain +
                     #iem.summ.temp * iem.summ.rain +
                     #iem.summ.temp * PropPtarmagin_S +
                     #iem.summ.rain * PropPtarmagin_S +
                     MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S,
                     data = sd_salix_cch, random = ~ 1|Section/ShrubID, method = "ML")

dredge_s = dredge(lme_global_s)

model.sel_s = model.sel(dredge_s)

write.table(model.sel_s, "/Users/peterfrank/Documents/Master's Thesis/DataAnalysis/AlaskaShrubs/R_Data/model.sel_s.txt", sep = "\t")

mod.avg_s = model.avg(model.sel_s)

importance(mod.avg_s)

summary(mod.avg_s)


# R2 BASED FORWARD MODEL SELECTION ####

y = select(sd_final_cch, resid)
Y = data.matrix(y)

x = y = select(sd_final_cch, iem.summ.temp, iem.summ.rain, 
               MooseDensity, HareIndex, PropMoose_S, PropHare_S, PropPtarmagin_S)
X = data.matrix(x)


forward.sel(Y, X, K = 7)

# CALCULATE VARIANCE INFLATION FACTOR (VIF) FOR BEST MODEL ####

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

#Calculate the VIFs for the model without climatic interaction terms 
#Note that values for VIF greater than two should be investigated further

vif.lme(CH1H2_model_b)

vif.lme(CH1_model_s)

# PLOTS ####

par(mfrow=c(4,1))
    
ggplot(data = sd_bena_cch, 
       aes(x = resid,
           y = HareIndex)) + 
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
  xlab("Residuals") + ylab("Moose Density (moose/km²)") 

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
