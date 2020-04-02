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
install.packages("olsrr")
install.packages("globaltest")
install.packages("coefplot")
install.packages("glmmTMB")
install.packages("forestplot")
install.packages("plotrix")

library(lme4)
library(nlme)
library(lmerTest)
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
library(olsrr)
library(nonnest2)
library(coefplot)
library(piecewiseSEM)
library(glmmTMB)
library(forestplot)
library(plotrix)

sessionInfo()
old.packages()
update.packages(ask = FALSE)
packageVersion("")
update.packages("")

# OPTIMAL MODEL - FORWARD SELECTION NO INTERACTIONS####
Fwd_Betula <- lme(resid ~ MooseDensity + iem.summ.temp + iem.summ.rain, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "REML")

summary(Fwd_Betula)

vif.lme(Fwd_Betula)

Fwd_Salix <- lme(resid ~ iem.summ.temp + MooseDensity + iem.summ.rain + HareIndex, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                 method = "REML")

summary(Fwd_Salix)

vif.lme(Fwd_Salix)

Fwd_Salix <- lme(resid ~ iem.summ.temp + MooseDensity + iem.summ.rain + HareIndex, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                 method = "RMEL")

# OPTIMAL MODEL - BACKWARD SELECTION NO INTERACTIONS####
Bkw_Betula <- lme(resid ~ MooseDensity + iem.summ.temp + iem.summ.rain, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "RMEL")

Bkw_Salix <- lme(resid ~ iem.summ.temp + MooseDensity + iem.summ.rain + HareIndex, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                 method = "RMEL")

vif.lme(Optimal_model_b)

# LIKELIHOOD RATIO TEST BASED ON FORWARD AND BACKWARD SELECTION ####

null_model_b = lme(resid ~ 1, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                   method = "ML")

null_model_b_S = lme(resid ~ 1, data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

summary(sd_salix_cch)

#Forward stepwise regression for Betula 

add1(null_model_b, scope = ~ iem.summ.temp + iem.summ.rain.10 + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . + MooseDensity), scope = ~ iem.summ.temp + iem.summ.rain.10 + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . + MooseDensity + iem.summ.temp), scope = ~ iem.summ.temp + iem.summ.rain.10 + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . + MooseDensity + iem.summ.temp + iem.summ.rain.10), scope = ~ iem.summ.temp + iem.summ.rain.10 + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

summary(update(null_model_b, ~ . + MooseDensity + iem.summ.temp + iem.summ.rain.10))


# Forward stepwise regression for Salix

add1(null_model_s_S, scope = ~ iem.summ.temp + iem.summ.rain.10 + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_s_S, ~ . + MooseDensity), scope = ~ iem.summ.temp + iem.summ.rain.10 + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_s_S, ~ . + MooseDensity + iem.summ.temp), scope = ~ iem.summ.temp + iem.summ.rain.10 + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_s_S, ~ . + MooseDensity + iem.summ.temp + HareIndex), scope = ~ iem.summ.temp + iem.summ.rain.10 + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_s_S, ~ . + MooseDensity + iem.summ.temp + HareIndex + iem.summ.rain.10), scope = ~ iem.summ.temp + iem.summ.rain.10 + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

summary(update(null_model_s, ~ . + MooseDensity + iem.summ.temp + HareIndex + iem.summ.rain.10))

# Backward stepwise regression Betula

lme_full_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S,
                 data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

summary(lme_full_b)

piecewiseSEM::rsquared(lme_full_b_S)

drop1(lme_full_b_S, test = "Chisq")

drop1(update(lme_full_b_S, ~ . -PropHare_S), test = "Chisq")

drop1(update(lme_full_b_S, ~ . -PropHare_S -PropMoose_S), test = "Chisq")

drop1(update(lme_full_b_S, ~ . -PropMoose_S -PropHare_S -PropPtarmagin_S), test = "Chisq")

drop1(update(lme_full_b_S, ~ . -PropMoose_S -PropHare_S -PropPtarmagin_S -HareIndex), test = "Chisq")

summary(update(lme_full_b_S, ~ . -PropMoose_S -PropHare_S -PropPtarmagin_S -HareIndex))

summary(lme(resid ~ MooseDensity + iem.summ.temp + iem.summ.rain.10, 
            data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
            method = "REML"))

# Backward stepwise regression for Salix

lme_full_s_S = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, 
               data = sd_salix_cch, random = ~ 1|Section/ShrubID,
               method = "ML")

summary(lme_full_s_S)

piecewiseSEM::rsquared(lme_full_s)

drop1(lme_full_s_S, test = "Chisq")

drop1(update(lme_full_s_S, ~ . -PropHare_S), test = "Chisq")

drop1(update(lme_full_s_S, ~ . -PropHare_S -PropMoose_S), test = "Chisq")

drop1(update(lme_full_s_S, ~ . -PropHare_S -PropMoose_S -PropPtarmagin_S), test = "Chisq")

summary(update(lme_full_s_S, ~ . -PropHare_S -PropMoose_S -PropPtarmagin_S))

summary(lme(resid ~ iem.summ.temp + iem.summ.rain.10 + MooseDensity + HareIndex, 
            data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
            method = "REML"))

# LIKELIHOOD RATIO TEST BASED ON FORWARD AND BACKWARD SELECTION ####

#Forward stepwise regression for Betula 

add1(null_model_b, scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . + MooseDensity), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . + MooseDensity + iem.summ.temp), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . + MooseDensity + iem.summ.temp + iem.summ.rain), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

summary(update(null_model_b, ~ . + MooseDensity + iem.summ.temp + iem.summ.rain))

# Backward stepwise regression Betula

lme_full_b = lme(resid ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S,
                 data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

summary(lme_full_b)

piecewiseSEM::rsquared(lme_full_b)

drop1(lme_full_b, test = "Chisq")

drop1(update(lme_full_b, ~ . -PropHare_S), test = "Chisq")

drop1(update(lme_full_b, ~ . -PropHare_S -PropMoose_S), test = "Chisq")

drop1(update(lme_full_b, ~ . -PropMoose_S -PropHare_S -PropPtarmagin_S), test = "Chisq")

drop1(update(lme_full_b, ~ . -PropMoose_S -PropHare_S -PropPtarmagin_S -HareIndex), test = "Chisq")

summary(update(lme_full_b, ~ . -PropMoose_S -PropHare_S -PropPtarmagin_S -HareIndex))

# Forward stepwise regression for Salix

add1(null_model_s, scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_s, ~ . + MooseDensity), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_s, ~ . + MooseDensity + iem.summ.temp), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_s, ~ . + MooseDensity + iem.summ.temp + HareIndex), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_s, ~ . + MooseDensity + iem.summ.temp + HareIndex + iem.summ.rain), scope = ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

summary(update(null_model_s, ~ . + MooseDensity + iem.summ.temp + HareIndex + iem.summ.rain))

# Backward stepwise regression for Salix

lme_full_s = lme(resid ~ iem.summ.temp + iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, 
               data = sd_salix_cch, random = ~ 1|Section/ShrubID,
               method = "ML")

summary(lme_full_s)

piecewiseSEM::rsquared(lme_full_s)

drop1(lme_full_s, test = "Chisq")

drop1(update(lme_full_s, ~ . -PropHare_S), test = "Chisq")

drop1(update(lme_full_s, ~ . -PropHare_S -PropMoose_S), test = "Chisq")

drop1(update(lme_full_s, ~ . -PropHare_S -PropMoose_S -PropPtarmagin_S), test = "Chisq")

summary(update(lme_full_s, ~ . -PropHare_S -PropMoose_S -PropPtarmagin_S))

# TESTING VARIABLE INTERACTIONS IN GLOBAL MODELS ####

add1(lme_full_b, scope = .~. + .^2, test= "Chisq")

add1(lme_full_s, scope = .~. + .^2, test= "Chisq")

# LIKELIHOOD RATIO TEST BASED WITH INTERACTION TERMS ####

#Forward stepwise regression for Betula 

add1(null_model_b, scope = ~ iem.summ.temp:iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . + iem.summ.temp:iem.summ.rain), scope = ~ iem.summ.temp:iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . +  iem.summ.temp:iem.summ.rain + HareIndex ), scope = ~ iem.summ.temp:iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . +  iem.summ.temp:iem.summ.rain + HareIndex + MooseDensity), scope = ~ iem.summ.temp:iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . + HareIndex), scope = ~ iem.summ.temp:iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . + HareIndex + iem.summ.temp:iem.summ.rain), scope = ~ iem.summ.temp:iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

add1(update(null_model_b, ~ . + HareIndex + iem.summ.temp:iem.summ.rain + MooseDensity), scope = ~ iem.summ.temp:iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S, test = "Chisq")

summary(update(null_model_b, ~ . + HareIndex + iem.summ.temp:iem.summ.rain + MooseDensity))

# Backward stepwise regression Betula

lme_full_I_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10
                   + MooseDensity + HareIndex 
                   + PropMoose_S + PropHare_S + PropPtarmagin_S)
                   #+ iem.summ.temp * iem.summ.rain.10
                   #+ iem.summ.temp * HareIndex
                   #+ iem.summ.temp * MooseDensity,

lme_full_I_b = lme(resid ~ iem.summ.temp:iem.summ.rain + MooseDensity + HareIndex + PropMoose_S + PropHare_S + PropPtarmagin_S,
                 data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

summary(lme_full_I_b)

piecewiseSEM::rsquared(lme_full_I_b)

drop1(lme_full_I_b, test = "Chisq")

drop1(update(lme_full_I_b, ~ . -PropHare_S), test = "Chisq")

drop1(update(lme_full_I_b, ~ . -PropHare_S -PropPtarmagin_S), test = "Chisq")

drop1(update(lme_full_I_b, ~ . -PropHare_S -PropPtarmagin_S -PropMoose_S), test = "Chisq")

drop1(update(lme_full_I_b, ~ . -PropHare_S -PropPtarmagin_S -PropMoose_S -HareIndex), test = "Chisq")

summary(update(lme_full_I_b, ~ . -PropHare_S -PropPtarmagin_S -PropMoose_S -HareIndex))

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

AIC_forward_b = stepAIC(null_model_b, direction = c("forward"), 
                      scope = (~ iem.summ.temp + iem.summ.rain.10 + 
                                 iem.summ.temp * iem.summ.rain.10 +
                                 iem.summ.temp * HareIndex +
                                 iem.summ.temp * MooseDensity +
                                 MooseDensity + HareIndex + ...))

summary(AIC_forward_b)


AIC_forward_s = stepAIC(null_model_s, direction = c("forward"), 
                      scope = (~ iem.summ.temp + iem.summ.rain.10 + 
                                 iem.summ.temp * iem.summ.rain.10 +
                                 iem.summ.temp * HareIndex +
                                 iem.summ.temp * MooseDensity +
                                 MooseDensity + HareIndex + ...))

summary(AIC_forward_s)

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
lme_global_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                     iem.summ.temp * iem.summ.rain.10 +
                     iem.summ.temp * HareIndex +
                     iem.summ.temp * MooseDensity +
                     MooseDensity + HareIndex,
                   data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML")


anova.lme(lme_global_b)

summary(lme_global_b)

intervals(lme_global_b)

r.squaredLR(lme_global_b)

plot(lme_global_b)

qqnorm(resid(lme_global_b))

qqline(resid(lme_global_b))

dredge_b = dredge(lme_global_b)

model.sel_b = model.sel(dredge_b, fit = TRUE)

write.table(model.sel_b, "/Users/peterfrank/Documents/Master's Thesis/DataAnalysis/AlaskaShrubs/R_Data/model.sel_b.txt", sep = "\t")

mod.avg_b = model.avg(model.sel_b)

importance(mod.avg_b)

pred_avg_b = predict(mod.avg_b)

summary(mod.avg_b)

summary(pred_avg_b)

plot(model.sel_b)



# Salix Model Averaging
lme_global_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                     iem.summ.temp * iem.summ.rain.10 +
                     iem.summ.temp * HareIndex +
                     iem.summ.temp * MooseDensity +
                     MooseDensity + HareIndex,
                     data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

summary(lme_global_s)

r.squaredLR(lme_global_s)

plot(lme_global_s)

qqnorm(resid(lme_global_s))

qqline(resid(lme_global_s))

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

vif.lme(Optimal_model_s)

vif.lme(Optimal_model_b)

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
