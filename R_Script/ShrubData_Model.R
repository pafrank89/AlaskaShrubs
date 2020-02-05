# Modeling the effects of Climate and Herbivory on Shrub Growth
# Peter Frank 
# peterfr@stud.ntnu.no
# 2020-02-03

# Our objective is to test the following four hypotheses:

  #H1 - Increased temperature and precipitation will have a positive effect on radial growth 
  #H2 - Increased herbivore density will have a negative effect on radial growth 
  #H3 - Shrub growth positivly effected by the interactive effects of herbivory and temperature 
  #H4 - The effects of herbivory will be less evident in well defended Betula species when compared to more palatable Salix species

# This code will utilize forward model selection based on liklihood ratio tests to determine the optimal mixed effects model to descripe the variance in 
# the detrended BAI data (resid). This process will begin by fitting a climate model and expanding upon that model to include herbivory factors.
# We utilize this apporach because our a priori expectation is that shrub growth is driven primarily by climatic variables. 

# 1. ASSESS VARIANCE IN THE RESPONSE VARIABLE ####

# Assess the residual BAI response variable so you can interperet the effect size

#All Data
mean(sd_final_cch$resid) # = 0.1251182
min(sd_final_cch$resid) # = -3.69711
max(sd_final_cch$resid) # = 4.042966

hist(sd_final_cch$resid, xlab = "Residuals", main = "All Data")

#Betula
mean(sd_bena_cch$resid) # = 0.1398543
min(sd_bena_cch$resid) # = -3.6971
max(sd_bena_cch$resid) # = 4.042966

hist(sd_bena_cch$resid, xlab = "Residuals", main = "Betula")

#Salix
mean(sd_salix_cch$resid) # = 0.09951921
min(sd_salix_cch$resid) # = -3.6971
max(sd_salix_cch$resid) # = 3.935671

hist(sd_salix_cch$resid, xlab = "Residuals", main = "Salix")

# Assess the range of resid values across sites, to show the need for random effects
boxplot(sd_final_cch$resid ~ sd_final_cch$Section, 
        xlab = "Site", ylab = "Residuals", main = "All Data")

boxplot(sd_bena_cch$resid ~ sd_bena_cch$Section, 
        xlab = "Site", ylab = "Residuals", main = "Betula")

boxplot(sd_salix_cch$resid ~ sd_salix_cch$Section, 
        xlab = "Site", ylab = "Residuals", main = "Salix")

# Review the fixed effects which will be used in the models and insure no NA values are present 
summary(sd_final_cch)

summary(sd_bena_cch)

summary(sd_salix_cch)

# Model Variables Key

str(sd_final_cch)

str(sd_bena_cch)

str(sd_salix_cch)

#Dependent/Response Variable:
# resid

#Random Effects:
# ~1 | Section/Shrub ID
# This indicates that shrub is nested within section

#Fixed Effects/Explanatory Variables
#C1 = iem.summ.temp
#C2 = iem.summ.rain.10

#H1 = MooseDensity
#H2 = HareIndex
#H3 = PropMoose
#H4 = PropHare
#H5 = PropPtarmagin

#X1 = Elevation
#X2 = Slope
#X3 = Y_Cord
#X4 = DistToRoad



# 2. NULL MODELS #### 

# Null models will be used to compare the effect of adding fixed effects to the model 

null_model = lme(resid ~ 1, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

null_model_b = lme(resid ~ 1, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

null_model_s = lme(resid ~ 1, data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

# 3. CLIMATE MODELS #### 

# Climate sensitivy is expected to be the primaty driver of shrub growth in this region, this we will begin forward selection by
# Determining the optimal climate model for the data. 

# Optimal Models:

  # All Data: C1xC2_model
  # Betula: C1xC2_model_b
  # Salix: C1xC2_model_s

## 3.1 Mean Summer Temperature #####

# MST for All Data
C1_model = lme(resid ~ iem.summ.temp, data = sd_final_cch, random = ~ 1|Section/ShrubID,
               method = "ML")

  anova(null_model, C1_model)
  
  summary(C1_model)

# MST for Betula
C1_model_b = lme(resid ~ iem.summ.temp, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
               method = "ML")

  anova(null_model_b, C1_model_b)
  
  summary(C1_model_b)

# MST for Salix
C1_model_s = lme(resid ~ iem.summ.temp, data = sd_salix_cch, random = ~ 1|Section/ShrubID,
               method = "ML")

  anova(null_model_s, C1_model_s)
  
  summary(C1_model_s)


## 3.2 Mean Summer Precipitation  ####

# MSP for All Data
C2_model = lme(resid ~ iem.summ.rain.10, data = sd_final_cch, random = ~ 1|Section/ShrubID,
               method = "ML")

  anova(null_model, C2_model)
  
  summary(C2_model)

# MSP for Betula 
C2_model_b = lme(resid ~ iem.summ.rain.10, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
               method = "ML")

  anova(null_model_b, C2_model_b)
  
  summary(C2_model_b)

# MSP for Salix 
C2_model_s = lme(resid ~ iem.summ.rain.10, data = sd_salix_cch, random = ~ 1|Section/ShrubID,
               method = "ML")

  anova(null_model_s, C2_model_s)
  
  summary(C2_model_s)


## 3.3 Addative Effect MST + MSP ####

# MST + MSP for All Data
C1C2_model = lme(resid ~ iem.summ.temp + iem.summ.rain.10, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

  anova(C1_model, C1C2_model)
  
  anova(C2_model, C1C2_model)
  
  summary(C1C2_model)

# MST + MSP for Betula
C1C2_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

  anova(C1_model_b, C1C2_model_b)
  
  anova(C2_model_b, C1C2_model_b)
  
  anova(null_model_b, C1C2_model_b)
  
  summary(C1C2_model_b)

# MST + MSP for Salix
C1C2_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10, data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                 method = "ML")

anova(C1_model_s, C1C2_model_s)

anova(C2_model_s, C1C2_model_s)

anova(null_model_s, C1C2_model_s)

summary(C1C2_model_s)

## 3.3 Interactive Effect MST * MSP ####

#This model is used to assess wether growth is greater in years when it is both warmer and wetter
#We see that as mean precipitation increases, shrub growth increases more for every degree c increase

# MST * MSP for All Data
C1xC2_model = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 , data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")

  anova(C1C2_model, C1xC2_model)
  
  summary(C1xC2_model)

# MST * MSP for Betula
C1xC2_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 , data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")

  anova(C1C2_model_b, C1xC2_model_b)
  
  anova(C2_model_b, C1xC2_model_b)
  
  anova(null_model_b, C1xC2_model_b)
  
  summary(C1xC2_model_b)

# MST * MSP for Salix
C1xC2_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 , data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")

  anova(C1C2_model_s, C1xC2_model_s)
  
  anova(null_model_s, C1xC2_model_s)
  
  summary(C1xC2_model_s)


## 3.4 Model Assumptions & Effect Size for Climate Model ####

#Plots the Satadardize Residuals plot for the core climate model

### All Data
# Plot Residuals vs. Fitted Plot to check for non-linearity, unequal error variances, and outliers
plot(C1xC2_model)

# Plot Normal QQ ti assess the distribution of the data
qqnorm(resid(C1xC2_model))
qqline(resid(C1xC2_model))

#Plot the effect size of the model
sjPlot :: plot_model(C1xC2_model, axis.labels=c("Interaction", "Mean Summer Pecip", "Mean Summer Temp"),
                     show.values=TRUE, show.p=TRUE,
                     title="Effect of Mean Summer Temp & Precip on Shrub Growth")

sjPlot::tab_model(C1xC2_model, show.re.var= TRUE, 
                  pred.labels =c("(Intercept)", "Mean Summer Temperature", "Mean Summer Precipitation", "Interaction MST & MSP"),
                  dv.labels= "Effects of Summer Temperature & Precipitation on Shrub Growth")

### Betula 
# Plot Residuals vs. Fitted Plot to check for non-linearity, unequal error variances, and outliers
plot(C1xC2_model_b)

# Plot Normal QQ ti assess the distribution of the data
qqnorm(resid(C1xC2_model_b))
qqline(resid(C1xC2_model_b))

#Plot the effect size of the model
sjPlot :: plot_model(C1xC2_model_b, axis.labels=c("Interaction", "Mean Summer Pecip", "Mean Summer Temp"),
                     show.values=TRUE, show.p=TRUE,
                     title="Effect of Mean Summer Temp & Precip on Shrub Growth")

sjPlot::tab_model(C1xC2_model_b, show.re.var= TRUE, 
                  pred.labels =c("(Intercept)", "Mean Summer Temperature", "Mean Summer Precipitation", "Interaction MST & MSP"),
                  dv.labels= "Effects of Summer Temperature & Precipitation on Shrub Growth")

### Salix
# Plot Residuals vs. Fitted Plot to check for non-linearity, unequal error variances, and outliers
plot(C1xC2_model_s)

# Plot Normal QQ ti assess the distribution of the data
qqnorm(resid(C1xC2_model_s))
qqline(resid(C1xC2_model_s))

#Plot the effect size of the model
sjPlot :: plot_model(C1xC2_model_s, axis.labels=c("Interaction", "Mean Summer Pecip", "Mean Summer Temp"),
                     show.values=TRUE, show.p=TRUE,
                     title="Effect of Mean Summer Temp & Precip on Shrub Growth")

sjPlot::tab_model(C1xC2_model_s, show.re.var= TRUE, 
                  pred.labels =c("(Intercept)", "Mean Summer Temperature", "Mean Summer Precipitation", "Interaction MST & MSP"),
                  dv.labels= "Effects of Summer Temperature & Precipitation on Shrub Growth")

# Optimal Climate Model = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 , data = sd_final_cch, random = ~ 1|Section/ShrubID, method = "ML")


# 4. HERBIVORY MODELS ####

# We will assess the herbivore variables by comparing them one at a time to the null model
# and retaining factors which significantly improve the fit of the model.

# Optimal Models:

  # All Data: H2_model
  # Betula: H2_model_b
  # Salix: No herbivory variables perform better than the NULL model
  
## 4.1 Temporal Moose Density model ####

# All Data
H1_model <- lme(resid ~ MooseDensity, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

  anova(null_model, H1_model)
  
  summary(H1_model)

# Betula
H1_model_b <- lme(resid ~ MooseDensity, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

  anova(null_model_b, H1_model_b)
  
  summary(H1_model_b)

# Salix
H1_model_s <- lme(resid ~ MooseDensity, data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

  anova(null_model_s, H1_model_s)
  
  summary(H1_model_s)

## 4.2 Temporal Hare Index ####
# This factor is based on a 1-3 scale representing different amplitudes of hare population peaks

# All Data
H2_model <- lme(resid ~ HareIndex, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

  anova(null_model, H2_model)
  
  summary(H2_model)

# Betula
H2_model_b <- lme(resid ~ HareIndex, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

  anova(null_model_b, H2_model_b)
  
  summary(H2_model_b)

# Salix
H2_model_s <- lme(resid ~ HareIndex, data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

  anova(null_model_s, H2_model_s)
  
  summary(H2_model_s)

## 4.3 Spatial browsing pressure by Moose ####

# All Data
H3_model <- lme(resid ~ PropMoose, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

  anova(null_model, H3_model)
  
  summary(H3_model)
  
# All Data Section
H3S_model <- lme(resid ~ PropMoose_S, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(null_model, H3S_model)
  
  summary(H3S_model)
  
  
# Betula
H3_model_b <- lme(resid ~ PropMoose, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(null_model_b, H3_model_b)
  
  summary(H3_model_b)
  
# Betula Section
H3S_model_b <- lme(resid ~ PropMoose_S, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(null_model_b, H3S_model_b)
  
  summary(H3S_model_b)
  
# Salix
H3_model_s <- lme(resid ~ PropMoose, data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(null_model_s, H3_model_s)
  
  summary(H3_model_s)
  
# Salix Section
H3S_model_s <- lme(resid ~ PropMoose_S, data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(null_model_s, H3S_model_s)
  
  summary(H3S_model_s)

## 4.4 Spatial browsing pressure by Hare #### 

# All Data
H4_model <- lme(resid ~ PropHare, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

  anova(null_model, H4_model)
  
  summary(H4_model)
  
# All Data Section
H4S_model <- lme(resid ~ PropHare_S, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(null_model, H4_model)
  
  summary(H4_model)
  
# Betula
H4_model_b <- lme(resid ~ PropHare, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(null_model_b, H4_model_b)
  
  summary(H4_model_b)
  
# Betula Section
H4S_model_b <- lme(resid ~ PropHare_S, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(null_model_b, H4S_model_b)
  
  summary(H4S_model_b)
  
# Salix
H4_model_s <- lme(resid ~ PropHare, data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(null_model_s, H4_model_s)
  
  summary(H4_model_s)
  
# Salix Section
H4S_model_s <- lme(resid ~ PropHare_S, data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(null_model_s, H4S_model_s)
  
  summary(H4S_model_s)

## 4.5 Spatial browsing pressure by Ptarmagin #### 

# All Data 
H5_model <- lme(resid ~ PropPtarmagin, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

  anova(null_model, H5_model)
  
  summary(H5_model)
  
# All Data Section 
H5S_model <- lme(resid ~ PropPtarmagin_S, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(null_model, H5_model)
  
  summary(H5_model)

# Betula
H5_model_b <- lme(resid ~ PropPtarmagin, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(null_model_b, H5_model_b)
  
  summary(H5_model_b)
  
# Betula Section
H5S_model_b <- lme(resid ~ PropPtarmagin_S, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(null_model_b, H5S_model_b)
  
  summary(H5S_model_b)
  
# Salix 
H5_model_s <- lme(resid ~ PropPtarmagin, data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(null_model_s, H5_model_s)
  
  summary(H5_model_s)
  
# Salix Section
H5S_model_s <- lme(resid ~ PropPtarmagin_S, data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(null_model_s, H5S_model_s)
  
  summary(H5S_model_s)

## 4.6 Herbivore model with temporal moose & hare index data ####

# All Data
H2H1_model <- lme(resid ~  HareIndex + MooseDensity, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")

  anova(H2_model, H2H1_model)
  
  summary(H2H1_model)

# Betula
H2H1_model_b <- lme(resid ~  HareIndex + MooseDensity, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")

  anova(H2_model_b, H2H1_model_b)
  
  summary(H2H1_model_b)
  
  
H2xH1_model_b <- lme(resid ~  HareIndex * MooseDensity, data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                      method = "ML")
  
  anova(H2_model_b, H2xH1_model_b)
  
  anova(H2H1_model_b, H2xH1_model_b)
  
  summary(H2xH1_model_b)
  

# Salix
H2H1_model_s <- lme(resid ~  HareIndex + MooseDensity, data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")

  anova(null_model_s, H2H1_model_s)
  
  summary(H2H1_model_s)

H2xH1_model_s <- lme(resid ~  HareIndex * MooseDensity, data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                     method = "ML")
  
  anova(null_model_s, H2xH1_model_s)
  
  summary(H2xH1_model_s)

# 5. HERBIVORY & CLIMATE MODELS ####
  
# We will further the model selection by adding the herbivory factors one at a time to the optimal climate model
# and retaining factors which significantly improve the fit of the model.
  
  
# Optimal Models:
  
  # All Data: CH2_model
  # Betula: CH1H2_model_b 
  # Salix: CH1_model_s
  
## 5.1 Climate & Moose Density ####

# All Data
CH1_model = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                MooseDensity, 
                data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")
  
  anova(C1xC2_model, CH1_model)
  
  summary(CH1_model)  
  
# Betula
CH1_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                MooseDensity, 
                data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                method = "ML")
  
  anova(C1xC2_model_b, CH1_model_b)
  
  summary(CH1_model_b) 
  
  
# Salix
CH1_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                MooseDensity, 
                data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                method = "ML")
  
  anova(C1xC2_model_s, CH1_model_s)
  
  summary(CH1_model_s) 
  
## 5.2 Climate & Hare Density ####

# ALl Data
CH2_model = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                HareIndex, 
                data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")
  
  anova(C1xC2_model, CH2_model)
  
  summary(C1xC2_model)
  
# Betula
CH2_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                HareIndex, 
                data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                method = "ML")
  
  anova(C1xC2_model_b, CH2_model_b)
  
  summary(CH2_model_b)
  
# Salix
CH2_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                HareIndex, 
                data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                method = "ML")
  
  anova(C1xC2_model_s, CH2_model_s)
  
  summary(CH2_model_s)
  
  ## 5.3.1 Climate & Moose Browse ####
  
  # ALl Data
  CH3_model = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                    PropMoose_S, 
                  data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(C1xC2_model, CH3_model)
  
  summary(CH3_model)
  
  # Betula
  CH3_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                      PropMoose_S, 
                    data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(C1xC2_model_b, CH3_model_b)
  
  summary(CH3_model_b)
  
  # Salix
  CH3_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                      PropMoose_S, 
                    data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(C1xC2_model_s, CH3_model_s)
  
  summary(CH3_model_s)
  
  ## 5.3.2 Climate & Hare Browse ####
  
  # ALl Data
  CH4_model = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                    PropHare_S, 
                  data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(C1xC2_model, CH4_model)
  
  summary(CH4_model)
  
  # Betula
  CH4_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                      PropHare_S, 
                    data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(C1xC2_model_b, CH4_model_b)
  
  summary(CH4_model_b)
  
  # Salix
  CH4_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                      PropHare_S, 
                    data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(C1xC2_model_s, CH4_model_s)
  
  summary(CH4_model_s)
  
  ## 5.3.3 Climate & Patamigin Browse ####
  
  # ALl Data
  CH5_model = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                    PropPtarmagin_S, 
                  data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(C1xC2_model, CH5_model)
  
  summary(CH5_model)
  
  # Betula
  CH5_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                      PropPtarmagin_S, 
                    data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(C1xC2_model_b, CH5_model_b)
  
  summary(CH5_model_b)
  
  # Salix
  CH5_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                      PropPtarmagin_S, 
                    data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(C1xC2_model_s, CH5_model_s)
  
  summary(CH5_model_s)
  
## 5.4 Climate & Moose Density + Hare Index ####
  
# All Data  
CH1H2_model = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                  MooseDensity + HareIndex,
                  data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(CH2_model, CH1H2_model)
  
  summary(CH1H2_model)
  
# Betula  
CH1H2_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                    MooseDensity + HareIndex,
                    data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(CH2_model_b, CH1H2_model_b)
  
  summary(CH1H2_model_b)
  
  
# Salix 
CH1H2_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                    MooseDensity + HareIndex,
                    data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(CH1_model_s, CH1H2_model_s)
  
  summary(CH1H2_model_s)
  
## 5.5 Climate & Moose Browse + Hare Index ####
  
# All Data  
CH3H2_model = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                  PropMoose_S + HareIndex,
                  data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(CH2_model, CH1H2_model)
  
  summary(CH1H2_model)
  
# Betula  
CH3H2_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                    PropMoose_S + HareIndex,
                    data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(CH2_model_b, CH3H2_model_b)
  
  summary(CH3H2_model_b)
  
  
# Salix 
CH3H2_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                    PropMoose_S + HareIndex,
                    data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(CH1_model_s, CH1H2_model_s)
  
  summary(CH1H2_model_s)  
  
## 5.6 Climate & Moose Density * Hare Index ####

# All Data  
CH1xH2_model = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                  MooseDensity * HareIndex,
                  data = sd_final_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(CH2_model, CH1xH2_model)
    
  summary(CH1xH2_model)
  
# Betula  
CH1XH2_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                  MooseDensity * HareIndex,
                  data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(CH2_model_b, CH1XH2_model_b)
  anova(CH1H2_model_b, CH1XH2_model_b)
  
  
  summary(CH1XH2_model_b)
  
  
# Salix 
CH1xH2_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                  MooseDensity * HareIndex,
                  data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                  method = "ML")
  
  anova(CH1_model_s, CH1xH2_model_s)
  
  summary(CH1xH2_model_s)
  
# 6. INTERACTION OF CLIMATE AND HERBIVORY ####

## To test if these is an interaction between climate and herbivory

## 6.1 MooseDensity : Mean Summer Temperature ####
  
# Betula  
CH_INT_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                     MooseDensity + HareIndex +
                     iem.summ.temp:MooseDensity,
                     data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                     method = "ML")
  
  anova(CH1H2_model_b, CH_INT_model_b)
  
  
  summary(CH_INT_model_b)
  
  
# Salix 
CH_INT_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                     MooseDensity + HareIndex +
                     iem.summ.temp:MooseDensity,
                     data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                     method = "ML")
  
  anova(CH1_model_s, CH_INT_model_s)
  
  summary(CH_INT_model_s)

  
## 6.2 Hare Index : Mean Summer Temperature ####
  
# Betula  
CH_INT2_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                    MooseDensity + HareIndex +
                    iem.summ.temp:HareIndex,
                    data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(CH1H2_model_b, CH_INT2_model_b)
  
  
  summary(CH1XH2_model_b)
  
  
# Salix 
CH_INT2_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                     MooseDensity + HareIndex +
                     iem.summ.temp:HareIndex,
                     data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                     method = "ML")
  
  anova(CH1_model_s, CH_INT2_model_s)
  
  summary(CH_INT2_model_s)
  
  

  
  
  
  
  
  
## 6.3 MooseDensity : Mean Mean Summer Precipitation ####
  
# Betula  
CH_INT3_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                    MooseDensity + HareIndex +
                    iem.summ.rain.10:MooseDensity,
                    data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(CH1H2_model_b, CH_INT3_model_b)
  
  
  summary(CH_INT3_model_b)
  
  
# Salix 
CH_INT3_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                    MooseDensity + HareIndex +
                      iem.summ.rain.10:MooseDensity,
                    data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                    method = "ML")
  
  anova(CH1H2_model_s, CH_INT3_model_s)
  
  summary(CH_INT3_model_s)
  
  
## 6.4 Hare Index : Mean Summer Precipitation ####
  
# Betula  
CH_INT4_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                      MooseDensity + HareIndex +
                      iem.summ.rain.10:HareIndex,
                      data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                      method = "ML")
  
  anova(CH1H2_model_b, CH_INT4_model_b)
  
  
  summary(CH_INT4_model_b)
  
  
# Salix 
CH_INT4_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                      MooseDensity + HareIndex +
                      iem.summ.rain.10:HareIndex,
                      data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                      method = "ML")
  
  anova(CH1_model_s, CH_INT4_model_s)
  
  summary(CH_INT4_model_s)
  
  
  
  
  
  
  
  
  
  

  
  
## 6.5 MooseDensity : Mean Mean Summer Precipitation & Hare Index : Mean Summer Temperature ####
  
# Betula  
CH_INT5_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                      MooseDensity + HareIndex +
                      iem.summ.rain.10:MooseDensity +
                      iem.summ.temp:HareIndex,
                      data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                      method = "ML")
  
  anova(CH_INT3_model_b, CH_INT5_model_b)
  
  
  summary(CH_INT5_model_b)
  
####### WORK ZONE #########
  
  
 
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
  