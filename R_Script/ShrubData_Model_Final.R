# Modeling the effects of Climate and Herbivory on Shrub Growth
# Peter Frank 
# peterfr@stud.ntnu.no
# 2020-03-04

# Our objective is to test the following three hypotheses:

#H1 - Increased temperature and precipitation will have a positive effect on radial growth 
#H2 - Increased herbivore density will have a negative effect on radial growth 
#H3 - Shrub growth positivly effected by the interactive effects of herbivory and temperature 
# ON HOLD **** H4 - The effects of herbivory will be less evident in well defended Betula species when compared to more palatable Salix species

# This code will utilize backward model selection based on liklihood ratio tests to determine the optimal mixed effects model to descripe the variance in 
# the detrended BAI data (resid). This process will begin by fitting a global model and proceeding with a backward selection process. 

#FULL MODEL TESTS ####

#Establish NULL model Betula
null_model_b = lme(resid ~ 1, data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

#Establish FULL model Betula
lme_global_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                     iem.summ.temp * HareIndex +
                     iem.summ.temp * MooseDensity +
                     #MooseDensity * HareIndex +
                     MooseDensity + HareIndex + PropMoose + PropHare + PropPtarmagin,
                   data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

# Compare NULL and FULL model with ANOVA to establish significance of the full model
anova(null_model_b, lme_global_b)

r.squaredLR(lme_global_b)

summary(lme_global_b)

#Establish NULL model Salix
null_model_s = lme(resid ~ 1, data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

#Establish FULL model Salix
lme_global_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                     iem.summ.temp * HareIndex +
                     iem.summ.temp * MooseDensity +
                     MooseDensity * HareIndex +
                     MooseDensity + HareIndex, # + PropMoose + PropHare + PropPtarmagin,
                   data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML")


# Compare NULL and FULL model with ANOVA to establish significance of the full model
anova(null_model_s, lme_global_s)

r.squaredLR(lme_global_s)

summary(lme_global_s)

#Plot the Full Model 

#"Temperature:Moose Interaction", "Temperature:Hare Interaction", "Ptarmigan Browsing", "Hare Browsing", "Moose Browsing", "Moose Density", "Hare Index", "Mean Summer Pecip", "Mean Summer Temp"
#"Moose:Hare Interaction", "Temperature:Moose Interaction", "Temperature:Hare Interaction", "Moose Density", "Hare Index", "Mean Summer Pecip", "Mean Summer Temp"

sjPlot :: plot_model(lme_global_b, axis.labels=c("Moose:Hare Interaction", "Temperature:Moose Interaction", "Temperature:Hare Interaction", "Moose Density", "Hare Index", "Mean Summer Pecip", "Mean Summer Temp"),
                     show.values=TRUE, show.p=TRUE, show.intercept = FALSE, show.data = TRUE, type = "est", p.shape = TRUE,
                     title="")

sjPlot :: plot_model(lme_global_s, axis.labels=c("Moose:Hare Interaction", "Temperature:Moose Interaction", "Temperature:Hare Interaction", "Moose Density", "Hare Index", "Mean Summer Pecip", "Mean Summer Temp"),
                     show.values=TRUE, show.p=TRUE, show.intercept = FALSE, show.data = TRUE, type = "est", p.shape = TRUE,
                     title="")

# 1. BACKWARD SELECTION FOR BETULA ####

# Establish a the global model with all variables and interactions 
summary(lme_global_b)

#Check the model assumptions, R2 and VIF for the global model
 
 # Plot Residuals vs. Fitted Plot to check for non-linearity, unequal error variances, and outliers
  plot(lme_global_b)

  # Plot Normal QQ ti assess the distribution of the data
  qqnorm(resid(lme_global_b))
  qqline(resid(lme_global_b))
  
  #Check R2 for global model
  r.squaredLR(lme_global_b)
  
  #Calculate the Variance Inflation Factor (VIF)
  vif.lme(lme_global_b)
  
#Preform a "full mode test" comparing (Forstmeier & Schielzeth, 2011), comparing global model to intercept only "Null Model"

null_model_b = lme(resid ~ 1, data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")
  
anova.lme(null_model_b, lme_global_b)

#Calculate means and standard errors of parameters from the global model
parameters::standard_error(lme_global_b)

summary(lme_global_b)

# Step 1 of backward selection using global model
step1_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                 iem.summ.temp * HareIndex +
                 iem.summ.temp * MooseDensity +
                 MooseDensity + HareIndex + PropMoose + PropHare + PropPtarmagin, 
                 data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step1_b)

summary(step1_b)

# Step 2 of backward selection 
step2_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * HareIndex +
                iem.summ.temp * MooseDensity +
                MooseDensity + HareIndex + PropHare + PropPtarmagin,
                data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step2_b)

summary(step2_b)

# Step 3 of backward selection 
step3_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * HareIndex +
                iem.summ.temp * MooseDensity +
                MooseDensity + HareIndex + PropPtarmagin,
                data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step3_b)

summary(step3_b)

# Step 4 of backward selection 
step4_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * HareIndex +
                iem.summ.temp * MooseDensity +
                MooseDensity + HareIndex,
              data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step4_b)

summary(step4_b)

# Step 5 of backward selection 
step5_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * HareIndex +
                MooseDensity + HareIndex,
              data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step5_b)

summary(step5_b)

# Create the Optimal Model for Betula after backward selection 
Optimal_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * HareIndex +
                MooseDensity + HareIndex,
                data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "REML")

#Check the model assumptions, R2 and VIF for the global model

  # Plot Residuals vs. Fitted Plot to check for non-linearity, unequal error variances, and outliers
  plot(Optimal_model_b)
  
  # Plot Normal QQ ti assess the distribution of the data
  qqnorm(resid(Optimal_model_b))
  qqline(resid(Optimal_model_b))
  
  #Check R2 for global model
  r.squaredLR(Optimal_model_b)
  
  #Calculate the Variance Inflation Factor (VIF)
  vif.lme(Optimal_model_b)

# Generate summary for the Optimal Model for Betula
summary(Optimal_model_b)

# Examine variance components and their confidence intervals for the Optimal Model for Betula
VarCorr(Optimal_model_b)

intervals(Optimal_model_b)

#Plot the effect size of the Optimal Model for Betula
fp = sjPlot :: plot_model(Optimal_model_b, axis.labels=c("Temperature:Hare Interaction", "Moose Density", "Hare Index", "Mean Summer Pecip", "Mean Summer Temp"),
                     show.values=TRUE, show.p=TRUE, show.intercept = FALSE, show.data = TRUE, type = "est", p.shape = TRUE,
                     title="")
fp

fp + theme_sjplot2()

sjPlot::tab_model(Optimal_model_b, show.re.var= TRUE, 
                  pred.labels =c("(Intercept)", "Mean Summer Temperature", "Mean Summer Precipitation", "Snowshoe Hare Index", "Moose Density", "Interaction Hare and Temperature"),
                  dv.labels= "Effects of Summer Temperature & Precipitation on Shrub Growth")

stargazer(Optimal_model_b, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# 2. BACKWARD SELECTION FOR SALIX ####
lme_global_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                     iem.summ.temp * HareIndex +
                     iem.summ.temp * MooseDensity +
                     MooseDensity + HareIndex + PropMoose + PropHare + PropPtarmagin,
                    data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML")


summary(lme_global_s)

#Check the model assumptions, R2 and VIF for the global model

  # Plot Residuals vs. Fitted Plot to check for non-linearity, unequal error variances, and outliers
  plot(lme_global_s)
  
  # Plot Normal QQ ti assess the distribution of the data
  qqnorm(resid(lme_global_s))
  qqline(resid(lme_global_s))
  
  #Check R2 for global model
  r.squaredLR(lme_global_s)
  
  #Calculate the Variance Inflation Factor (VIF)
  vif.lme(lme_global_s)

#Preform a "full mode test" comparing (Forstmeier & Schielzeth, 2011), comparing global model to intercept only "Null Model"
  
null_model_s = lme(resid ~ 1, data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")
  
anova.lme(null_model_s, lme_global_s)

summary(lme_global_s)
  
# Step 1 of backward selection using global model
step1_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * HareIndex +
                iem.summ.temp * MooseDensity +
                MooseDensity + HareIndex + PropMoose + PropHare + PropPtarmagin,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step1_s)

summary(step2_s)
  
# Step 2 of backward selection 
step2_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * HareIndex +
                MooseDensity + HareIndex + PropMoose + PropHare + PropPtarmagin,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step2_s)

summary(step2_s)

# Step 3 of backward selection 
step3_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * HareIndex +
                MooseDensity + HareIndex + PropMoose + PropPtarmagin,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step3_s)

summary(step3_s)

# Step 4 of backward selection 
step4_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                MooseDensity + HareIndex + PropMoose + PropPtarmagin,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step4_s)

summary(step4_s)

# Step 5 of backward selection 
step5_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                MooseDensity + HareIndex + PropMoose,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step5_s)

summary(step5_s)

# Step 6 of backward selection 
step6_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                MooseDensity + HareIndex,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step6_s)

summary(step6_s)

# Step 7 of backward selection 
step7_s = lme(resid ~ iem.summ.temp + 
                MooseDensity + HareIndex,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step7_s)

summary(step7_s)

# Create the Optimal Model for Betula after backward selection 
Optimal_model_s = lme(resid ~ iem.summ.temp + 
                        MooseDensity + HareIndex,
                      data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "REML")

#Check the model assumptions, R2 and VIF for the global model

  # Plot Residuals vs. Fitted Plot to check for non-linearity, unequal error variances, and outliers
  plot(Optimal_model_s)
  
  # Plot Normal QQ ti assess the distribution of the data
  qqnorm(resid(Optimal_model_s))
  qqline(resid(Optimal_model_s))
  
  #Check R2 for global model
  r.squaredLR(Optimal_model_s)
  
  #Calculate the Variance Inflation Factor (VIF)
  vif.lme(Optimal_model_s)

# Generate summary for the Optimal Model for Betula
summary(Optimal_model_s)

# Examine variance components and their confidence intervals for the Optimal Model for Betula
VarCorr(Optimal_model_s)

intervals(Optimal_model_s)

#Plot the effect size of the Optimal Model for Betula
forest_plot_s = sjPlot :: plot_model(Optimal_model_s, axis.labels=c("Hare Index", "Moose Density", "Mean Summer Temp"),
                     show.values=TRUE, show.p=TRUE, 
                     vline.color = "grey", value.size = 5, dot.size = 3, line.size = 1, wrap.labels = 14,
                     title="")

forest_plot_s

forest_plot_s + theme_sjplot2(base_size = 15, base_family = "")


sjPlot::tab_model(Optimal_model_s, show.re.var= TRUE, 
                  pred.labels =c("(Intercept)", "Mean Summer Temperature", "Snowshoe Hare Index", "Moose Density"),
                  dv.labels= "Effects of Summer Temperature & Precipitation on Shrub Growth")







# 3. PLOT MODEL SELECTION PROCESS ####

#Salix
#rownames(ModelSelection_Salix) = ModelSelection_Salix$Parameter

#ModelSelection_Salix$Parameter = NULL

par(mar = c(5,12,5,2) + 0.1)

color2D.matplot(ModelSelection_Salix, 
                vcol = "White", na.color = "White",
                extremes = c("Blue","Red"),
                nslices = 50,
                show.legend=TRUE, show.values=4,
                axes=FALSE, xlab="",ylab="")

axis(3,at=0.5:7,las=1,labels=c("Full\n Model", "Step 1", "Step 2", "Step 3", "Step 4", "Step 5", "Minimal\nAdequate\nModel"))
axis(2,at=0.5:9,las=2,labels=c("Snowshow Hare :\nMean SummerTemperature\nInteraction",
                               "Moose :\nMean Summer Temperature\nInteraction",
                               "Ptarmagin\n Browsing Intensity", "Snowshoe Hare\nBrowsing Intensity", "Moose\nBrowsingIntensity",
                               "Snowshoe Hare\nIndex", "Moose Density", 
                               "Mean Summer\nTemperature", "Mean Summer\nPrecipitation"))
#Betula
#rownames(ModelSelection_Betula) = ModelSelection_Betula$Parameter

#ModelSelection_Betula$Parameter = NULL

par(mar = c(5,12,5,2) + 0.1)

color2D.matplot(ModelSelection_Betula, 
                vcol = "White", na.color = "White",
                extremes = c("Blue","Red"),
                nslices = 50,
                show.legend=TRUE, show.values=4,
                axes=FALSE, xlab="",ylab="")

#axis(3,at=0.5:5,las=1,labels=colnames(ModelSelection_Betula))
axis(3,at=0.5:5,las=1,labels=c("Full\n Model", "Step 1", "Step 2", "Step 3", "Minimal\nAdequate\nModel"))
axis(2,at=0.5:9,las=2,labels=c("Snowshow Hare :\nMean SummerTemperature\nInteraction",
                               "Moose :\nMean Summer Temperature\nInteraction",
                               "Ptarmagin\n Browsing Intensity", "Snowshoe Hare\nBrowsing Intensity", "Moose\nBrowsingIntensity",
                               "Snowshoe Hare\nIndex", "Moose Density", 
                               "Mean Summer\nTemperature", "Mean Summer\nPrecipitation"))

# MODEL SELECTION THROUGH MODEL AVERAGING ####
# Betula Model Averaging
lme_global_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                     iem.summ.temp * HareIndex +
                     iem.summ.temp * MooseDensity +
                     MooseDensity + HareIndex +
                     PropMoose + PropHare + PropPtarmagin,
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
                     iem.summ.temp * HareIndex +
                     iem.summ.temp * MooseDensity +
                     MooseDensity + HareIndex +
                     PropMoose + PropHare + PropPtarmagin,
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

model.importance.plot(model.sel_s)

# MODEL SELECTION OF 15 CANDIDATE MODELS ####

# This code will test a series of 15 candidate models for each species

# NULL MODEL ####
#Null models will be used to compare the effect of adding fixed effects to the model 
null_model_b = lme(resid ~ 1, data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

null_model_s = lme(resid ~ 1, data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")


#CLIMATE MODELS ####

## Mean Summer Temperature ####

# MST for Betula
C1_model_b = lme(resid ~ iem.summ.temp, data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                 method = "ML")

anova(null_model_b, C1_model_b)

summary(C1_model_b)

r.squaredLR(C1_model_b)

# MST for Salix
C1_model_s = lme(resid ~ iem.summ.temp, data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                 method = "ML")

anova(null_model_s, C1_model_s)

summary(C1_model_s)

r.squaredLR(C1_model_s)

## Mean Summer Precipitation  ####

# MSP for Betula 
C2_model_b = lme(resid ~ iem.summ.rain.10, data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                 method = "ML")

anova(null_model_b, C2_model_b)

summary(C2_model_b)

r.squaredLR(C2_model_b)

# MSP for Salix 
C2_model_s = lme(resid ~ iem.summ.rain.10, data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                 method = "ML")

anova(null_model_s, C2_model_s)

summary(C2_model_s)

r.squaredLR(C2_model_b)

## Addative Effect MST + MSP ####

# MST + MSP for Betula
C1C2_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10, data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

anova(null_model_b, C1C2_model_b)

summary(C1C2_model_b)

r.squaredLR(C1C2_model_b)

# MST + MSP for Salix
C1C2_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10, data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

anova(null_model_s, C1C2_model_s)

summary(C1C2_model_s)

r.squaredLR(C1C2_model_s)

# HERBIVORY MODELS ####

## Temporal Moose Density model ####

# Betula
H1_model_b <- lme(resid ~ MooseDensity, data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_b, H1_model_b)

summary(H1_model_b)

r.squaredLR(H1_model_b)

# Salix
H1_model_s <- lme(resid ~ MooseDensity, data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_s, H1_model_s)

summary(H1_model_s)

r.squaredLR(H1_model_s)

## Temporal Hare Index ####

# Betula
H2_model_b <- lme(resid ~ HareIndex, data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_b, H2_model_b)

summary(H2_model_b)

r.squaredLR(H2_model_b)

# Salix
H2_model_s <- lme(resid ~ HareIndex, data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_s, H2_model_s)

summary(H2_model_s)

r.squaredLR(H2_model_s)

## Spatial browsing pressure by Moose ####

# Betula 
H3S_model_b <- lme(resid ~ PropMoose_S, data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

anova(null_model_b, H3S_model_b)

summary(H3S_model_b)

r.squaredLR(H3S_model_b)

# Salix 
H3S_model_s <- lme(resid ~ PropMoose_S, data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

anova(null_model_s, H3S_model_s)

summary(H3S_model_s)

r.squaredLR(H3S_model_s)

## Spatial browsing pressure by Hare #### 

# Betula 
H4S_model_b <- lme(resid ~ PropHare_S, data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

anova(null_model_b, H4S_model_b)

summary(H4S_model_b)

r.squaredLR(H4S_model_b)

# Salix 
H4S_model_s <- lme(resid ~ PropHare_S, data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

anova(null_model_s, H4S_model_s)

summary(H4S_model_s)

r.squaredLR(H4S_model_s)

## Spatial browsing pressure by Ptarmagin #### 

# Betula 
H5S_model_b <- lme(resid ~ PropPtarmagin_S, data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

anova(null_model_b, H5S_model_b)

summary(H5S_model_b)

r.squaredLR(H5S_model_b)

# Salix 
H5S_model_s <- lme(resid ~ PropPtarmagin_S, data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

anova(null_model_s, H5S_model_s)

summary(H5S_model_s)

r.squaredLR(H5S_model_s)

## Temporal Moose Density + Tempooral Hare Index ####

# Betula
H1H2_model_b <- lme(resid ~ MooseDensity + HareIndex, data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_b, H1H2_model_b)

summary(H1H2_model_b)

r.squaredLR(H1H2_model_b)

# Salix
H1H2_model_s <- lme(resid ~ MooseDensity + HareIndex, data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_s, H1H2_model_s)

summary(H1H2_model_s)

r.squaredLR(H1H2_model_s)

## Spatial browsing pressure by Moose + Spatial browsing pressure by Harex ####

# Betula
H3H4_model_b <- lme(resid ~ PropMoose_S + PropHare_S, data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                    method = "ML")

anova(null_model_b, H3H4_model_b)

summary(H3H4_model_b)

r.squaredLR(H3H4_model_b)

# Salix
H3H4_model_s <- lme(resid ~ PropMoose_S + PropHare_S, data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                    method = "ML")

anova(null_model_s, H3H4_model_s)

summary(H3H4_model_s)

r.squaredLR(H3H4_model_s)

# HERBIVORY & CLIMATE MODELS ####

## Climate & Moose Density ####

# Betula
CH1_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10  +
                    MooseDensity, 
                  data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_b, CH1_model_b)

summary(CH1_model_b) 

r.squaredLR(CH1_model_b)

# Salix
CH1_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                    MooseDensity, 
                  data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_s, CH1_model_s)

summary(CH1_model_s) 

r.squaredLR(CH1_model_s)

## Climate & Hare Density ####

# Betula
CH2_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                    HareIndex, 
                  data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_b, CH2_model_b)

summary(CH2_model_b)

r.squaredLR(CH2_model_b)

# Salix
CH2_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                    HareIndex, 
                  data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_s, CH2_model_s)

summary(CH2_model_s)

r.squaredLR(CH2_model_s)

## Climate & browsing pressure by Moose ####

# Betula
CH3_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10  +
                    PropMoose_S, 
                  data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_b, CH3_model_b)

summary(CH3_model_b) 

r.squaredLR(CH3_model_b)

# Salix
CH3_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                    PropMoose_S, 
                  data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_s, CH3_model_s)

summary(CH3_model_s) 

r.squaredLR(CH3_model_s)

## Climate & browsing pressure by Hare ####

# Betula
CH4_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                    PropHare_S, 
                  data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_b, CH4_model_b)

summary(CH4_model_b)

r.squaredLR(CH4_model_b)

# Salix
CH4_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                    PropHare_S, 
                  data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_s, CH4_model_s)

summary(CH4_model_s)

r.squaredLR(CH4_model_s)

## Climate & browsing pressure by Ptarmagin ####

# Betula
CH5_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                    PropPtarmagin_S, 
                  data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_b, CH5_model_b)

summary(CH5_model_b)

r.squaredLR(CH5_model_b)

# Salix
CH5_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                    PropPtarmagin_S, 
                  data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_s, CH5_model_s)

summary(CH5_model_s)

r.squaredLR(CH5_model_s)

## Climate & Moose Density + Hare Index ####

# Betula
CH1H2_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10  +
                    MooseDensity + HareIndex, 
                  data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_b, CH1H2_model_b)

summary(CH1H2_model_b) 

r.squaredLR(CH1H2_model_b)

# Salix
CH1H2_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                    MooseDensity + HareIndex, 
                  data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                  method = "ML")

anova(null_model_s, CH1H2_model_s)

summary(CH1H2_model_s) 

r.squaredLR(CH1H2_model_s)

# INTERACTION OF CLIMATE AND HERBIVORY ####

## MooseDensity : Mean Summer Temperature ####

# Betula  
CH1_INT_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 +
                       MooseDensity +
                       iem.summ.temp*MooseDensity,
                     data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                     method = "ML")

anova(null_model_b, CH1_INT_model_b)

summary(CH1_INT_model_b)

r.squaredLR(CH1_INT_model_b)

# Salix 
CH1_INT_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 +
                       MooseDensity * HareIndex +
                       iem.summ.temp:MooseDensity,
                     data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                     method = "ML")

anova(null_model_s, CH1_INT_model_s)

summary(CH1_INT_model_s)

r.squaredLR(CH1_INT_model_s)

## Hare Index : Mean Summer Temperature ####

# Betula  
CH2_INT_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                        HareIndex + MooseDensity +
                        iem.summ.temp:MooseDensity + iem.summ.temp:HareIndex,
                      data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                      method = "ML")

anova(null_model_b, CH2_INT_model_b)


summary(CH2_INT_model_b)

r.squaredLR(CH2_INT_model_b)


# Salix 
CH2_INT_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                        HareIndex + MooseDensity +
                        iem.summ.temp*HareIndex + iem.summ.temp*MooseDensity,
                      data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                      method = "ML")

anova(null_model_s, CH2_INT_model_s)

summary(CH2_INT_model_s)

r.squaredLR(CH2_INT_model_s)


## Moose Density + HareIndex + Hare Index : Mean Summer Temperature ####

# Betula  
CH1H2_INT_model_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                        HareIndex +
                        iem.summ.temp * MooseDensity,
                      data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                      method = "ML")

anova(null_model_b, CH1H2_INT_model_b)


summary(CH1H2_INT_model_b)

r.squaredLR(CH1H2_INT_model_b)


# Salix 
CH1H2_INTH2_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                        
                        iem.summ.temp* HareIndex + iem.summ.temp* MooseDensity ,
                      data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                      method = "ML")

anova(null_model_s, CH1H2_INTH2_model_s)

summary(CH1H2_INTH2_model_s)

r.squaredLR(CH1H2_INTH2_model_s)


