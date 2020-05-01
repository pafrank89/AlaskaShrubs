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

# 1. ANALYSIS WITH SPATIAL BROWSING ####

# Establish NULL model Betula
null_model_b = lme(resid ~ 1, data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

# Establish FULL model Betula
lme_global_b_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                       iem.summ.temp * PropMoose_S +
                       iem.summ.temp * PropHare_S +
                       iem.summ.temp * PropPtarmagin_S +
                       PropMoose_S + PropHare_S + PropPtarmagin_S,
                     data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "REML")

# Compare NULL and FULL model with ANOVA to establish significance of the full model
anova(null_model_b, lme_global_b_s)

r.squaredLR(lme_global_b_s)

summary(lme_global_b_s)

#Establish NULL model Salix
null_model_s = lme(resid ~ 1, data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

#Establish FULL model Salix
lme_global_s_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                       iem.summ.temp * PropMoose_S +
                       iem.summ.temp * PropHare_S +
                       iem.summ.temp * PropPtarmagin_S +
                       PropMoose_S + PropHare_S + PropPtarmagin_S,
                     data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "REML")


# Compare NULL and FULL model with ANOVA to establish significance of the full model
anova(null_model_s, lme_global_s_s)

r.squaredLR(lme_global_s_s)

summary(lme_global_s_s)

#Plot the Full Model 

#"Temperature:Moose Interaction", "Temperature:Hare Interaction", "Ptarmigan Browsing", "Hare Browsing", "Moose Browsing", "Moose Density", "Hare Index", "Mean Summer Pecip", "Mean Summer Temp"
#"Moose:Hare Interaction", "Temperature:Moose Interaction", "Temperature:Hare Interaction", "Moose Density", "Hare Index", "Mean Summer Pecip", "Mean Summer Temp"

sjPlot :: plot_model(lme_global_b, axis.labels=c("Moose:Hare Interaction", "Temperature:Moose Interaction", "Temperature:Hare Interaction", "Moose Density", "Hare Index", "Mean Summer Pecip", "Mean Summer Temp"),
                     show.values=TRUE, show.p=TRUE, show.intercept = FALSE, show.data = TRUE, type = "est", p.shape = TRUE,
                     title="")

sjPlot :: plot_model(lme_global_s, axis.labels=c("Moose:Hare Interaction", "Temperature:Moose Interaction", "Temperature:Hare Interaction", "Moose Density", "Hare Index", "Mean Summer Pecip", "Mean Summer Temp"),
                     show.values=TRUE, show.p=TRUE, show.intercept = FALSE, show.data = TRUE, type = "est", p.shape = TRUE,
                     title="")

# 1.1. BACKWARD SELECTION FOR BETULA WITH SPATIAL BROWSING ####

summary(lme_global_b_s)

#Check the model assumptions, R2 and VIF for the global model

# Plot Residuals vs. Fitted Plot to check for non-linearity, unequal error variances, and outliers
plot(lme_global_b_s)

# Plot Normal QQ ti assess the distribution of the data
qqnorm(resid(lme_global_b_s))
qqline(resid(lme_global_b_s))

#Check R2 for global model
r.squaredLR(lme_global_b_s)

#Calculate the Variance Inflation Factor (VIF)
vif.lme(lme_global_b_s)

#Preform a "full mode test" comparing (Forstmeier & Schielzeth, 2011), comparing global model to intercept only "Null Model"

null_model_b = lme(resid ~ 1, data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

anova.lme(null_model_b, lme_global_b_s)

#Calculate means and standard errors of parameters from the global model
parameters::standard_error(lme_global_b_s)

summary(lme_global_b_s)

# Step 1 of backward selection using global model
step1_b_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                  iem.summ.temp * PropMoose_S +
                  iem.summ.temp * PropHare_S +
                  iem.summ.temp * PropPtarmagin_S +
                  PropMoose_S + PropHare_S + PropPtarmagin_S, 
                data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step1_b_s)

summary(step1_b_s)

# Step 2 of backward selection 
step2_b_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                  iem.summ.temp * PropMoose_S +
                  iem.summ.temp * PropPtarmagin_S +
                  PropMoose_S + PropPtarmagin_S, 
                data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step2_b_s)

summary(step2_b_s)

# Step 3 of backward selection 
step3_b_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                  iem.summ.temp * PropPtarmagin_S +
                  PropPtarmagin_S, 
                data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step3_b_s)

summary(step3_b_s)

# Step 4 of backward selection 
step4_b_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10, 
                data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step4_b_s)

summary(step4_b_s)

# Step 5 of backward selection 
step5_b_s = lme(resid ~ iem.summ.rain.10,
                data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step5_b_s)

summary(step5_b_s)

# Create the Optimal Model for Betula after backward selection 
Optimal_model_b_s = lme(resid ~ iem.summ.rain.10, 
                          data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "REML")

#Check the model assumptions, R2 and VIF for the global model

# Plot Residuals vs. Fitted Plot to check for non-linearity, unequal error variances, and outliers
plot(Optimal_model_b_s)

# Plot Normal QQ ti assess the distribution of the data
qqnorm(resid(Optimal_model_b_s))
qqline(resid(Optimal_model_b_s))

#Check R2 for global model
r.squaredLR(Optimal_model_b_s)

#Calculate the Variance Inflation Factor (VIF)
vif.lme(Optimal_model_b_s)

# Generate summary for the Optimal Model for Betula
summary(Optimal_model_b_s)

# Examine variance components and their confidence intervals for the Optimal Model for Betula
VarCorr(Optimal_model_b_s)

intervals(Optimal_model_b_s)

#Plot the effect size of the Optimal Model for Betula
sjPlot :: plot_model(Optimal_model_b_s, axis.labels=c("Temperature:Hare Interaction", "Moose Density", "Hare Index", "Mean Summer Pecip", "Mean Summer Temp"),
                     show.values=TRUE, show.p=TRUE, 
                     vline.color = "grey", value.size = 5, dot.size = 3, line.size = 1, wrap.labels = 14,
                     title="")


sjPlot::tab_model(Optimal_model_b_s, show.re.var= TRUE, 
                  pred.labels =c("(Intercept)", "Mean Summer Temperature", "Mean Summer Precipitation", "Snowshoe Hare Index", "Moose Density", "Interaction Hare and Temperature"),
                  dv.labels= "Effects of Summer Temperature & Precipitation on Shrub Growth")

stargazer(Optimal_model_b_s, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

# 1.2. BACKWARD SELECTION FOR SALIX WITH SPATIAL BROWSING ####

summary(lme_global_s_s)

#Check the model assumptions, R2 and VIF for the global model

# Plot Residuals vs. Fitted Plot to check for non-linearity, unequal error variances, and outliers
plot(lme_global_s_s)

# Plot Normal QQ ti assess the distribution of the data
qqnorm(resid(lme_global_s_s))
qqline(resid(lme_global_s_s))

#Check R2 for global model
r.squaredLR(lme_global_s_s)

#Calculate the Variance Inflation Factor (VIF)
vif.lme(lme_global_s_s)

#Preform a "full mode test" comparing (Forstmeier & Schielzeth, 2011), comparing global model to intercept only "Null Model"

null_model_s = lme(resid ~ 1, data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

anova.lme(null_model_s, lme_global_s_s)

summary(lme_global_s_s)

# Step 1 of backward selection using global model
step1_s_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                  iem.summ.temp * PropMoose_S +
                  iem.summ.temp * PropHare_S +
                  iem.summ.temp * PropPtarmagin_S +
                  PropMoose_S + PropHare_S + PropPtarmagin_S,
                data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step1_s_s)

summary(step1_s_s)

# Step 2 of backward selection 
step2_s_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                  iem.summ.temp * PropHare_S +
                  iem.summ.temp * PropPtarmagin_S +
                  PropHare_S + PropPtarmagin_S,
                data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step2_s_s)

summary(step2_s_s)

# Step 3 of backward selection 
step3_s_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 +
                  iem.summ.temp * PropPtarmagin_S +
                  PropPtarmagin_S,
                data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step3_s_s)

summary(step3_s_s)

# Create the Optimal Model for Betula after backward selection 
Optimal_model_s_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 +
                          iem.summ.temp : PropPtarmagin_S +
                          PropPtarmagin_S,
                        data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "REML")

#Check the model assumptions, R2 and VIF for the global model

# Plot Residuals vs. Fitted Plot to check for non-linearity, unequal error variances, and outliers
plot(Optimal_model_s_s)

# Plot Normal QQ ti assess the distribution of the data
qqnorm(resid(Optimal_model_s_s))
qqline(resid(Optimal_model_s_s))

#Check R2 for global model
r.squaredLR(Optimal_model_s_s)

#Calculate the Variance Inflation Factor (VIF)
vif.lme(Optimal_model_s_s)

# Generate summary for the Optimal Model for Betula
summary(Optimal_model_s_s)

# Examine variance components and their confidence intervals for the Optimal Model for Betula
VarCorr(Optimal_model_s_s)

intervals(Optimal_model_s_s)

#Plot the effect size of the Optimal Model for Betula
sjPlot :: plot_model(Optimal_model_s_s, axis.labels=c("Temperature:Ptagmigan Browsing", "Ptagmigan Browsing", "Mean Summer Pecip", "Mean Summer Temp"),
                     show.values=TRUE, show.p=TRUE, 
                     vline.color = "grey", value.size = 5, dot.size = 3, line.size = 1, wrap.labels = 14,
                     title="")

sjPlot::tab_model(Optimal_model_s_s, show.re.var= TRUE, 
                  pred.labels =c("(Intercept)", "Mean Summer Temperature", "Snowshoe Hare Index", "Moose Density"),
                  dv.labels= "Effects of Summer Temperature & Precipitation on Shrub Growth")



# 2. ANALYSIS WITH TEMPORAL BROWSING ####

#Establish NULL model Betula
null_model_b = lme(resid ~ 1, data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

#Establish FULL model Betula
lme_global_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                     iem.summ.temp * MooseDensity +
                     iem.summ.temp * HareIndex +
                     MooseDensity + HareIndex,
                   data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "REML")

# Compare NULL and FULL model with ANOVA to establish significance of the full model
anova(null_model_b, lme_global_b)

r.squaredLR(lme_global_b)

summary(lme_global_b)

#Establish NULL model Salix
null_model_s = lme(resid ~ 1, data = sd_salix_cch_S, random = ~ 1|Section/ShrubID,
                   method = "ML")

#Establish FULL model Salix
lme_global_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                     iem.summ.temp * MooseDensity +
                     iem.summ.temp * HareIndex +
                     MooseDensity + HareIndex,
                   data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "REML")


# Compare NULL and FULL model with ANOVA to establish significance of the full model
anova(null_model_s, lme_global_s)

r.squaredLR(lme_global_s)

summary(lme_global_s)

#Plot the Full Model 

#"Temperature:Moose Interaction", "Temperature:Hare Interaction", "Ptarmigan Browsing", "Hare Browsing", "Moose Browsing", "Moose Density", "Hare Index", "Mean Summer Pecip", "Mean Summer Temp"
#"Moose:Hare Interaction", "Temperature:Moose Interaction", "Temperature:Hare Interaction", "Moose Density", "Hare Index", "Mean Summer Pecip", "Mean Summer Temp"

sjPlot :: plot_model(lme_global_b, axis.labels=c("Temperature:Hare Interaction", "Temperature:Moose Interaction", "Moose Density", "Hare Index", "Mean Summer Pecip", "Mean Summer Temp"),
                     show.values=TRUE, show.p=TRUE, show.intercept = FALSE, show.data = TRUE, type = "est", p.shape = TRUE,
                     title="")

sjPlot :: plot_model(lme_global_s, axis.labels=c("Temperature:Hare Interaction", "Temperature:Moose Interaction", "Moose Density", "Hare Index", "Mean Summer Pecip", "Mean Summer Temp"),
                     show.values=TRUE, show.p=TRUE, show.intercept = FALSE, show.data = TRUE, type = "est", p.shape = TRUE,
                     title="")

# 2.1. BACKWARD SELECTION FOR BETULA WITH TEMPORAL DATA ####

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


#Calculate means and standard errors of parameters from the global model
parameters::standard_error(lme_global_b)

summary(lme_global_b)

# Step 1 of backward selection using global model
step1_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * MooseDensity +
                iem.summ.temp * HareIndex +
                MooseDensity + HareIndex,
              data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step1_b)

summary(step1_b)

# Step 2 of backward selection 
step2_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 +
                iem.summ.temp * HareIndex +
                MooseDensity + HareIndex,
              data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step2_b)

summary(step2_b)

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

OMB_coef = coef(Optimal_model_b)

#Plot the effect size of the Optimal Model for Betula
fp = sjPlot :: plot_model(Optimal_model_b, axis.labels=c("Temperature:Hare Interaction", "Moose Density", "Hare Index", "Mean Summer Pecip", "Mean Summer Temp"),
                          show.values=TRUE, show.p=TRUE,
                          vline.color = "grey", value.size = 5, dot.size = 3, line.size = 1, wrap.labels = 14,
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

# 2.2. BACKWARD SELECTION FOR SALIX WITH TEMPORAL DATA ####

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
                MooseDensity + HareIndex,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step1_s)

summary(step2_s)

# Step 2 of backward selection 
step2_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * HareIndex +
                MooseDensity + HareIndex,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step2_s)

summary(step2_s)

# Step 3 of backward selection 
step3_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 +
                MooseDensity + HareIndex,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step3_s)

summary(step3_s)

# Step 4 of backward selection 
step4_s = lme(resid ~ iem.summ.temp + 
                MooseDensity + HareIndex,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

anova.lme(step4_s)

summary(step4_s)

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

axis(3,at=0.5:2,las=1,labels=c("Full\n Model", "Minimal\nAdequate\nModel"))
axis(2,at=0.5:6,las=2,labels=c("Snowshow Hare :\nMean SummerTemperature\nInteraction",
                               "Moose :\nMean Summer Temperature\nInteraction",
                               "Snowshoe Hare\nIndex", "Moose Density", 
                               "Mean Summer\nTemperature", "Mean Summer\nPrecipitation"))

mtext('Betula nana Model Selection', side=3, line=3, padj = 2, at = -.35, cex = 1.25)

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

axis(3,at=0.5:4,las=1,labels=c("Full\n Model", "Step 1", "Step 2", "Minimal\nAdequate\nModel"))
axis(2,at=0.5:6,las=2,labels=c("Snowshow Hare :\nMean SummerTemperature\nInteraction",
                               "Moose :\nMean Summer Temperature\nInteraction",
                               "Snowshoe Hare\nIndex", "Moose Density", 
                               "Mean Summer\nTemperature", "Mean Summer\nPrecipitation"))

mtext('Salix spp. Model Selection', side=3, line=3, padj = 2, at = -.8, cex = 1.25)


#Salix: Spatial
#rownames(ModelSelection_Salix_S) = ModelSelection_Salix_S$Parameter

#ModelSelection_Salix_S$Parameter = NULL

par(mar = c(5,12,5,2) + 0.1)

color2D.matplot(ModelSelection_Salix_S, 
                vcol = "White", na.color = "White",
                extremes = c("Blue","Red"),
                nslices = 50,
                show.legend=TRUE, show.values=4,
                axes=FALSE, xlab="",ylab="")

axis(3,at=0.5:3,las=1,labels=c("Full\n Model", "Step 1", "Minimal\nAdequate\nModel"))
axis(2,at=0.5:8,las=2,labels=c("Ptarmagin :\nMean SummerTemperature\nInteraction",
                               "Snowshow Hare :\nMean Summer Temperature\nInteraction",
                               "Moose :\nMean Summer Temperature\nInteraction",
                               "Ptarmagin\n Browsing Intensity", "Snowshoe Hare\nBrowsing Intensity", "Moose\nBrowsingIntensity",
                               "Mean Summer\nPrecipitation", "Mean Summer\nTemperature"))

mtext('Salix spp. Model Selection', side=3, line=3, padj = 2, at = -.6, cex = 1.25)




