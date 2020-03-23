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


# 1. BACKWARD SELECTION FOR BETULA ####

# Establish a the global model with all variables and interactions 
lme_global_b = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                    iem.summ.temp * iem.summ.rain.10 +
                    iem.summ.temp * HareIndex +
                    iem.summ.temp * MooseDensity +
                    MooseDensity + HareIndex,
                    data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML")

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
anova.lme(lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                 iem.summ.temp * iem.summ.rain.10 +
                 iem.summ.temp * HareIndex +
                 iem.summ.temp * MooseDensity +
                 MooseDensity + HareIndex,
                 data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML"))

# Step 2 of backward selection 
anova.lme(lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * HareIndex +
                iem.summ.temp * MooseDensity +
                MooseDensity + HareIndex,
                data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML"))

# Step 3 of backward selection 
anova.lme(lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * HareIndex +
                MooseDensity + HareIndex,
                data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML"))

# Step 4 of backward selection 
anova.lme(lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * HareIndex +
                MooseDensity + HareIndex,
                data = sd_bena_cch_S, random = ~ 1|Section/ShrubID, method = "ML"))

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
?"sjPlot-themes"

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
                    iem.summ.temp * iem.summ.rain.10 +
                    iem.summ.temp * HareIndex +
                    iem.summ.temp * MooseDensity +
                    MooseDensity + HareIndex,
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
anova.lme(lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
              iem.summ.temp * iem.summ.rain.10 +
              iem.summ.temp * HareIndex +
              iem.summ.temp * MooseDensity +
              MooseDensity + HareIndex +
              PropMoose + PropHare + PropPtarmagin
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML"))
  
  
# Step 2 of backward selection 
anova.lme(lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
              iem.summ.temp * iem.summ.rain.10 +
              iem.summ.temp * HareIndex +
              MooseDensity + HareIndex,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML"))

# Step 3 of backward selection 
anova.lme(lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
              iem.summ.temp * iem.summ.rain.10 +
              MooseDensity + HareIndex,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML"))

# Step 4 of backward selection 
anova.lme(lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
              MooseDensity + HareIndex,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML"))

# Step 5 of backward selection 
anova.lme(lme(resid ~ iem.summ.temp + 
              MooseDensity + HareIndex,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML"))

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






#WORK ZONE ####
# Step 1 of backward selection using global model
anova.lme(lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * iem.summ.rain.10 +
                iem.summ.temp * HareIndex +
                iem.summ.temp * MooseDensity +
                MooseDensity + HareIndex +
                PropMoose + PropHare + PropPtarmagin,
                data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML"))

# Step 2 of backward selection using global model
anova.lme(lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * iem.summ.rain.10 +
                iem.summ.temp * HareIndex +
                MooseDensity + HareIndex +
                PropMoose + PropHare + PropPtarmagin,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML"))

# Step 3 of backward selection using global model
anova.lme(lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * iem.summ.rain.10 +
                MooseDensity + HareIndex +
                PropMoose + PropHare + PropPtarmagin,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML"))

# Step 4 of backward selection using global model
anova.lme(lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * iem.summ.rain.10 +
                MooseDensity + HareIndex +
                PropMoose + PropPtarmagin,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML"))

# Step 5 of backward selection using global model
anova.lme(lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                iem.summ.temp * iem.summ.rain.10 +
                MooseDensity + HareIndex +
                PropMoose,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML"))

# Step 6 of backward selection using global model
anova.lme(lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                MooseDensity + HareIndex +
                PropMoose,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML"))

anova.lme(lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                MooseDensity + HareIndex,
              data = sd_salix_cch_S, random = ~ 1|Section/ShrubID, method = "ML"))









