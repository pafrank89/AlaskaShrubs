# Modeling the effects of Climate and Herbivory on Shrub Growth
# Via Testing of Candidate Model Against Null Model
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

# MODEL SELECTION OF CANDIDATE MODELS ####

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



  
  
  
  
  
  