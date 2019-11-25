# Modeling the effects of Climate and Herbivory on Shrub Growth
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-11-23

install.packages("dplR")           
install.packages("reshape2")        
install.packages("dplyr")           
install.packages("lme4")           
install.packages("nlme") 
install.packages("ggplot2")
install.packages("PerformanceAnalytics")

library(dplR)
library(reshape2)
library(dplyr)
library(lme4)
library(nlme)
library(ggplot2)
library(PerformanceAnalytics)


# Check for correlations between variables
ShrubCCH_DataX = subset (ShrubCCH_Data, select = c("RingWidth", "BAI", "Age", "Section", "Species", "Elevation", "Slope", "Y_Cord", "CanopyCover", 
                                                   "summ.temp", "temp", "summ.min", "summ.max", "wint.rain", "summ.rain", "frost", "wet", "pet",
                                                   "HareIndex", "PropMoose", "PropHare", "PropPtarmagin", "BinaryBrowse", "MooseDensity"))

ShrubCCH_DataX = subset (ShrubCCH_Data, select = c("RingWidth", "BAI", "Age", "Elevation", "Slope", "Y_Cord", "CanopyCover", 
                                                   "summ.temp", "temp", "wint.rain", "summ.rain", "frost", "wet",
                                                   "PropMoose", "PropHare", "PropPtarmagin", "BinaryBrowse"))
           
chart.Correlation(ShrubCCH_DataX, histogram = TRUE, method = c("pearson"))

#Plot BAI as a function of Age
ShrubCCH_BENA <- subset(ShrubCCH_Data, Species == "BENA", select = ShrubID : Age)
                             
ShrubCCH_SALIX <- subset(ShrubCCH_Data, grepl("SA", names(ShrubCCH_Data), value = TRUE), select = ShrubID : Age)

ShrubCCH_SALIX = data.frame(ShrubCCH_Data %>% dplyr:: select (grep("SA", names(ShrubCCH_Data))), check.names = FALSE)

plot(BAI ~ Age, data = ShrubCCH_BENA,
     col = "black", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)")

# Develop mixed effects models 

#Model Structure
#lme(fixed, data, random, correlation, weights, subset, method,
# na.action, control, contrasts = NULL, keep.data = TRUE)

model_BAI <- lme(BAI ~ summ.rain + Total, data = ShrubCCH_Data, random = ~ 1|Section/ShrubID,
                 na.action=na.pass)

summary(model_BAI)
