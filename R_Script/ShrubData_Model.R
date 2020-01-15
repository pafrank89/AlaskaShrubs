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
install.packages("corrplot")
install.packages("RColorBrewer")
install.packages("gamlss")

library(dplR)
library(reshape2)
library(dplyr)
library(lme4)
library(nlme)
library(ggplot2)
library(PerformanceAnalytics)
library(corrplot)
library(RColorBrewer)
library(gamlss)

#CORRELATION MATRICIES ----------------------
# Check for correlations between variables
ShrubCCH_DataX = subset (ShrubCCH_Data, select = c("RingWidth", "BAI", "Age", "Section", "Species", "Elevation", "Slope", "Y_Cord", "CanopyCover", 
                                                   "summ.temp", "temp", "summ.min", "summ.max", "wint.rain", "summ.rain", "frost", "wet", "pet",
                                                   "HareIndex", "MooseDensity", "PropMoose", "PropHare"))

ShrubCCH_DataX = subset (ShrubCCH_Data, select = c("RingWidth", "BAI", "Age", "Elevation", "Slope", "Y_Cord", "CanopyCover", 
                                                   "summ.temp", "temp", "wint.rain", "summ.rain", "frost", "wet",
                                                   "PropMoose", "PropHare"))

#Climate and BAI correlations
ShrubCCH_DataX = subset (ShrubCCH_Data, select = c("RingWidth", "BAI",  
                                                   "summ.temp", "temp", "summ.min", "summ.max", "wint.rain", "summ.rain", "frost", "wet", "pet"))
                 
#Age and BAI correlations
ShrubCCH_DataX = subset (ShrubCCH_Data, select = c("RingWidth", "BAI", "Age"))


#Herbivore and BAI correlations                                  
#Not enough observations to include ptarmagin browsing 
ShrubCCH_DataX = subset (ShrubCCH_Data, select = c("RingWidth", "BAI", "Age",
                                                   "HareIndex", "MooseDensity", "PropMoose", "PropHare"))
#Climate and Herbivory correlations
ShrubCCH_DataX = subset (ShrubCCH_Data, select = c("RingWidth", "BAI", "Age",
                                                  "summ.temp", "temp", "summ.min", "summ.max", "wint.rain", "summ.rain", "frost", "wet", "pet", 
                                                   "HareIndex", "MooseDensity"))
#Environmental Covariates and BAI
ShrubCCH_DataX = subset (ShrubCCH_Data, select = c("RingWidth", "BAI", "Age",  
                                                   "Elevation", "Slope", "Y_Cord", "CanopyCover", "DistToRoad"))


#Creates a correlation matrix using the variables specified above
chart.Correlation(ShrubCCH_DataX, histogram = TRUE, method = c("pearson"))

#Creates a color coded correlation matrix using the variables specified above
ShrubCCH_DataCorr = cor(ShrubCCH_DataX)

corrplot(ShrubCCH_DataCorr , type="upper", order="hclust",
         col=brewer.pal(n=9, name="RdYlBu"))

#BOX PLOTS----------------------
#Plot BAI as a function of soil texture
plot(BAI ~ factor(SoilText), data = ShrubCCH_Data, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2,
     pch = 1, ylab = "Basal Area Increment", xlab = "")

#plot BAI as a function of soil moisture
plot(BAI ~ factor(SoilMoist), data = ShrubCCH_Data, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2,
     pch = 1, ylab = "Basal Area Increment", xlab = "")

#Plot BAI as a function of observed vegetation cover 
plot(BAI ~ factor(VegCoverOBS), data = ShrubCCH_Data, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2,
     pch = 1, ylab = "Basal Area Increment", xlab = "")

#STEM HEIGHT VS STEM DIAMETER -----------------------
#Plot Height ad a function of Stem Diameter
plot(StemHeight ~ StemDiam, data = ShrubCCH_SALIX,
     col = "black", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)")

ggplot(data = ShrubCCH_Data, 
       aes(x = StemDiam,
           y = StemHeight,
           colour=Species)) + 
  geom_point(size = 2) +
  geom_smooth(method= "lm") +
  xlab("Diameter (mm") + ylab("Height (cm)")


#BAI VS AGE -----------------------
#Subsets the data by species
ShrubCCH_BENA <- subset(ShrubCCH_Data, Species == "BENA", select = ShrubID : Age)
                             
ShrubCCH_SALIX <- subset(ShrubCCH_Data, grepl("^SA", ShrubCCH_Data$Species), select = ShrubID : Age)

#Plots BAI as a function of age on two graphs by species
par(mfrow=c(1,2))

plot(BAI ~ Age, data = ShrubCCH_BENA,
     col = "black", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)", ylim=c(0, 83), main = "Betula")

abline(v=5)

plot(BAI ~ Age, data = ShrubCCH_SALIX,
     col = "blue", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)", ylim=c(0, 83), main = "Salix")

abline(v=5)

ln_shrub = gamlss(ShrubCCH_Data$BAI ~ ShrubCCH_Data$Age)

plot(ln_shrub)


#Take the risiduals of a log normal age trend log against age and the residuals 
#age standardizatio of BAI to the residuals 

#interaction across all sites vs herbivory for models within each site

# each site as a model then modle the model 

#Plots BAI as a function of age on the same graph
par(mfrow=c(1,1))
colors = c("Black", "Red", "Red", "Red")

plot(BAI ~ Age, data = ShrubCCH_Data,
     col = colors[ unclass(as.factor(ShrubCCH_Data$Species))], pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)")

legend("topright", inset = 0.02, legend = c("Betula", "Salix"), col = c("black", "red"), pch = 1)


#Develop mixed effects models --------------------

#Model Structure
#lme(fixed, data, random, correlation, weights, subset, method,
# na.action, control, contrasts = NULL, keep.data = TRUE)

model_BAI <- lme(BAI ~ summ.rain + Total, data = ShrubCCH_Data, random = ~ 1|Section/ShrubID,
                 na.action=na.pass)

summary(model_BAI)
