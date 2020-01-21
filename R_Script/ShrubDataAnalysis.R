# Master's Thesis Data Analysis
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-10-06

library(tidyverse)
library(dlpr)
library(ggplot2)
library(GGally)
library(reshape2)

#### PAIRS PLOTS FOR COLINEARITY AMONG VARIABLES ####
#This code will assess trends and relationships in the dataset

#Subsets the Shrub dataset to retain only the necessary fields 
PairsData = subset(ShrubData, select = c("StemLength", "VertHeight", "CanopyCover", 
                                         "PropMoose", "PropHare","PropPtarmagin",
                                         "Y_Cord", "Elevation", "Aspect", "Slope", "Y_Cord"))

#Establishes parameters which will be applied to each plot in the pairs diagram
lowerFn <- function(data, mapping, method = "lm") 
  p <- ggplot(data = data, mapping = mapping) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_smooth(method = method, color = "red")

#Plots all variables in PairsData against one another, lists a correlation coefficient and fits a regression line 
ggpairs(PairsData [, 1:11], lower = list(continuous = wrap (lowerFn, method = "lm")))




#### CORRELATION OF MEASURED BROWSING AND ENVIRONMENTAL COVARIATES ####
# Assess the relationship beteween feces counts and other variables across the 23 sampled sections

ggplot(data = SectionData, 
       aes(x = Y_Cord,
           y = Prop_TwigsBrowsed)) + 
  geom_smooth(method=lm) + 
  geom_point(size = 2) +
  xlab("Latitude") + ylab("Proportion of Twigs Browsed")

ggplot(data = SectionData, 
       aes(x = Elevation,
           y = Prop_TwigsBrowsed)) + 
  geom_smooth(method=lm) + 
  geom_point(size = 2) +
  xlab("Elevation") + ylab("Proportion of Twigs Browsed")


#### HEIGHT ~ BROWSING INTENSITY DENSITY PLOT ####
#This section of code will create a density plot showing Shrub Vertical Height on the X-axis and the frequency of 
#browsing at that height for each of the three study species. 

#Select only pertinent columns from the larger ShrubData file 
SD_BiBrowse = select(ShrubData, "ShrubID", "StemLength", "VertHeight", "BinaryMoose", "BinaryHare", "BinaryPtarmagin")

subset (sd_final_cch, select = c("resid_t", "MooseDensity", "HareIndex", "PropMoose", "PropHare", "iem.summ.temp", "iem.temp", "iem.summ.rain", "iem.wint.rain", "summ.temp", "temp", "summ.min", "summ.max", "summ.rain", "wint.rain", "pet", "wet", "frost", "Elevation", "Slope", "Y_Cord", "CanopyCover", "DistToRoad"))

#Specify the data frame as a table prior to the melt function
SD_BiBrowseTable= as.data.table(SD_BiBrowse)

#Melt the data retaining the ShrubID, StemLength & VertHeight variables while melting out the browsing varibales
SD_BiBrowseMelt = melt.data.table(SD_BiBrowseTable, id.vars = c("ShrubID", "StemLength", "VertHeight"), 
                                  measure.vars = c("BinaryMoose", "BinaryHare", "BinaryPtarmagin"))
#Rename the melt output values field to Species
names(SD_BiBrowseMelt)[5]<-"Species"

#Subsets the output data from the melt to retain only the necessary fields 
DP_Data = subset(SD_BiBrowseMelt, select = c("ShrubID", "StemLength", "VertHeight", "Species"))

#Plot the density plot using ggplot2
DP_Data %>% 
  filter(Species == c("Hare", "Moose", "Ptarmagin")) %>% 
  ggplot(aes(x=VertHeight, group=Species, colour=Species, fill= Species, xtitle = "Shrub Height (cm)")) +
  geom_density(alpha = 0.2) +
  scale_x_continuous (name = "Shrub Height (cm)") +
  scale_y_continuous (name = "Frequency") +
  theme_bw()

#### LINEAR REGRESSION: BROWSING ~ SHRUB HEIGHT ####

lmHeightMoose = lm(ShrubData$PropMooseBrowse ~ ShrubData$VertHeight) 
lmHeightHare = lm(ShrubData$PropHareBrowse ~ ShrubData$VertHeight) 
lmHeightPtar = lm(ShrubData$PropPtarmaginBrowse ~ ShrubData$VertHeight) 

summary(lmHeightMoose)
summary(lmHeightHare)
summary(lmHeightPtar)

plot(PropMooseBrowse ~ VertHeight, data = ShrubData,
     col = "blue", pch = 1, ylab = "Proportion of Twigs Browsed", xlab = "Vertical Height")
points(PropHareBrowse ~ VertHeight, data = ShrubData,
       col = "green", pch = 8)
points(PropPtarmaginBrowse ~ VertHeight, data = ShrubData,
       col = "red", pch = 2)
abline(lmHeightMoose, col = "blue")
abline(lmHeightHare, col = "green")
abline(lmHeightPtar, col = "red")

legend("topright", legend=c("Moose Browse", "Hare Browse", "Ptarmagin Browse"),
       col=c("blue", "green", "red"),  pch = c(1,8,2), cex=0.8)


ggplot(data = ShrubData, 
       aes(x = VertHeight,
           y = Y_Cord,
           colour=Species)) + 
  geom_smooth(method=lm) + 
  geom_point(size = 2) +
  ggtitle("Shrub Vertical Height vs. Latitude") +
  xlab("Vertical Height (cm)") + ylab("Latitude")
