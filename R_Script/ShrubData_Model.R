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

# 1. CORRELATION MATRICIES ####

# Check for correlations between variables
CorPlot = subset (sd_final_cch, select = c("resid", "MooseDensity", "HareIndex", "PropMoose", "PropHare"))

#Creates a correlation matrix using the variables specified above
chart.Correlation(CorPlot, histogram = TRUE, method = c("spearman"))

#Creates a color coded correlation matrix using the variables specified above
sd_final_cchCorr = cor(CorPlot)

corrplot(sd_final_cchCorr , type="upper", order="hclust",
         col=brewer.pal(n=9, name="RdYlBu"))

#All
#("resid_t", "MooseDensity", "HareIndex", "PropMoose", "PropHare", iem.summ.temp", "iem.temp", "iem.summ.rain", "iem.wint.rain", summ.temp", "temp", "summ.min", "summ.max", "summ.rain", "wint.rain", "pet", "wet", "frost", "Elevation", "Slope", "Y_Cord", "CanopyCover", "DistToRoad")

#Browsing Variables 
  #("resid_t", "MooseDensity", "HareIndex", "PropMoose", "PropHare")

#IEM Climate and BAI correlations
  #(resid_t",iem.summ.temp", "iem.temp", "iem.summ.rain", "iem.wint.rain")

#CRU Climate and BAI correlations
#("resid_t", summ.temp", "temp", "summ.min", "summ.max", "summ.rain", "wint.rain", "pet", "wet", "frost")

#Age and BAI correlations
  #("resid", "resid_t", "BAI", "Age")

#Environmental Covariates and BAI
  #("resid", "resid_t", "BAI", "Age", "Elevation", "Slope", "Y_Cord", "CanopyCover", "DistToRoad")

#IEM Climate and Browsing correlations
#("MooseDensity", "HareIndex", "PropMoose", "PropHare","iem.summ.temp", "iem.temp", "iem.summ.rain", "iem.wint.rain")

#CRU Climate and Browsing correlations
#("MooseDensity", "HareIndex", "PropMoose", "PropHare","summ.temp", "temp", "summ.min", "summ.max", "summ.rain", "wint.rain", "pet", "wet", "frost")


# 2. BOX PLOTS ####
#Plot BAI as a function of soil texture
plot(resid_t ~ factor(SoilText), data = sd_final_cch, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2,
     pch = 1, ylab = "Basal Area Increment", xlab = "")

#plot BAI as a function of soil moisture
plot(resid_t ~ factor(SoilMoist), data = sd_final_cch, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2,
     pch = 1, ylab = "Basal Area Increment", xlab = "")

#Plot BAI as a function of observed vegetation cover 
plot(resid_t ~ factor(VegCoverOBS), data = sd_final_cch, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2,
     pch = 1, ylab = "Basal Area Increment", xlab = "")

# 3. STEM HEIGHT ~ STEM DIAMETER & Y CORD #####
#Plot Height ad a function of Stem Diameter
plot(StemHeight ~ StemDiam, data = sd_final_cch,
     col = "black", pch = 1, ylab = "Diameter", xlab = "Stem Height")

ggplot(data = sd_final_cch, 
       aes(x = StemDiam,
           y = StemHeight,
           colour=Species)) + 
  geom_point(size = 2) +
  geom_smooth(method= "lm") +
  xlab("Diameter (mm") + ylab("Height (cm)")

ggplot(data = sd_final_cch, 
       aes(x = StemHeight,
           y = Y_Cord,
           colour=Species)) + 
  geom_smooth(method=lm) + 
  geom_point(size = 2) +
  ggtitle("Shrub Vertical Height vs. Latitude") +
  xlab("Vertical Height (cm)") + ylab("Latitude")

# 4. STEM HEIGHT ~ BROWSING DENSITY PLOT ####

lmHeightMoose = lm(sd_final_cch$PropMoose ~ sd_final_cch$StemHeight) 
lmHeightHare = lm(sd_final_cch$PropHare ~ sd_final_cch$StemHeight) 
lmHeightPtar = lm(sd_final_cch$PropPtarmagin ~ sd_final_cch$StemHeight) 

summary(lmHeightMoose)
summary(lmHeightHare)
summary(lmHeightPtar)

plot(PropMoose ~ StemHeight, data = sd_final_cch,
     col = "blue", pch = 1, ylab = "Proportion of Twigs Browsed", xlab = "Vertical Height")
points(PropHare ~ StemHeight, data = sd_final_cch,
       col = "green", pch = 8)
points(PropPtarmagin ~ StemHeight, data = sd_final_cch,
       col = "red", pch = 2)
abline(lmHeightMoose, col = "red")
abline(lmHeightHare, col = "green")
abline(lmHeightPtar, col = "blue")

legend("topright", legend=c("Moose Browse", "Hare Browse", "Ptarmagin Browse"),
       col=c("green", "red", "blue"),  pch = c(1,8,2), cex=0.8)

#This section of code will create a density plot showing Shrub Vertical Height on the X-axis and the frequency of 
#browsing at that height for each of the three study species. 

#Select only pertinent columns from the larger ShrubData file 
sd_bibrowse = subset(shrub_data_ss, select = c("ShrubID", "Species", "StemLength", "StemHeight", "Bin_Moose", "Bin_Hare", "Bin_Ptarmagin"))

  sd_bibrowse_bena = subset(sd_bibrowse, Species == "BENA")

  sd_bibrowse_salix = subset(sd_bibrowse, grepl("^SA", sd_bibrowse$Species))

#Melt the data retaining the ShrubID, StemLength & VertHeight variables while melting out the browsing varibales
sd_bibrowse_melt = melt(sd_bibrowse, id.vars = c("ShrubID", "StemLength", "StemHeight"), 
                        measure.vars = c("Bin_Moose", "Bin_Hare", "Bin_Ptarmagin"))

#Rename the melt output values field to Species
names(sd_bibrowse_melt )[5]<-"Species"

#Subsets the output data from the melt to retain only the necessary fields 
DP_Data = subset(sd_bibrowse_melt, select = c("ShrubID", "StemLength", "StemHeight", "Species"))

# reassign depth values under 10 to zero
#DP_Data$Species[DP_Data$Species>0] <- 1

#Plot the density plot using ggplot2
DP_Data %>% 
  filter(Species == c("Moose", "Hare", "Ptarmagin")) %>% 
  ggplot(aes(x=StemHeight, group=Species, colour=Species, fill= Species, xtitle = "Shrub Height (cm)")) +
  geom_density(alpha = 0.2) +
  scale_x_continuous (name = "Shrub Height (cm)") +
  scale_y_continuous (name = "Frequency") +
  theme_bw()

# 5. TEMPORAL HERBIVORE DATA VALIDATION ####
# The objective is to see if our spatial browsing intensity and feces trasect data jives with the spatial trend in temporal data 
str(Section_Data)

# Check for correlations between variables
HerbiPlot = subset (Section_Data, select = c("PropMoose", "MooseFeces", "AvgMooseDensity", "2015MooseDensity", "PropHare", "HareFeces", "PropPtarmagin", "PtarmaginFeces"))

#Creates a correlation matrix using the variables specified above
chart.Correlation(HerbiPlot, histogram = TRUE, method = c("spearman"))

plot(HerbiPlot$PropMoose ~ HerbiPlot$`2015MooseDensity`)

plot(HerbiPlot$MooseFeces ~ HerbiPlot$`2015MooseDensity`)

# 6. PLOT ENVIRONEMNTAL VARIABLES ALONG Y-CORD #### 

par(mfrow=c(5,1), omi=c(0.5,0.3,0,0), plt=c(0.1,0.9,0,0.7)) 

plot(Section_Data$Elevation ~ Section_Data$Y_Cord, type = "l", xaxt='n', main="Elevation", ylab = "Meters")
plot(Section_Data$iem.temp ~ Section_Data$Y_Cord, type = "l", xaxt='n', main="IEM Temperature", ylab = "°C")
plot(Section_Data$temp ~ Section_Data$Y_Cord, type = "l", xaxt='n', main="CRU-TS Temperature", ylab = "°C")
plot(Section_Data$iem.summ.rain ~ Section_Data$Y_Cord, type = "l", xaxt='n', main="IEM Summer Precipitation", ylab = "mm")
plot(Section_Data$summ.rain ~ Section_Data$Y_Cord, type = "l", main="CRU-TS Summer Precipitation", ylab = "mm", xlab = "Latitude")

plot(Section_Data$PropMoose ~ Section_Data$Y_Cord, type = "l")
plot(Section_Data$PropHare ~ Section_Data$Y_Cord, type = "l")
plot(Section_Data$PropPtarmagin ~ Section_Data$Y_Cord, type = "l")

# 7. DEVELOP MIXED EFFECTS MODELS ####

#Model Structure
#lme(fixed, data, random, correlation, weights, subset, method,
# na.action, control, contrasts = NULL, keep.data = TRUE)

#work with residuals 

str(sd_final_cch)

model_BAI <- lme(resid_t ~ temp + PropMoose + PropHare, data = sd_final_cch, random = ~ 1|Section/ShrubID,
                 na.action=na.pass, control = lmeControl(opt = "optim"))

summary(model_BAI)
