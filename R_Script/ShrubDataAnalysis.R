# Master's Thesis Data Analysis
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-10-06

library(tidyverse)
library(dlpr)
library(ggplot2)
library(GGally)
library(reshape2)
library(dplyr)
library(lme4)
library(nlme)
library(PerformanceAnalytics)
library(corrplot)
library(RColorBrewer)
library(gamlss)



# 1. CORRELATION MATRICIES ####

# Check for correlations between variables
CorPlot = subset (sd_final_cch, select = c("Age", "MooseDensity", "HareIndex", "PropMoose", "PropHare", "PropPtarmagin", "PropMoose_S", "PropHare_S", "PropPtarmagin_S"))

#Creates a correlation matrix using the variables specified above
chart.Correlation(CorPlot, histogram = TRUE, method = c("spearman"))

#Creates a color coded correlation matrix using the variables specified above
sd_final_cchCorr = cor(CorPlot)

corrplot(sd_final_cchCorr , type="upper", order="hclust",
         col=brewer.pal(n=9, name="RdYlBu"))

#All
#("resid", "MooseDensity", "HareIndex", "PropMoose", "PropHare", iem.summ.temp", "iem.temp", "iem.summ.rain", "iem.wint.rain", summ.temp", "temp", "summ.min", "summ.max", "summ.rain", "wint.rain", "pet", "wet", "frost", "Elevation", "Slope", "Y_Cord", "CanopyCover", "DistToRoad")

#Browsing Variables 
#("resid", "MooseDensity", "HareIndex", "PropMoose", "PropHare")

#IEM Climate and BAI correlations
#(resid",iem.summ.temp", "iem.temp", "iem.summ.rain", "iem.wint.rain")

#CRU Climate and BAI correlations
#("resid", summ.temp", "temp", "summ.min", "summ.max", "summ.rain", "wint.rain", "pet", "wet", "frost")

#Age and BAI correlations
#("resid", "resid_t", "BAI", "Age")

#Environmental Covariates and BAI
#("resid", "resid_t", "BAI", "Age", "Elevation", "Slope", "Y_Cord", "CanopyCover", "DistToRoad")

#IEM Climate and Browsing correlations
#("MooseDensity", "HareIndex", "PropMoose", "PropHare","iem.summ.temp", "iem.temp", "iem.summ.rain", "iem.wint.rain")

#CRU Climate and Browsing correlations
#("MooseDensity", "HareIndex", "PropMoose", "PropHare","summ.temp", "temp", "summ.min", "summ.max", "summ.rain", "wint.rain", "pet", "wet", "frost")


# 2. CORRELATION MATRICIES BY SPECIES ####

sd_bena_cch = subset(sd_final_cch, Species == "BENA")

sd_salix_cch = subset(sd_final_cch, grepl("^SA", sd_final_cch$Species)) 

# Check for correlations between variables
CorPlot_bena = subset (sd_bena_cch, select = c("BAI", "resid", "iem.summ.temp", "iem.temp", "iem.summ.rain", "MooseDensity", "HareIndex", "PropMoose", "PropHare", "PropPtarmagin", "Elevation", "Slope", "Y_Cord", "DistToRoad"))

CorPlot_salix = subset (sd_salix_cch, select = c("BAI", "resid", "iem.summ.temp", "iem.temp", "iem.summ.rain", "MooseDensity", "HareIndex", "PropMoose", "PropHare", "PropPtarmagin", "Elevation", "Slope", "Y_Cord", "DistToRoad"))

#Creates a correlation matrix using the variables specified above
chart.Correlation(CorPlot_bena, histogram = TRUE, method = c("spearman"))

chart.Correlation(CorPlot_salix, histogram = TRUE, method = c("spearman"))


# 3. BOX PLOTS ####
#Plot BAI as a function of soil texture
plot(resid ~ factor(SoilText), data = sd_final_cch, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2,
     pch = 1, ylab = "Basal Area Increment", xlab = "")

#plot BAI as a function of soil moisture
plot(resid ~ factor(SoilMoist), data = sd_final_cch, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2,
     pch = 1, ylab = "Basal Area Increment", xlab = "")

#Plot BAI as a function of observed vegetation cover 
plot(resid_t ~ factor(VegCoverOBS), data = sd_final_cch, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2,
     pch = 1, ylab = "Basal Area Increment", xlab = "")

# 4. STEM HEIGHT ~ STEM DIAMETER & Y CORD #####
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

# 5. STEM HEIGHT ~ BROWSING DENSITY PLOT FOR SUB-SETTED DATA ####

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

# 6. STEM HEIGHT ~ BROWSING DENSITY PLOT FOR FULL DATA ####
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
  ggplot(aes(x=StemHeight, group=Species, colour=Species, fill= Species, xtitle = "Shrub Height (cm)")) +
  geom_density(alpha = 0.2) +
  scale_x_continuous (name = "Shrub Height (cm)") +
  scale_y_continuous (name = "Frequency") +
  theme_bw()

# 7. BROWSING DATA ~ ENVIRONMENTAL COVARIATES ####
# Assess the relationship beteween browsing data and other variables across the 23 sampled sections

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

# 8. TEMPORAL HERBIVORE DATA VALIDATION ####
# The objective is to see if our spatial browsing intensity and feces trasect data jives with the spatial trend in temporal data 
str(Section_Data)

# Check for correlations between variables
HerbiPlot = subset (Section_Data, select = c("PropMoose", "MooseFeces", "AvgMooseDensity", "2015MooseDensity", "PropHare", "HareFeces", "PropPtarmagin", "PtarmaginFeces"))

#Creates a correlation matrix using the variables specified above
chart.Correlation(HerbiPlot, histogram = TRUE, method = c("spearman"))

plot(HerbiPlot$PropMoose ~ HerbiPlot$`2015MooseDensity`)

plot(HerbiPlot$MooseFeces ~ HerbiPlot$`2015MooseDensity`)

# 9. PLOT ENVIRONEMNTAL VARIABLES ALONG Y-CORD #### 

par(mfrow=c(6,1), omi=c(1,0,0,0), plt=c(0.1,0.9,0,0.8)) 

#par(mfrow=c(1,1))

plot(Section_Data$PropMoose ~ Section_Data$Y_Cord, 
     type = "b", pch = 1, col = "green", lty = 1,
     xaxt='n', frame.plot = FALSE,
     ylab = "% Twigs 
     Browsed", xlab = "")

lines(Section_Data$Y_Cord, Section_Data$PropHare, type = "b", pch = 1, col = "red", lty = 2)
lines(Section_Data$Y_Cord, Section_Data$PropPtarmagin, type = "b", pch = 1, col = "blue", lty = 3)

legend("topleft", legend=c("Moose", "Hare", "Ptarmagin"),
       col=c("green", "red", "blue"), lty=1:3, cex=0.8, bty = "n")

mtext("Browsing Pressure", side= 3, line = -1, adj = 1, padj = 0, cex=0.6)

    #plot(Section_Data$PropHare ~ Section_Data$Y_Cord, 
         #type = "b", pch = 1, col = "green", lty = 1,
         #xaxt='n', frame.plot = FALSE,
         #ylab = "% Twigs", xlab = "")

    #plot(Section_Data$PropPtarmagin ~ Section_Data$Y_Cord, 
         #type = "b", pch = 1, col = "green", lty = 1,
         #xaxt='n', frame.plot = FALSE,
         #ylab = "% Twigs", xlab = "")
    
plot(Section_Data$CanopyCover ~ Section_Data$Y_Cord,
     type = "b", pch = 1, col = "forest green", 
     xaxt='n', frame.plot = FALSE,
     ylab = "%", xlab = "")

#mtext("Canopy Cover", side= 4, line = 1, cex=0.6)

mtext("Canopy Cover", side= 3, line = -3, adj = 1, cex=0.6)

plot(Section_Data$StemHeight ~ Section_Data$Y_Cord,
     type = "b", pch = 1, col = "forest green", 
     xaxt='n', frame.plot = FALSE,
     ylab = "cm", xlab = "")

mtext("Canopy Height", side= 3, line = -2.5, adj = 1, padj = 0, cex=0.6)

plot(Section_Data$iem.summ.rain ~ Section_Data$Y_Cord,
     type = "b", pch = 1, col = "dark blue", 
     xaxt='n', frame.plot = FALSE,
     ylab = "mm", xlab = "")

mtext("Mean Summer Precipitation", side= 3, line = -1.25, adj = 1, padj = 0, cex=0.6)

plot(Section_Data$iem.summ.temp ~ Section_Data$Y_Cord,
     type = "b", pch = 1, col = "red", 
     xaxt='n', frame.plot = FALSE,
     ylab = "°C", xlab = "")

mtext("Mean Summer Temperature", side= 3, line = -2, adj = 1, padj = 0, cex=0.6)

plot(Section_Data$Elevation ~ Section_Data$Y_Cord,
     type = "b", pch = 1, col = "black", 
     frame.plot = FALSE,
     ylab = "m", xlab = "Latitude")

mtext("Elevation", side= 3, line = -.5, adj = 1, padj = 0, cex=0.6)
mtext("Latitude", side= 1, line = 3, cex=0.7)

# 10. PLOT BAI TREND OVER TIME ####

sd_BAI_bena_agg = aggregate((x = sd_bena_res),
                            by = list(sd_bena_res$Year),
                            FUN = mean,
                            na.rm = TRUE)



sd_BAI_salix_agg = aggregate((x = sd_salix_res),
                             by = list(sd_salix_res$Year),
                             FUN = mean,
                             na.rm = TRUE)



sd_BAI_agg = aggregate((x = sd_all),
                       by = list(sd_all$Year),
                       FUN = mean,
                       na.rm = TRUE)

par(mfrow=c(1,1), omi=c(0.5,0.3,0,0), plt=c(0.1,0.9,0,0.7)) 

plot(sd_BAI_bena_agg$BAI ~ sd_BAI_bena_agg$Year, type = "l", xaxt='n', main="Betula", ylab = "BAI", xlim = c(1987, 2015))

plot(sd_BAI_salix_agg$BAI ~ sd_BAI_salix_agg$Year, type = "l", xaxt='n', main="Salix", ylab = "BAI", xlim = c(1987, 2015))

plot(sd_BAI_agg$BAI ~ sd_BAI_agg$Year, type = "l", xlab = "Year", ylab = "BAI", xlim = c(1987, 2015))

plot(HareCycle$HareIndex ~ HareCycle$Year, type = "l", main="Snowshow Hare Population Index", xlab = "Year", ylab = "Hare Index", xlim = c(1987, 2015))



