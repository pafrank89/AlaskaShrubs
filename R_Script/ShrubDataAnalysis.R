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

install.packages("Hmisc")
library(Hmisc)

# 1. CORRELATION MATRICIES ####

# Check for correlations between variables
CorPlot_b = subset (sd_bena_cch, select = c("resid", "BAI", "Age", "Elevation", "Slope", "Y_Cord", "CanopyCover", "DistToRoad"))

CorPlot_s = subset (sd_salix_cch, select = c("resid", "BAI", "Age", "Elevation", "Slope", "Y_Cord", "CanopyCover", "DistToRoad"))

#Creates a correlation matrix using the variables specified above
chart.Correlation(CorPlot_b, histogram = TRUE, method = c("spearman"))

chart.Correlation(CorPlot_s, histogram = TRUE, method = c("spearman"))


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
plot(resid ~ factor(SoilText), data = sd_bena_cch, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2,
     pch = 1, ylab = "Residuals of Standardized BAI", xlab = "")

plot(resid ~ factor(SoilText), data = sd_salix_cch, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2,
     pch = 1, ylab = "Residuals of Standardized BAI", xlab = "")

#plot BAI as a function of soil moisture
plot(resid ~ factor(SoilMoist), data = sd_bena_cch, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2,
     pch = 1, ylab = "Residuals of Standardized BAI", xlab = "")

plot(resid ~ factor(SoilMoist), data = sd_salix_cch, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2,
     pch = 1, ylab = "Residuals of Standardized BAI", xlab = "")

#Plot BAI as a function of observed vegetation cover 
plot(resid ~ factor(VegCoverOBS), data = sd_bena_cch, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2,
     pch = 1, ylab = "Residuals of Standardized BAI", xlab = "")

plot(resid ~ factor(VegCoverOBS), data = sd_salix_cch, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2,
     pch = 1, ylab = "Residuals of Standardized BAI", xlab = "")

#Plot BAI as a function of  
ggplot(data = sd_bena_cch, 
       aes(x = resid,
           y = iem.summ.temp)) + 
  geom_point(size = 2) +
  geom_smooth(method= "lm") +
  xlab("Residuals of Standardized BAI") +
  ylab("Mean Summer Temperature")

ggplot(data = sd_bena_cch, 
       aes(x = resid,
           y = iem.summ.rain)) + 
  geom_point(size = 2) +
  geom_smooth(method= "lm") +
  xlab("Residuals of Standardized BAI") +
  ylab("Mean Summer Temperature")

ggplot(data = sd_salix_cch, 
       aes(x = resid,
           y = iem.summ.temp)) + 
  geom_point(size = 2) +
  geom_smooth(method= "lm") +
  xlab("Residuals of Standardized BAI") +
  ylab("Mean Summer Temperature")

ggplot(data = sd_salix_cch, 
       aes(x = resid,
           y = iem.summ.rain)) + 
  geom_point(size = 2) +
  geom_smooth(method= "lm") +
  xlab("Residuals of Standardized BAI") +
  ylab("Mean Summer Precipitation")


ggplot(data = sd_bena_cch, 
       aes(x = resid,
           y = CanopyCover)) + 
  geom_point(size = 2) +
  geom_smooth(method= "lm") +
  xlab("Residuals of Standardized BAI") +
  ylab("Canopy Cover (%)")

ggplot(data = sd_salix_cch, 
       aes(x = resid,
           y = CanopyCover)) + 
  geom_point(size = 2) +
  geom_smooth(method= "lm") +
  xlab("Residuals of Standardized BAI") +
  ylab("Canopy Cover (%)")



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

# 10. PLOT GROWTH TREND OVER TIME ####

# Aggregate growth data by year
sd_BAI_bena_agg = aggregate((x = sd_bena_cch),
                            by = list(sd_bena_cch$Year),
                            FUN = mean,
                            na.rm = TRUE)

sd_BAI_salix_agg = aggregate((x = sd_salix_cch),
                             by = list(sd_salix_cch$Year),
                             FUN = mean,
                             na.rm = TRUE)


sd_all_bena_agg = aggregate((x = sd_all_bena_cch),
                            by = list(sd_all_bena_cch$Year),
                            FUN = mean,
                            na.rm = TRUE)

sd_all_salix_agg = aggregate((x = sd_all_salix_cch),
                             by = list(sd_all_salix_cch$Year),
                             FUN = mean,
                             na.rm = TRUE)

## Plot Aggregated Growth Vatiables together
par(mfrow=c(1,2))

plot(RingWidth ~ Year, data = sd_all_bena_agg, main = "Betula",
     col = "blue", type = "l", ylim=c(-1.5, 1.5), xlim=c(1975, 2020),
     ylab = "Ring Width", xlab = "Year")

par(new=TRUE)

plot(RWI_Spline ~  Year, data = sd_all_bena_agg, 
     col = "red", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-1.5, 1.5), xlim=c(1975, 2020))

par(new=TRUE)

plot(RWI_NegExp ~ Year, data = sd_all_bena_agg,
     col = "green", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-1.5, 1.5), xlim=c(1975, 2020))

par(new=TRUE)

plot(log(BAI) ~ Year, data = sd_all_bena_agg,
     col = "orange", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-1.5, 2), xlim=c(1975, 2020))

par(new=TRUE)

plot(resid ~ Year, data = sd_all_bena_agg,
     col = "purple", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-1.5, 2), xlim=c(1975, 2020))

legend("bottomleft",legend=c("Ring Width","RWI_Spline", "RWI_NegExp", "lnBAI", "BAI Residuals"),
       text.col=c("blue", "red", "green", "orange", "purple"), 
       lty = c(1, 1, 1, 1, 1), col=c("blue", "red", "green", "orange", "purple"), bty = "n", cex=0.75)

# For Salix

plot(RingWidth ~ Year, data = sd_all_salix_agg, main = "Salix",
     col = "blue", type = "l", ylim=c(-.6,1.5), xlim=c(1975, 2020),
     ylab = "Ring Width", xlab = "Year")

par(new=TRUE)

plot(RWI_Spline ~  Year, data = sd_all_salix_agg, 
     col = "red", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-.6,2.5), xlim=c(1975, 2020))

par(new=TRUE)

plot(RWI_NegExp ~ Year, data = sd_all_salix_agg,
     col = "green", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-.6,2.5), xlim=c(1975, 2020))

par(new=TRUE)

plot(log(BAI) ~ Year, data = sd_all_salix_agg,
     col = "orange", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-.6,2.5), xlim=c(1975, 2020))

par(new=TRUE)

plot(resid ~ Year, data = sd_all_salix_agg,
     col = "purple", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-.6,2.5), xlim=c(1975, 2020))

#legend("topright",legend=c("Ring Width","RWI_Spline", "RWI_NegExp", "lnBAI", "BAI Residuals"),
       #text.col=c("blue", "red", "green", "orange", "purple"), 
       #lty = c(1, 1, 1, 1, 1), col=c("blue", "red", "green", "orange", "purple"), bty = "n", cex=0.75)

# 11. PLOT GROWTH TREND BY AGE ####
par(mfrow=c(2,5))

#Betula
plot(RingWidth ~ Age, data = sd_all_bena_cch,
     col = "black", pch = 1, ylab = "Ring Width", xlab = "Ring Age (years)", main = "Betula")

plot(RWI_Spline ~ Age, data = sd_all_bena_cch,
     col = "black", pch = 1, ylab = "Ring Width Index (Spline)", xlab = "Ring Age (years)")

plot(RWI_NegExp ~ Age, data = sd_all_bena_cch,
     col = "black", pch = 1, ylab = "Ring Width Index (Negative Expontial)", xlab = "Ring Age (years)")

plot(log(BAI) ~ Age, data = sd_all_bena_cch,
     col = "black", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)")

plot(resid ~ Age, data = sd_all_bena_cch,
     col = "black", pch = 1, ylab = "Residuals of BAI", xlab = "Ring Age (years)")


#Salix
plot(RingWidth ~ Age, data = sd_all_salix_cch, 
     col = "blue", pch = 1, ylab = "Ring Width", xlab = "Ring Age (years)", main = "Salix")

plot(RWI_Spline ~ Age, data = sd_all_salix_cch, 
     col = "blue", pch = 1, ylab = "Ring Width Index (Spline)", xlab = "Ring Age (years)")

plot(RWI_NegExp ~ Age, data = sd_all_salix_cch,
     col = "blue", pch = 1, ylab = "Ring Width Index (Negative Expontial)", xlab = "Ring Age (years)")

plot(log(BAI) ~ Age, data = sd_all_salix_cch,
     col = "blue", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)")

plot(resid ~ Age, data = sd_all_salix_cch,
     col = "blue", pch = 1, ylab = "Residuals of BAI", xlab = "Ring Age (years)")


# 12. VIOLIN PLOTS 
# 12. PLOT AGE DISTRIBUTION OVER TIME ####

ggplot(sd_all_bena_cch, aes(x=as.factor(Year), y=Age)) +
    geom_violin(trim=FALSE) +
  #geom_boxplot(width=0.1) +
  #stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  stat_summary(fun.y=median, geom="point", size=2, color="red") 
  #stat_summary(fun.data="mean_sdl", mult=1,geom="crossbar", width=0.2 ) +
  #stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="red")
  #geom_jitter(shape=16, position=position_jitter(0.2))


ggplot(sd_all_bena_cch, aes(x=as.factor(Year), y=resid)) +
  geom_violin(trim=FALSE) +
  #geom_boxplot(width=0.1) +
  stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  stat_summary(fun.y=median, geom="point", size=2, color="red") 
  #stat_summary(fun.data="mean_sdl", mult=1,geom="crossbar", width=0.2 ) +
  #stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="red")
  #geom_jitter(shape=16, position=position_jitter(0.2))

# 13. PLOT GROTH TREND AND FIXED EFFECTS OVER TIME ####

## Plot standardized BAI residuals against mean summer temperature
# Start by adding extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

# 13.1 Plot first set of data and draw its axis
plot(sd_BAI_bena_agg$iem.summ.temp ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, ylim=c(10,14), #main="Shrub Growth ",
     type = "l", xlab = "", ylab = "", 
     col = "red", lwd = 1, cex.lab = 1)

axis(2, ylim=c(9,15),col="black",las=1)

mtext("Mean Summer Temperature",side=2,line=2.5)

box()

# Allow a second plot on the same graph
par(new=TRUE)

# Plot the second plot and put axis scale on right
plot(sd_BAI_bena_agg$resid ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, ylim=c(-.7, .7),
     type = "l", xlab = "", ylab = "", 
     col = "black", lwd = 1, lty = 5, cex.lab = 1)

# Allow for a third plot using the second accis
par(new=TRUE)

# Plot the third plot and put axis scale on right
plot(sd_BAI_salix_agg$resid ~ sd_BAI_salix_agg$Year, 
     axes=FALSE, ylim=c(-.7, .7),
     type = "l", xlab = "", ylab = "", 
     col = "grey 52", lwd = 1, lty = 2, cex.lab = 1)

mtext("Age Standardized BAI", side=4,col="black",line=4) 

axis(4, ylim=c(-.7, .7), col="black", col.axis="black", las=1)

# Add the Years axis
axis(1,pretty(range(sd_BAI_bena_agg$Year),5))
mtext("Year",side=1, col="black", line=2.5) 

## Add Legend
legend("topright",legend=c("Temperature","Betula", "Salix"),
       text.col=c("red", "black", "grey 52"), lty = c(1, 5, 2), col=c("red", "black", "dark grey"), bty = "n")


# 13.2 Plot raw BAI against mean summer Precip

# Start by adding extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

# Plot first set of data and draw its axis
plot(sd_BAI_bena_agg$iem.summ.rain.10 ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, ylim=c(7,37), #main="Shrub Growth ",
     type = "l", xlab = "", ylab = "", 
     col = "blue", lwd = 1, cex.lab = 1)

axis(2, ylim=c(9,15),col="black",las=1)

mtext("Mean Summer Temperature",side=2,line=2.5)

box()

# Allow a second plot on the same graph
par(new=TRUE)

# Plot the second plot and put axis scale on right
plot(sd_BAI_bena_agg$resid ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, ylim=c(-.7, .7),
     type = "l", xlab = "", ylab = "", 
     col = "black", lwd = 1, lty = 5, cex.lab = 1)

# Allow for a third plot using the second accis
par(new=TRUE)

# Plot the third plot and put axis scale on right
plot(sd_BAI_salix_agg$resid~ sd_BAI_salix_agg$Year, 
     axes=FALSE, ylim=c(-.7, .7),
     type = "l", xlab = "", ylab = "", 
     col = "grey 52", lwd = 1, lty = 2, cex.lab = 1)

mtext("Age Standardized BAI", side=4,col="black",line=4) 

axis(4, ylim=c(-.7, .7), col="black", col.axis="black", las=1)

# Add the Years axis
axis(1,pretty(range(sd_BAI_salix_agg$Year),5))
mtext("Year",side=1, col="black", line=2.5) 

## Add Legend
legend("topright",legend=c("Precipitation","Betula", "Salix"),
       text.col=c("blue", "black", "grey 52"), lty = c(1, 5, 2), col=c("blue", "black", "dark grey"), bty = "n")


# 13.3 Plot Betula standardized BAI residuals against Hare Cycles 

# Start by adding extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

# Plot first set of data and draw its axis
plot(sd_BAI_bena_agg$HareIndex ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, ylim=c(0,4), main="",
     type = "l", xlab = "", ylab = "", 
     col = "forest green", lwd = 1, cex.lab = 1)

axis(2, ylim=c(1,3),col="black",las=1)

mtext("Hare Cycle Index",side=2,line=2.5)

box()

# Allow a second plot on the same graph
par(new=TRUE)

# Plot the second plot and put axis scale on right
plot(sd_BAI_bena_agg$resid ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, ylim=c(-.7, .7),
     type = "l", xlab = "", ylab = "", 
     col = "black", lwd = 1, lty = 5, cex.lab = 1)

# Allow for a third plot using the second accis
par(new=TRUE)

# Plot the third plot and put axis scale on right
plot(sd_BAI_salix_agg$resid~ sd_BAI_salix_agg$Year, 
     axes=FALSE, ylim=c(-.7, .7),
     type = "l", xlab = "", ylab = "", 
     col = "grey 52", lwd = 1, lty = 2, cex.lab = 1)

mtext("Residuals of Standardized BAI", side=4,col="black",line=4) 

axis(4, ylim=c(-.7, .7), col="black", col.axis="black", las=1)

# Add the Years axis
axis(1,pretty(range(sd_BAI_bena_agg$Year),5))
mtext("Year",side=1, col="black", line=2.5) 


# 14. PLOT MIXED EFFECTS ACROSS SITES ####

ggplot(sd_bena_cch, aes(x = iem.summ.temp, y = resid, colour = ShrubID)) +
  facet_wrap(~Section, nrow=4) +   # a panel for each mountain range
  geom_point(alpha = 0.5) +
  theme_classic() +
  geom_line(data = cbind(sd_bena_cch, pred = predict(CH1H2_model_b)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"))  # adding space between panels)

ggplot(sd_salix_cch, aes(x = iem.summ.temp, y = resid, colour = ShrubID)) +
  facet_wrap(~Section, nrow=4) +   # a panel for each mountain range
  geom_point(alpha = 0.5) +
  theme_classic() +
  geom_line(data = cbind(sd_salix_cch, pred = predict(CH1_model_s)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"))  # adding space between panels)

lme_rain_b = lmer(resid ~ iem.summ.rain.10 + (1 + iem.summ.rain.10|Section/ShrubID), data = sd_bena_cch)

lme_rain_s = lmer(resid ~ iem.summ.rain.10 + (1 + iem.summ.rain.10|Section/ShrubID), data = sd_salix_cch)

ggplot(sd_bena_cch, aes(x = iem.summ.rain.10, y = resid, colour = ShrubID)) +
  facet_wrap(~Section, nrow=2) +   # a panel for each mountain range
  geom_point(alpha = 0.5) +
  theme_classic() +
  geom_line(data = cbind(sd_bena_cch, pred = predict(lme_rain_b)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"))  # adding space between panels)

ggplot(sd_salix_cch, aes(x = iem.summ.rain.10, y = resid, colour = ShrubID)) +
  facet_wrap(~Section, nrow=2) +   # a panel for each mountain range
  geom_point(alpha = 0.5) +
  theme_classic() +
  geom_line(data = cbind(sd_salix_cch, pred = predict(lme_rain_s)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"))  # adding space between panels)


ggplot(sd_salix_cch) + 
  aes(x = iem.summ.temp, y = resid) + 
  stat_smooth(method = "lm", se = FALSE) +
  # Put the points on top of lines
  geom_point() +
  facet_wrap("Section") +
  labs(x = "Mean Summer Temperature", y = "Residuals of standardized BAI") + 
  # We also need to help the x-axis, so it doesn't 
  # create gridlines/ticks on 2.5 days
  scale_x_continuous(breaks = 0:4 * 2)

ggplot(sd_bena_cch) + 
  aes(x = iem.summ.temp, y = resid) + 
  stat_smooth(method = "lm", se = FALSE) +
  # Put the points on top of lines
  geom_point() +
  facet_wrap("Section") +
  labs(x = "Mean Summer Temperature", y = "Residuals of standardized BAI") + 
  # We also need to help the x-axis, so it doesn't 
  # create gridlines/ticks on 2.5 days
  scale_x_continuous(breaks = 0:4 * 2)



