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
library(heplots)
library(Hmisc)
library(data.table)

# 1. CORRELATION MATRICIES ####

# Check for correlations between variables
CorPlot_b = subset (sd_bena_cch, select = c("resid", "MooseDensity", "HareIndex", "PropMoose", "PropHare", "PropPtarmagin", "iem.summ.temp", "iem.summ.rain.10","iem.wint.rain", "Elevation", "Slope", "Aspect", "Y_Cord", "CanopyCover", "DistToRoad"))

CorPlot_s = subset (sd_salix_cch, select = c("resid", "MooseDensity", "HareIndex", "PropMoose", "PropHare", "PropPtarmagin", "iem.summ.temp", "iem.summ.rain.10", "iem.wint.rain", "Elevation", "Slope", "Aspect", "Y_Cord", "CanopyCover", "DistToRoad"))


#CorPlot_b = subset (sd_bena_cch, select = c("PropMoose", "MooseFeces_S", "PropHare", "HareFeces _S", "PropPtarmagin", "PtarmaginFeces_S"))

#CorPlot_s = subset (sd_salix_cch, select = c( "PropMoose", "MooseFeces_S", "PropHare", "HareFeces _S", "PropPtarmagin", "PtarmaginFeces_S"))

# 1. CORRELATION MATRICIES ####

# Check for correlations between variables
CorPlot_b = subset (sd_bena_cch, select = c("resid", "MooseDensity", "HareIndex", "PropMoose", "PropHare", "iem.summ.temp", "iem.temp", "iem.summ.rain", "iem.wint.rain", "Elevation", "Slope", "Y_Cord", "CanopyCover", "DistToRoad"))

CorPlot_s = subset (sd_salix_cch, select = c("resid", "MooseDensity", "HareIndex", "PropMoose", "PropHare", "iem.summ.temp", "iem.temp", "iem.summ.rain", "iem.wint.rain", "Elevation", "Slope", "Y_Cord", "CanopyCover", "DistToRoad"))

#Creates a correlation matrix using the variables specified above
chart.Correlation(CorPlot_b, histogram = TRUE, method = c("spearman"))

chart.Correlation(CorPlot_s, histogram = TRUE, method = c("spearman"))


#Creates a color coded correlation matrix using the variables specified above
cchCorrb = cor(CorPlot_b)

cchCorrs = cor(CorPlot_s)

corrplot.mixed(cchCorrb, lower.col = "black", number.cex = .7, tl.cex = .7)

corrplot.mixed(cchCorrs, lower.col = "black", number.cex = .7, tl.cex = .7)

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

cor(sd_bena_cch$resid, sd_bena_cch$Year)

cor(sd_salix_cch$resid, sd_salix_cch$Year)

dt <- data.table(sd_bena_cch)
dtCor <- dt[, .(mCor = cor(dt$resid, dt$Year)), by=dt$Section]

dtCor = by(sd_bena_cch, sd_bena_cch$Section, FUN = function(X) cor(dt$resid, dt$resid, method = "pearson"))

dtCor

# 3. BOX PLOTS ####
#Plot BAI as a function of soil texture
par(mar=c(4,9,2,1)) 

plot(resid ~ factor(SoilText), data = sd_bena_cch, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2, 
     pch = 1, ylab = "Residuals of Standardized BAI", xlab = "")

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
par(mar=c(4,12,2,1)) 

plot(resid ~ factor(VegCoverOBS), data = sd_bena_cch, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2,
     pch = 1, ylab = "Residuals of Standardized BAI", xlab = "")

veg_b_anova = aov(resid ~ VegCoverOBS, data = sd_bena_cch)

summary(veg_b_anova)

# ANOVA equivalent to R2
etasq(veg_b_anova, partial = FALSE)

plot(resid ~ factor(VegCoverOBS), data = sd_salix_cch, notch = TRUE,varwidth = TRUE,
     col = "white", border = "black", horizontal = TRUE, las = 2,
     pch = 1, ylab = "Residuals of Standardized BAI", xlab = "")

veg_s_anova = aov(iem.summ.rain.10 ~ VegCoverOBS, data = sd_salix_cch)

summary.aov(veg_s_anova)

etasq(veg_s_anova, partial = FALSE)

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
SD_BiBrowse = subset(ShrubData, select = c("ShrubID", "StemLength", "StemHeight", "BMoose", "BHare", "BPtarmagin"))

#Specify the data frame as a table prior to the melt function
SD_BiBrowseTable= as.data.table(SD_BiBrowse)

#Melt the data retaining the ShrubID, StemLength & VertHeight variables while melting out the browsing varibales
SD_BiBrowseMelt = melt.data.table(SD_BiBrowseTable, id.vars = c("ShrubID", "StemLength", "StemHeight"), 
                                  measure.vars = c("BMoose", "BHare", "BPtarmagin"))

#Rename the melt output values field to Species
names(SD_BiBrowseMelt)[5]<-"Species"

#Subsets the output data from the melt to retain only the necessary fields 
DP_Data = subset(SD_BiBrowseMelt, select = c("ShrubID", "StemLength", "StemHeight", "Species"))

#Plot the density plot using ggplot2
P = DP_Data %>% 
  filter(Species == c("Hare", "Moose", "Ptarmagin")) %>% 
  ggplot(aes(x=StemHeight, group=Species, colour=Species, fill= Species, xtitle = "Shrub Height (cm)")) +
  geom_density(alpha = 0.2) +
  scale_x_continuous (name = "Shrub Height (cm)") +
  scale_y_continuous (name = "Frequency") +
  theme_bw()

P + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          axis.text=element_text(size=14), axis.title=element_text(size=16),
          legend.position = c(0.8, 0.8), legend.title = element_text( size=16), legend.text = element_text( size=14))


# 7. BROWSING DATA ~ ENVIRONMENTAL COVARIATES ####
# Assess the relationship beteween browsing data and other variables across the 23 sampled sections

str(Section_Data)

CorPlot_browse = subset (Section_Data, select = c("PropMoose", "PropHare", "PropPtarmagin", "StemHeight", "CanopyCover", "Y_Cord", "Slope", "PropALVI", "PropBENA", "PropSALIX"))

#"MooseFeces", "HareFeces", "PtarmaginFeces"

chart.Correlation(CorPlot_browse, histogram = TRUE, method = c("pearson"))

cor.test(Section_Data$PropPtarmagin, Section_Data$Y_Cord, method = "pearson")

kruskal.test(Section_Data$PropMoose, Section_Data$Y_Cord)

#Assess Browsing Overlap 
#792 Shrubs Sampled: Create a dataframe with values from 1-7,920,000
ProbBase = data.frame(ID=c(1:7920000))

#323 Browsed
  #141 Browsed by Moose
  #159 Browsed by Hare
  #113 Browsed by Ptarmagin

BrowseValue = c(1, 0)

# Randomly assign values the dataframe based on the observed browsing numbers
ProbBase$M1[sample(1:nrow(ProbBase), nrow(ProbBase), FALSE)] = rep(BrowseValue, c(1410000, 6510000))

ProbBase$H1[sample(1:nrow(ProbBase), nrow(ProbBase), FALSE)] = rep(BrowseValue, c(1590000, 6330000))

ProbBase$P1[sample(1:nrow(ProbBase), nrow(ProbBase), FALSE)] = rep(BrowseValue, c(1130000, 6790000))

#Combine Values for each for each combination of herbivore variables 
ProbBase$MH = ProbBase$M1 + ProbBase$H1

ProbBase$MP = paste(ProbBase$M1, ProbBase$P1)

ProbBase$HP = paste(ProbBase$H1, ProbBase$P1)

#Count the # of times a value occurs
plyr::count(ProbBase$MH) 

#1 0 5203424
#2 1 2433152
#3 2  283424

283424/10000 # 28.3424

prop.test(x = 283424, n = 7920000, conf.level=0.95, correct = FALSE)

binconf(x = 283424, n = 7920000)

# PointEst      Lower      Upper
# 0.03578586 0.03565672 0.03591545

# Moose and hare overlap

plyr::count(ProbBase$MP) 

#x    freq
#1     5581154
#2 P   928846
#3 M   1208846
#4 MP  201154

201154/10000 # 20.1154

plyr::count(ProbBase$HP) 

#x    freq
#1     5426435
#2 P   903565
#3 H   1363565
#4 HP  226435

226435/10000 #22.6435

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
par(mfrow=c(7,1), omi=c(1,0,0,0), plt=c(0.1,0.9,0,0.8)) #, bg=NA) 

#par(mfrow=c(1,1))

plot(Section_Data$PropMoose ~ Section_Data$Y_Cord,
     type = "b", pch = 1, col = "chartreuse3", lty = 1, lwd = 1.5, 
     xaxt='n', frame.plot = FALSE,
     ylab = "% Twigs Browsed", xlab = "", cex.lab = 1.5, cex.axis = 1.25)

lines(Section_Data$Y_Cord, Section_Data$PropHare, type = "b", pch = 1, col = "firebrick3", lty = 2, lwd = 1.5 )
lines(Section_Data$Y_Cord, Section_Data$PropPtarmagin, type = "b", pch = 1, col = "dodgerblue4", lty = 3, lwd = 1.5)

legend("topleft", legend=c("Moose", "Hare", "Ptarmagin"),
       col=c("chartreuse3", "firebrick3", "dodgerblue4"), lty=1:3, cex=1.2, bty = "n", text.width=0)

mtext("Browsing Intensity", side= 3, line = -1, adj = 1, padj = 0, cex=1.25)

plot(Section_Data$PropBENA ~ Section_Data$Y_Cord,
     type = "b", pch = 0, col = "darkolivegreen2", lty = 4, lwd = 1.5, 
     xaxt='n', frame.plot = FALSE, ylim = c(-.15,0.85),
     ylab = "% of Shrubs", xlab = "", cex.lab = 1.5, cex.axis = 1.25)

lines(Section_Data$Y_Cord, Section_Data$PropALVI, type = "b", pch = 5, col = "darkolivegreen3", lty = 5, lwd = 1.5 )
lines(Section_Data$Y_Cord, Section_Data$PropSALIX, type = "b", pch = 6, col = "darkolivegreen4", lty = 6, lwd = 1.5)

legend("bottom", "groups", legend=c("Betula nana", "Alnus viridus", "Salix spp."), ncol=3, inset=c(-0.2,-.08),
       col=c("darkolivegreen2", "darkolivegreen3", "darkolivegreen4"), lty=4:6, pch = c(0,5,6), cex=1.2, bty = "n", text.width=.4)

mtext("Dominant Shrub Species", side= 3, line = -1, adj = 1, padj = 0, cex=1.25)
    
plot(Section_Data$CanopyCover ~ Section_Data$Y_Cord,
     type = "b", pch = 1, col = "forest green", 
     xaxt='n', frame.plot = FALSE, lwd = 1.5, 
     ylab = "%", xlab = "", cex.lab = 1.5, cex.axis = 1.25)

mtext("Canopy Cover", side= 3, line = -3, adj = 1, cex=1.25)

plot(Section_Data$StemHeight ~ Section_Data$Y_Cord,
     type = "b", pch = 1, col = "darkslategray", 
    frame.plot = FALSE, lwd = 1.5, xaxt='n',
     ylab = "cm", xlab = "", cex.lab = 1.5, cex.axis = 1.25)

mtext("Canopy Height", side= 3, line = -2.5, adj = 1, padj = 0, cex=1.25)

plot(Section_Data$iem.summ.rain ~ Section_Data$Y_Cord,
     type = "b", pch = 1, col = "dark blue", 
     xaxt='n', frame.plot = FALSE, lwd = 1.5, 
     ylab = "mm", xlab = "", cex.lab = 1.5, cex.axis = 1.25)

mtext("Mean Summer Precipitation", side= 3, line = -1.25, adj = 1, padj = 0, cex=1.25)

plot(Section_Data$iem.summ.temp ~ Section_Data$Y_Cord,
     type = "b", pch = 1, col = "red", 
     xaxt='n', frame.plot = FALSE, lwd = 1.5, 
     ylab = "°C", xlab = "", cex.lab = 1.5, cex.axis = 1.25)

mtext("Mean Summer Temperature", side= 3, line = -2, adj = 1, padj = 0, cex=1.25)

plot(Section_Data$Elevation ~ Section_Data$Y_Cord,
     type = "b", pch = 1, col = "black", 
     frame.plot = FALSE, lwd = 1.5, 
     ylab = "m", xlab = "Latitude", cex.lab = 1.5, cex.axis = 1.25)

mtext("Elevation", side= 3, line = -.5, adj = 1, padj = 0, cex=1.25)
mtext("Latitude", side= 1, line = 3, cex=1.25)


# 10. PLOT GROWTH TREND OVER TIME ####

# Aggregate growth data by year
sd_BAI_bena_agg_ = aggregate((x = sd_bena_cch),
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


# Aggregate growth data by year & section

sd_all_bena_agg_s = aggregate((x = sd_all_bena_cch),
                            by = list(sd_all_bena_cch$Year, sd_all_bena_cch$Section),
                            FUN = mean,
                            na.rm = TRUE)

sd_all_salix_agg_s = aggregate((x = sd_all_salix_cch),
                             by = list(sd_all_salix_cch$Year, sd_all_salix_cch$Section),
                             FUN = mean,
                             na.rm = TRUE)

## Plot Aggregated Growth Vatiables together
par(mfrow=c(1,2))

plot(RingWidth ~ Year, data = sd_all_bena_agg, main = "Betula",
     col = "blue", type = "l", ylim=c(-1.7, 1.5), xlim=c(1955, 2020),
     ylab = "Ring Width", xlab = "Year")

par(new=TRUE)

plot(RWI_Spline ~  Year, data = sd_all_bena_agg, 
     col = "red", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-1.7, 1.5), xlim=c(1955, 2020))

par(new=TRUE)

plot(RWI_NegExp ~ Year, data = sd_all_bena_agg,
     col = "green", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-1.7, 1.5), xlim=c(1955, 2020))

par(new=TRUE)

plot(log(BAI) ~ Year, data = sd_all_bena_agg,
     col = "orange", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-1.7, 2), xlim=c(1955, 2020))

par(new=TRUE)

plot(resid ~ Year, data = sd_all_bena_agg,
     col = "purple", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-1.7, 2), xlim=c(1955, 2020))

par(new=TRUE)

plot(resid_ll ~ Year, data = sd_all_bena_agg,
     col = "pink", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-1.7, 2), xlim=c(1955, 2020))

legend("bottomright",legend=c("Ring Width","RWI_Spline", "RWI_NegExp", "lnBAI", "BAI Residuals"),
       text.col=c("blue", "red", "green", "orange", "purple"), 
       lty = c(1, 1, 1, 1, 1), col=c("blue", "red", "green", "orange", "purple"), bty = "n", cex=0.75)

# For Salix

plot(RingWidth ~ Year, data = sd_all_salix_agg, main = "Salix",
     col = "blue", type = "l", ylim=c(-1.5,2.5), xlim=c(1975, 2020),
     ylab = "Ring Width", xlab = "Year")

par(new=TRUE)

plot(RWI_Spline ~  Year, data = sd_all_salix_agg, 
     col = "red", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-1.5,2.5), xlim=c(1975, 2020))

par(new=TRUE)

plot(RWI_NegExp ~ Year, data = sd_all_salix_agg,
     col = "green", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-1.5,2.5), xlim=c(1975, 2020))

par(new=TRUE)

plot(log(BAI) ~ Year, data = sd_all_salix_agg,
     col = "orange", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-1.5,2.5), xlim=c(1975, 2020))

par(new=TRUE)

plot(resid ~ Year, data = sd_all_salix_agg,
     col = "purple", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-1.5,2.5), xlim=c(1975, 2020))

par(new=TRUE)

plot(resid_ll ~ Year, data = sd_all_salix_agg,
     col = "pink", type = "l", xlab = "", ylab = "", axes=FALSE, ylim=c(-1.5,2.5), xlim=c(1975, 2020))

legend("bottomright",legend=c("Ring Width","RWI_Spline", "RWI_NegExp", "lnBAI", "BAI Residuals"),
       text.col=c("blue", "red", "green", "orange", "purple"), 
       lty = c(1, 1, 1, 1, 1), col=c("blue", "red", "green", "orange", "purple"), bty = "n", cex=0.75)


# Plot Age Trend for Poster 
par(oma=c(0,.5,0,0))

par(mar=c(5, 5, 2, 2))

plot(BAI ~ Year, data = sd_all_bena_agg,
     col = "Black", type = "l", lwd = 2.5, lty = 1,
     ylim=c(0,11), xlim=c(1955, 2020),
     ylab = "Basal Area Increment (mm²)", xlab = "Year",
     cex.lab = 2.15, cex.axis = 1.35)

par(new=TRUE)

plot(BAI ~ Year, data = sd_all_salix_agg,
     col = "Blue", type = "l", lwd = 2.5, lty = 1,
     xlab = "", ylab = "", axes=FALSE, 
     ylim=c(0,11), xlim=c(1955, 2020))

legend("topleft",legend=c("Betula","Salix"),
text.col=c("black", "blue"),
lty = c(1, 1), lwd = c(2,2), col=c("black", "blue"), bty = "n", cex=2)

hist(sd_all_cch$resid, main = "", xlab = "Age Standardized BAI", cex.lab = 2, cex.axis = 1.35)


# PLOT GROWTH TREND OVER TIME BY SECTION & SPECIES ####

#Plot growth trend by section
ggplot(sd_all_bena_cch) + 
  aes(x = Year, y = resid) + 
  stat_summary(geom = "line", fun.y = mean) +
  stat_summary(geom = "ribbon", fun.data = mean_cl_boot, alpha = 0.3) +
  #Add Data for Salix to the existing graph
  stat_summary(data = sd_all_salix_cch, geom = "line", fun.y = mean, colour = "blue") +
  stat_summary(data = sd_all_salix_cch, geom = "ribbon", fun.data = mean_cl_boot, alpha = 0.3, fill = "blue") +
  #Add Climate Variable on the second Y Axis
  scale_y_continuous(sec.axis = sec_axis(~.*1, name = "Mean Summer Temperature (°C)")) +
  stat_summary(data = sd_all_cch, aes(x = Year, y = (iem.summ.temp/10)), geom = "line", fun.y = mean, colour = "red") +
  #facet_wrap("Section") +
  labs(x = "Year", y = "Residuals of standardized BAI")

#Plot growth trend ontop of climate data
par(mar=c(5, 5, 4, 6) + 0.1)

plot(resid ~ Year, data = sd_all_bena_agg, xlab="Year", ylab="Radial Growth\n(Age Standardized BAI)", 
     axes=TRUE, col = "black", type = "l", lty = 1, lwd = 1.5, xlim=c(1985,2018))

lines(sd_all_salix_agg$Year, sd_all_salix_agg$resid, type = "l", pch = 1, col = "blue", lty = 1, lwd = 1.5 )

par(new=TRUE)

plot(tmp ~ Year, data = sum_temp_agg, xlab="", ylab="", ylim=c(8,15), xlim=c(1985,2018),
     axes=FALSE, col=alpha(rgb(1,0,0), 0.75), type = "l", lty = 3, lwd = 1.5) 

mtext("Mean Summer Temperature (°C)",side=4, col="black",line=4) 

axis(4, ylim=c(8,15), col="black",col.axis="black",las=1)

# 11. PLOT GROWTH TREND BY AGE ####

par(mfrow=c(2,2))

par(mfrow=c(2,5))

#Betula
plot(RingWidth ~ Age, data = sd_all_bena_cch,
     col = "black", pch = 1, ylab = "Ring Width", xlab = "Ring Age (years)", main = "Betula")

plot(RWI_Spline ~ Age, data = sd_all_bena_cch,
     col = "black", pch = 1, ylab = "Ring Width Index (Spline)", xlab = "Ring Age (years)")

plot(RWI_NegExp ~ Age, data = sd_all_bena_cch,
     col = "black", pch = 1, ylab = "Ring Width Index (Negative Expontial)", xlab = "Ring Age (years)")

plot(BAI ~ Age, data = sd_all_bena_cch,
     col = "black", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)", main = "Betula")

plot(resid ~ Age, data = sd_all_bena_cch,
     col = "black", pch = 1, ylab = "Residuals of lnBAI", xlab = "Ring Age (years)")

lmResid_b = lm(resid ~ Age, data = sd_all_bena_cch)

abline(lmResid_b, col = "red", lwd = 2)

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

plot(BAI ~ Age, data = sd_all_salix_cch,
     col = "blue", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)", main = "Salix")

abline(lmSalix_l, col = "red", lwd = 2)

plot(resid ~ Age, data = sd_all_salix_cch,
     col = "blue", pch = 1, ylab = "Residuals of lnBAI", xlab = "Ring Age (years)")

lmResid_s = lm(resid ~ Age, data = sd_all_salix_cch)

abline(lmResid_s, col = "red", lwd = 2)

plot(log(BAI) ~ Age, data = sd_all_salix_cch,
     col = "blue", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)")

plot(resid ~ Age, data = sd_all_salix_cch,
     col = "blue", pch = 1, ylab = "Residuals of BAI", xlab = "Ring Age (years)")

# Plot for Poster
par(oma=c(0,.5,0,0))

par(mar=c(5, 5, 2, 2))

plot(resid ~ Age, data = sd_all_bena_cch,
     col = "black", pch = 1, cex.lab = 2, cex.axis = 1.25,
     ylab = "ln Basal Area Increment", xlab = "Ring Age (years)")

abline(lmResid_b, col = "red", lwd = 2)

par(new=TRUE)

plot(resid ~ Age, data = sd_all_salix_cch,
     col = "blue", pch = 4, cex.lab = 1.75,
     xlab = "", ylab = "", axes=FALSE)

abline(lmResid_s, col = "green", lwd = 2)



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

# 13. PLOT GROWTH TREND (BAI) AND FIXED EFFECTS OVER TIME ####

## Plot standardized BAI residuals against mean summer temperature

range(sd_BAI_bena_agg$resid)
range(sd_BAI_salix_agg$resid)

#To plot graphs next to one another
par(mfrow=c(1,2))

# Start by adding extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

# 13.1 Plot first set of data and draw its axis
plot(sd_BAI_bena_agg$iem.summ.temp ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, ylim=c(10,14), #main="Shrub Growth ",
     type = "l", xlab = "", ylab = "", 
     col=alpha(rgb(1,0,0), 0.75), lwd = 1.5, lty = 3, cex.lab = 1)

axis(2, ylim=c(9,15),col="black",las=1)

mtext("Mean Summer Temperature (°C)",side=2,line=2.5)

box()

matplot(dat$Age,
  dat$CO2 + outer(dat$Standard_error, c(0,1,-1)),
  type="l", lty=c(1,2,2), col=c(1,2,2),
  xlab="Age", ylab="CO2"
)

# Allow a second plot on the same graph
par(new=TRUE)

# Plot the second plot and put axis scale on right
plot(sd_BAI_bena_agg$resid ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, ylim=c(-1.5, .7),
     type = "l", xlab = "", ylab = "", 
     col = "black", lwd = 1, lty = 1, cex.lab = 1)

# Allow for a third plot using the second accis
par(new=TRUE)

# Plot the third plot and put axis scale on right
plot(sd_BAI_salix_agg$resid ~ sd_BAI_salix_agg$Year, 
     axes=FALSE, ylim=c(-1.5, .7),
     type = "l", xlab = "", ylab = "", 
     col = "grey 52", lwd = 1, lty = 1, cex.lab = 1)

mtext("Age Standardized BAI", side=4,col="black",line=4) 

axis(4, ylim=c(-1.5, .7), col="black", col.axis="black", las=1)

# Add the Years axis
axis(1,pretty(range(sd_BAI_bena_agg$Year),5))
mtext("Year",side=1, col="black", line=2.5) 

## Add Legend
legend("topleft",legend=c("Temperature","Betula nana", "Salix spp."),
       text.col=c("red", "black", "grey 52"), lty = c(3, 1, 1), col=c("red", "black", "dark grey"), bty = "n")


# 13.2 Plot raw BAI against mean summer Precip

# Start by adding extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

# Plot first set of data and draw its axis
plot(sd_BAI_bena_agg$iem.summ.rain.10 ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, ylim=c(7,37), #main="Shrub Growth ",
     type = "l", xlab = "", ylab = "", 
     col = "blue", lwd = 1, cex.lab = 1)

axis(2, ylim=c(9,15),col="black",las=1)

mtext("Mean Summer Precipitation (cm)",side=2,line=2.5)

box()

# Allow a second plot on the same graph
par(new=TRUE)

# Plot the second plot and put axis scale on right
plot(sd_BAI_bena_agg$resid ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, ylim=c(-1.5, .7),
     type = "l", xlab = "", ylab = "", 
     col = "black", lwd = 1, lty = 5, cex.lab = 1)

# Allow for a third plot using the second accis
par(new=TRUE)

# Plot the third plot and put axis scale on right
plot(sd_BAI_salix_agg$resid~ sd_BAI_salix_agg$Year, 
     axes=FALSE, ylim=c(-1.5, .7),
     type = "l", xlab = "", ylab = "", 
     col = "grey 52", lwd = 1, lty = 2, cex.lab = 1)

mtext("Age Standardized BAI", side=4,col="black",line=4) 

axis(4, ylim=c(-1.5, .7), col="black", col.axis="black", las=1)

# Add the Years axis
axis(1,pretty(range(sd_BAI_salix_agg$Year),5))
mtext("Year",side=1, col="black", line=2.5) 

## Add Legend
legend("topleft",legend=c("Precipitation","Betula", "Salix"),
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
     axes=FALSE, ylim=c(-1.5, .7),
     type = "l", xlab = "", ylab = "", 
     col = "black", lwd = 1, lty = 5, cex.lab = 1)

# Allow for a third plot using the second accis
par(new=TRUE)

# Plot the third plot and put axis scale on right
plot(sd_BAI_salix_agg$resid~ sd_BAI_salix_agg$Year, 
     axes=FALSE, ylim=c(-1.5, .7),
     type = "l", xlab = "", ylab = "", 
     col = "grey 52", lwd = 1, lty = 2, cex.lab = 1)

mtext("Residuals of Standardized BAI", side=4,col="black",line=4) 

axis(4, ylim=c(-1.5, .7), col="black", col.axis="black", las=1)

# Add the Years axis
axis(1,pretty(range(sd_BAI_bena_agg$Year),5))
mtext("Year",side=1, col="black", line=2.5) 


# 14. PLOT GROWTH TREND (RWI) AND FIXED EFFECTS OVER TIME ####

## Plot standardized BAI residuals against mean summer temperature

#To plot graphs next to one another
par(mfrow=c(1,2))

# Start by adding extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

# 14.1 Plot first set of data and draw its axis
plot(sd_BAI_bena_agg$iem.summ.temp ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, ylim=c(10,14), #main="Shrub Growth ",
     type = "l", xlab = "", ylab = "", 
     col = "red", lwd = 1, cex.lab = 1)

axis(2, ylim=c(9,15),col="black",las=1)

mtext("Mean Summer Temperature (°C)",side=2,line=2.5)

box()

# Allow a second plot on the same graph
par(new=TRUE)

# Plot the second plot and put axis scale on right
plot(sd_BAI_bena_agg$RWI_Spline ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, ylim=c(0.75, 1.25),
     type = "l", xlab = "", ylab = "", 
     col = "black", lwd = 1, lty = 5, cex.lab = 1)

# Allow for a third plot using the second accis
par(new=TRUE)

# Plot the third plot and put axis scale on right
plot(sd_BAI_salix_agg$RWI_Spline ~ sd_BAI_salix_agg$Year, 
     axes=FALSE, ylim=c(0.75, 1.25),
     type = "l", xlab = "", ylab = "", 
     col = "grey 52", lwd = 1, lty = 2, cex.lab = 1)

mtext("Ring Width Index", side=4,col="black",line=4) 

axis(4, ylim=c(0.75, 1.25), col="black", col.axis="black", las=1)

# Add the Years axis
axis(1,pretty(range(sd_BAI_bena_agg$Year),5))
mtext("Year",side=1, col="black", line=2.5) 

## Add Legend
legend("topright",legend=c("Temperature","Betula", "Salix"),
       text.col=c("red", "black", "grey 52"), lty = c(1, 5, 2), col=c("red", "black", "dark grey"), bty = "n")


# 14.2 Plot raw BAI against mean summer Precip

# Start by adding extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1)

# Plot first set of data and draw its axis
plot(sd_BAI_bena_agg$iem.summ.rain.10 ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, ylim=c(7,37), #main="Shrub Growth ",
     type = "l", xlab = "", ylab = "", 
     col = "blue", lwd = 1, cex.lab = 1)

axis(2, ylim=c(9,15),col="black",las=1)

mtext("Mean Summer Precipitation (cm)",side=2,line=2.5)

box()

# Allow a second plot on the same graph
par(new=TRUE)

# Plot the second plot and put axis scale on right
plot(sd_BAI_bena_agg$RWI_Spline ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, ylim=c(0.75, 1.25),
     type = "l", xlab = "", ylab = "", 
     col = "black", lwd = 1, lty = 5, cex.lab = 1)

# Allow for a third plot using the second accis
par(new=TRUE)

# Plot the third plot and put axis scale on right
plot(sd_BAI_salix_agg$RWI_Spline~ sd_BAI_salix_agg$Year, 
     axes=FALSE, ylim=c(0.75, 1.25),
     type = "l", xlab = "", ylab = "", 
     col = "grey 52", lwd = 1, lty = 2, cex.lab = 1)

mtext("Ring Width Index", side=4,col="black",line=4) 

axis(4, ylim=c(0.75, 1.25), col="black", col.axis="black", las=1)

# Add the Years axis
axis(1,pretty(range(sd_BAI_salix_agg$Year),5))
mtext("Year",side=1, col="black", line=2.5) 

## Add Legend
legend("topright",legend=c("Precipitation","Betula", "Salix"),
       text.col=c("blue", "black", "grey 52"), lty = c(1, 5, 2), col=c("blue", "black", "dark grey"), bty = "n")


# 14.3 Plot Betula standardized BAI residuals against Hare Cycles 

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
plot(sd_BAI_bena_agg$RWI_Spline ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, ylim=c(0.75, 1.25),
     type = "l", xlab = "", ylab = "", 
     col = "black", lwd = 1, lty = 5, cex.lab = 1)

# Allow for a third plot using the second accis
par(new=TRUE)

# Plot the third plot and put axis scale on right
plot(sd_BAI_salix_agg$RWI_Spline~ sd_BAI_salix_agg$Year, 
     axes=FALSE, ylim=c(0.75, 1.25),
     type = "l", xlab = "", ylab = "", 
     col = "grey 52", lwd = 1, lty = 2, cex.lab = 1)


mtext("Ring WIdth Index", side=4,col="black",line=4) 

axis(4, ylim=c(0.75, 1.25), col="black", col.axis="black", las=1)

# Add the Years axis
axis(1,pretty(range(sd_BAI_bena_agg$Year),5))
mtext("Year",side=1, col="black", line=2.5) 


# 15. PLOT MIXED EFFECTS ACROSS SITES ####


ggplot(sd_bena_cch_S, aes(x = Year, y = resid, colour = Genus)) +
  facet_wrap(~Section, nrow=4) +   # a panel for each sites
  geom_point(alpha = 0.5) +
  theme_classic() +
  geom_line(data = cbind(sd_bena_cch_S, pred = predict(Optimal_model_b)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 

  #facet_wrap(~Section, nrow=4) +   # a panel for each mountain range
  geom_point(alpha = 0.5) +
  theme_classic() +
  geom_line(data = cbind(sd_bena_cch_S, pred = predict(CH1H2_model_b)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"))  # adding space between panels)

ggplot(sd_salix_cch_S, aes(x = iem.summ.temp, y = resid, colour = ShrubID)) +
  facet_wrap(~Section, nrow=4) +   # a panel for each mountain range
  geom_point(alpha = 0.5) +
  theme_classic() +
  geom_line(data = cbind(sd_salix_cch_S, pred = predict(CH1_model_s)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
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
  geom_point() +
  stat_smooth(method = "lm") +
  # Put the points on top of lines
  #facet_wrap("Section") +
  labs(x = "Mean Summer Temperature", y = "Age Standardized BAI") + 

  aes(x = iem.summ.temp, y = resid) + 
  stat_smooth(method = "lm", se = FALSE) +
  # Put the points on top of lines
  geom_point() +
  facet_wrap("Section") +
  labs(x = "Mean Summer Temperature", y = "Residuals of standardized BAI") + 

  # We also need to help the x-axis, so it doesn't 
  # create gridlines/ticks on 2.5 days
  scale_x_continuous(breaks = 0:4 * 2)

# 16. PLOT CHRONOLOGIES FOR POSTER GRAPH ####

col1 = rgb(77, 175, 74, max = 255, alpha = 255)
col2 = rgb(55, 125, 184, max = 255, alpha = 255)
col3 = rgb(152, 78, 163, max = 255, alpha = 255)
col4 = rgb(255, 127, 0, max = 255, alpha = 255)

par(mfrow=c(2,1), omi=c(1,0,0,0), plt=c(0.1,0.9,0,0.9))

plot(R2 ~ Year, data= graph_rw,
     xlab = "", ylab = "",
     ylim = c(0,.5), type = "l",
     cex.axis = 1, cex.lab = 1.25, lwd = 1.5,
     col = rgb(55, 125, 184, max = 255, alpha = 255),
     axes = TRUE, xaxt='n')

lines(graph_rw$Year, graph_rw$R1, type = "l", pch = 1, lty = 1, lwd = 1.5, col = rgb(77, 175, 74, max = 255, alpha = 255))
lines(graph_rw$Year, graph_rw$R3, type = "l", pch = 1, lty = 1, lwd = 1.5, col = rgb(152, 78, 163, max = 255, alpha = 255))
lines(graph_rw$Year, graph_rw$R4, type = "l", pch = 1, lty = 1, lwd = 1.5, col = rgb(255, 127, 0, max = 255, alpha = 255))
#lines(graph_rw$Year, graph_rw$Avg, type = "l", pch = 1, col = "red", lty = 1, lwd = 3 )

legend("topleft", legend=c("R1", "R2", "R3", "R4"),
       col=c(col1, col2, col3, col4), lty=1, cex=1.05, bty = "n", text.width=0)


plot(Avg ~ Year, data= graph_rw,
     xlab = "Year", ylab = "",
     ylim = c(0,.5), type = "o",
     cex.axis = 1, cex.lab = 1.25, lwd = 2,
     col = rgb(55, 125, 184, max = 255, alpha = 255))

mtext("Mean Chronology ", side= , line = -.75, adj = .02, padj = 1, cex=1.25)

mtext("Year ", side= 1, outer = TRUE, line = 1.75, adj = .5, padj = 1, cex=1.3)

mtext("Ring Width (mm) ", side= 2, outer = TRUE, line = -0.9, adj = .5, padj = 1, cex=1.3)

