# Thesis Figures
# Peter Frank 
# peterfr@stud.ntnu.no
# 2020-04-01

# FIGURE 5: Height distributions of shrubs browseing ####

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

# FIGURE 6: Age Standardized BAI Plot ####
## Plot standardized BAI residuals against mean summer temperature

range(sd_BAI_bena_agg$resid)
range(sd_BAI_salix_agg$resid)

# Start by adding extra space to right margin of plot within frame
par(mar=c(5, 4, 4, 6) + 0.1, mai = c(0.1, 0.6, 0.1, 0.1), mfrow=c(3,2)) #bottom, left, top and right

layout(matrix(c(1,1,2,3,4,5), 3, 2, byrow = TRUE))

# 1. Plot Betula & Salix BAI Over Time 

plot(sd_BAI_bena_agg$resid ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, ylim=c(-1.5, .7),
     type = "l", xlab = "", ylab = "", 
     col = "black", lwd = 1.5, lty = 1, cex.lab = 1)

axis(1,pretty(range(sd_BAI_bena_agg$Year),20), cex = 1.5)

# Allow for a third plot using the second accis
par(new=TRUE)

# Plot the second plot and put axis scale on right
plot(sd_BAI_salix_agg$resid ~ sd_BAI_salix_agg$Year, 
     axes=FALSE, ylim=c(-1.5, .7),
     type = "l", xlab = "", ylab = "", 
     col = "grey 52", lwd = 1.5, lty = 1, cex.lab = 1)

# Calculate and Plot Confidence Intervals

#CI_BetulaBAI = subset (sd_BAI_bena_agg, select = c("Year", "resid"))
    #CI(CI_BetulaBAI$resid)
    #std.error(CI_BetulaBAI$resid)
    #CI_BetulaBAI$UpperCI = CI_BetulaBAI$resid + 0.03762269
    #CI_BetulaBAI$SE_UP = CI_BetulaBAI$resid +  0.06925253
    #CI_BetulaBAI$SE_LOW = CI_BetulaBAI$resid -  0.06925253
    #CI_BetulaBAI$LowerCI = CI_BetulaBAI$resid - 0.2460921

lines(CI_BetulaBAI$Year, CI_BetulaBAI$SE_UP, type = "l", pch = 1, col = alpha("black", 0.25), lty = 2, lwd = 1.5 )

lines(CI_BetulaBAI$Year, CI_BetulaBAI$SE_LOW, type = "l", pch = 1, col = alpha("black", 0.25), lty = 2, lwd = 1.5 )

#CI_SalixBAI = subset (sd_BAI_salix_agg, select = c("Year", "resid"))
    #CI(CI_SalixBAI$resid)
    #std.error(CI_SalixBAI$resid)
    #CI_SalixBAI$UpperCI = CI_SalixBAI$resid + 0.2101331
    #CI_SalixBAI$SE_UP = CI_SalixBAI$resid +  0.08283985
    #CI_SalixBAI$SE_LOW = CI_SalixBAI$resid -  0.08283985
    #CI_SalixBAI$LowerCI = CI_SalixBAI$resid - 0.5495125

lines(CI_SalixBAI$Year, CI_SalixBAI$SE_UP, type = "l", pch = 1, col = alpha("grey 52", 0.25), lty = 2, lwd = 1.5 )

lines(CI_SalixBAI$Year, CI_SalixBAI$SE_LOW, type = "l", pch = 1, col = alpha("grey 52", 0.25), lty = 2, lwd = 1.5 )

# Add text
mtext("Age Standardized BAI", side=2, col="black", line=2.75, cex = 1) 

axis(2, ylim=c(-1.5, .7), col="black", col.axis="black", las=1, cex = 1.5)

box()

legend(1985.5, 0.8, legend=c("A."), bty = "n", cex = 1.5)

# Add Legend
legend("bottomright",legend=c("Betula nana", "Salix spp."),
       text.col=c("black", "grey 52"), lty = c(1, 1), col=c("black", "dark grey"), bty = "n", cex = 1.5)

#Insert blank plot 
#plot(0,type='n',axes=FALSE,ann=FALSE)

# 2. Plot Moose Density
par(mar=c(5, 4, 4, 6) + 0.1, mai = c(0.1, 0.6, 0.3, 0.1)) 

plot(GMU_24A ~ Year, data = GMU_MooseDensity_Graph,
     col = "darkorchid4", type = "l", axes=FALSE, #ylim=c(0,.65), xlim=c(1985, 2020)
     ylab = "", xlab = "", cex.lab = 1.5, lwd = 1.5)

par(new=TRUE)

plot(GMU_20F ~ Year, data = GMU_MooseDensity_Graph, 
     col = "chocolate1", type = "l", lwd = 1.5, xlab = "", ylab = "", axes=FALSE, ylim=c(0,.65), xlim=c(1985, 2020))

par(new=TRUE)

plot(GMU_26B ~ Year, data = GMU_MooseDensity_Graph, 
     col = "seagreen4", type = "l", lwd = 1.5, xlab = "", ylab = "", axes=FALSE, ylim=c(0,.65), xlim=c(1985, 2020))


legend("topright", legend=c("24A","20F", "26B"), title="GMU", #"24A (4,146 km²)","20F (6,267 km²)", "26B (16,332 km²)"
       #text.col=c("black", "blue", "red", "forest green"), 
       lty = c(1, 1, 1), col=c("darkorchid4", "chocolate1", "seagreen4"), bty = "n", cex=1.25)

mtext("Moose Density (moose/km²)", side=2, col="black", line=2.75, cex = 1) 

axis(2, ylim=c(0,.65), col="black", col.axis="black", las=1, cex = 1.25)

box()

legend(1982.5, 0.08, legend=c("B."), bty = "n", cex = 1.25)

# 3. Plot Snowshoe Hare Density 

plot(sd_BAI_bena_agg$HareIndex ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, ylim=c(0.75,3.25), main="",
     type = "l", xlab = "", ylab = "", 
     col = "darkslateblue", lwd = 1.5, cex.lab = 1)

axis(2, at = c(1,2,3), col="black",las=1)

mtext("Snowshoe Hare Cycle Index",side=2,line=2.5, cex = 1)

box()

legend(1984.5, 1.05, legend=c("C."), bty = "n", cex = 1.25)

# 4. Plot MST Over Time 

par(mar=c(5, 4, 4, 6) + 0.1, mai = c(0.5, 0.6, 0.1, 0.1)) 

plot(sd_BAI_bena_agg$iem.summ.temp ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, 
     type = "l", xlab = "", ylab = "", 
     col=alpha(rgb(1,0,0), 0.75), lwd = 1.75, lty = 3, cex.lab = 1)

axis(2, ylim=c(9,15),col="black",las=1)

mtext("Mean Summer Temperature (°C)", side=2, line=2.75, cex = 1)

box()

legend(1984.5, 10.9, legend=c("D."), bty = "n", cex = 1.25)

# Add the Years axis
axis(1, pretty(range(sd_BAI_bena_agg$Year), 20))

mtext("Year", side=1, col="black", line=2.7, cex = 1) 

# 5. Plot MSP Over Time

plot(sd_BAI_bena_agg$iem.summ.rain.10 ~ sd_BAI_bena_agg$Year, 
     axes=FALSE, 
     type = "l", xlab = "", ylab = "", 
     col=alpha(rgb(0,0,1), 0.75), lwd = 1.75, lty = 3, cex.lab = 1)

axis(2, ylim=c(9,15),col="black",las=1)

mtext("Mean Summer Precipitation (cm)", side=2, line=2.75, cex = 1)

box()

legend(1984.5, 10.9, legend=c("E."), bty = "n", cex = 1.25)

# Add the Years axis
axis(1,pretty(range(sd_BAI_bena_agg$Year),20))

# Add A central Yea Text 
mtext("Year", side=1, col="black", line=2.7, cex = 1) 


# APPENDIX 1: Variation in Site Covariates with Latitude ####
par(mfrow=c(7,1), omi=c(1,0,0,0), plt=c(0.1,0.9,0,0.8))

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

# APPENDIX 2: Model Selection via Backwards Elimination ####

#Betula

par(mar = c(5,12,5,2) + 0.1)

color2D.matplot(ModelSelection_Betula, 
                vcol = "White", na.color = "White",
                extremes = c("Blue","Red"),
                nslices = 50,
                show.legend=TRUE, show.values=4,
                axes=FALSE, xlab="",ylab="")

axis(3,at=0.5:5,las=1,labels=c("Full\n Model", "Step 1", "Step 2", "Step 3", "Minimal\nAdequate\nModel"))
axis(2,at=0.5:9,las=2,labels=c("Snowshow Hare :\nMean SummerTemperature\nInteraction",
                               "Moose :\nMean Summer Temperature\nInteraction",
                               "Ptarmagin\n Browsing Intensity", "Snowshoe Hare\nBrowsing Intensity", "Moose\nBrowsingIntensity",
                               "Snowshoe Hare\nIndex", "Moose Density", 
                               "Mean Summer\nTemperature", "Mean Summer\nPrecipitation"))

#Salix

par(mar = c(5,12,5,2) + 0.1)

color2D.matplot(ModelSelection_Salix, 
                vcol = "White", na.color = "White",
                extremes = c("Blue","Red"),
                nslices = 50,
                show.legend=TRUE, show.values=4,
                axes=FALSE, xlab="",ylab="")

axis(3,at=0.5:7,las=1,labels=c("Full\n Model", "Step 1", "Step 2", "Step 3", "Step 4", "Step 5", "Minimal\nAdequate\nModel"))
axis(2,at=0.5:9,las=2,labels=c("Snowshow Hare :\nMean SummerTemperature\nInteraction",
                               "Moose :\nMean Summer Temperature\nInteraction",
                               "Ptarmagin\n Browsing Intensity", "Snowshoe Hare\nBrowsing Intensity", "Moose\nBrowsingIntensity",
                               "Snowshoe Hare\nIndex", "Moose Density", 
                               "Mean Summer\nTemperature", "Mean Summer\nPrecipitation"))
