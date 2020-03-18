install.packages("qualityTools")
library(qualityTools)

# DEVELOP HEAT MAPS FOR OPTIMAL BETULA MODEL ####
HM_B_MST_MSP = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                MooseDensity,
                data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                method = "REML")

summary(HM_B_MST_MSP)

HI = mean(sd_bena_cch$HareIndex)
MD = mean(sd_bena_cch$MooseDensity)
ST = mean(sd_bena_cch$iem.summ.temp)
SR = mean(sd_bena_cch$iem.summ.rain)
range(sd_bena_cch$iem.summ.temp)
range(sd_bena_cch$iem.summ.rain.10)

# Predict
MyData_b<-expand.grid(iem.summ.temp = seq(6, 18, length = 190),   #min and max of temp
                    iem.summ.rain.10 = seq(3, 48, length = 190), #min and max of pre
                    MooseDensity = MD)


MyData_b$Pred <- predict(HM_B_MST_MSP, MyData_b, level = 0) #Predicts the values based on model

# Calculate SEs
Designmat_b1 <- model.matrix(formula(HM_B_MST_MSP)[-2], MyData_b) ## [-2] drops response from formula
predvar_b1 <- diag(Designmat_b1 %*% vcov(HM_B_MST_MSP) %*% t(Designmat_b1)) 
MyData_b$SE <- sqrt(predvar_b1)
MyData_b$SEup<-MyData_b$SE+MyData_b$Pred
MyData_b$SEdown<-MyData_b$Pred-MyData_b$SE


# Plot
col.l = colorRampPalette(c( 'white', rgb(0, 80, 158, max = 255)))
z = c(0:10)
pb = contourplot(Pred ~ iem.summ.temp * iem.summ.rain.10,
               data=MyData_b,
               xlab="Mean Summer Temperature",
               ylab="Mean Summer Precipitation",
               pretty=TRUE,
               lty=1,
               zlim=range(z, finite=TRUE),
               lwd=0.5,
               labels=list(cex=1),
               col.regions=col.l,
               region=TRUE,
               main=list("", cex=1))

pb

# Plot Standard error lines 
pb = pb + contourplot(SEup ~ iem.summ.temp * iem.summ.rain.10, 
                 data = MyData_b,
                 cuts=10,
                 at = c(0.2), #change these when you see the plot
                 pretty=TRUE,
                 lty=2,
                 zlim = range(z, finite = TRUE),
                 lwd=0.5,
                 labels = list(cex=0),
                 region=FALSE,
                 main = list("", cex = 1))

pb = pb + contourplot(SEdown ~ iem.summ.temp * iem.summ.rain.10, 
                 data=MyData_b,
                 cuts=10,
                 at = c(0.2), #change these when you see the plot
                 pretty=TRUE,
                 lty=2,
                 zlim = range(z, finite = TRUE),
                 lwd=0.5,
                 labels=list(cex=0),
                 region=FALSE,
                 main = list("", cex = 1, font=1))

pb

# plot points growth & size by sample size:
str(sd_bena_cch)

sd_bena_cch$Year_Section <- do.call(paste, c(sd_bena_cch[c("Year", "Section")], sep = "_"))

growth<-tapply(sd_bena_cch$resid, list(sd_bena_cch$Year_Section),mean)    
names <- rownames(growth)
rownames(growth) <- NULL
growth <- cbind(names,growth)
colnames(growth)[colnames(growth)=="names"] <- "Year_Section"

temp<-tapply(sd_bena_cch$iem.summ.temp, list(sd_bena_cch$Year_Section),mean)    
names <- rownames(temp)
rownames(temp) <- NULL
temp <- cbind(names,temp)
colnames(temp)[colnames(temp)=="names"] <- "Year_Section"

rain<-tapply(sd_bena_cch$iem.summ.rain.10, list(sd_bena_cch$Year_Section),mean)    
names <- rownames(rain)
rownames(rain) <- NULL
rain <- cbind(names,rain)
colnames(rain)[colnames(rain)=="names"] <- "Year_Section"

moose<-tapply(sd_bena_cch$MooseDensity, list(sd_bena_cch$Year_Section),mean)    
names <- rownames(moose)
rownames(moose) <- NULL
moose <- cbind(names,moose)
colnames(moose)[colnames(moose)=="names"] <- "Year_Section"

s<- count(sd_bena_cch, vars = "Year_Section")

plotpoints<-cbind(growth, moose, temp, rain, s, by="Year_Section")

str(plotpoints)

plotpoints$n<-(plotpoints$freq)/5

plotpoints$moose<-as.numeric(as.character(plotpoints$moose))
plotpoints$temp<-as.numeric(as.character(plotpoints$temp))
plotpoints$rain<-as.numeric(as.character(plotpoints$rain))
plotpoints$n<-as.numeric(as.character(plotpoints$n))
plotpoints$growth<-as.numeric(as.character(plotpoints$growth))

pb

trellis.focus("panel", 1, 1, highlight=F)

lpoints(sd_bena_cch$iem.summ.temp, y = sd_bena_cch$iem.summ.rain.10, 
        col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), 
        pch = 4, cex = 0.6)

lpoints(plotpoints$temp, y = plotpoints$rain,
        cex=c(plotpoints$n),
        col="black", # outline color
        bg=c('col.l', by=plotpoints$growth),
        pch=21) # type

# DEVELOP HEAT MAPS FOR OPTIMAL SALIX MODEL ####
HM_S_MST_MSP = lme(resid ~ iem.summ.temp + iem.summ.rain.10 +
                  iem.summ.temp * iem.summ.rain.10, 
                  data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                  method = "REML")

summary(HM_S_MST_MSP)

HIs = mean(sd_salix_cch$HareIndex)
MDs = mean(sd_salix_cch$MooseDensity)
SRs = mean(sd_salix_cch$iem.summ.rain)
STs = mean(sd_salix_cch$iem.summ.temp)
range(sd_salix_cch$iem.summ.temp)
range(sd_salix_cch$iem.summ.rain.10)
range(sd_salix_cch$HareIndex)
range(sd_salix_cch$MooseDensity)

# Predict
MyData_s<-expand.grid(iem.summ.temp = seq(6, 18, length = 190),
                    iem.summ.rain.10 = seq(3, 48, length = 190))

MyData_s$Pred <- predict(HM_S_MST_MSP, MyData_s, level = 0) #Predicts the values based on model

# Calculate SEs
Designmat_s1 <- model.matrix(formula(HM_S_MST_MSP)[-2], MyData_s) ## [-2] drops response from formula
predvar_s1 <- diag(Designmat_s1 %*% vcov(HM_S_MST_MSP) %*% t(Designmat_s1)) 
MyData_s$SE <- sqrt(predvar_s1)
MyData_s$SEup<-MyData_s$SE+MyData_s$Pred
MyData_s$SEdown<-MyData_s$Pred-MyData_s$SE


# Plot
col.l = colorRampPalette(c( 'white', rgb(0, 80, 158, max = 255)))
z = c(0:10)
ps = contourplot(Pred ~ iem.summ.temp + iem.summ.rain.10,
                data=MyData_s,
                xlab="mean summer temperature",
                ylab="mean summer precip",
                pretty=TRUE,
                lty=1,
                zlim=range(z, finite=TRUE),
                lwd=0.5,
                labels=list(cex=1),
                col.regions=col.l,
                region=TRUE,
                main=list("Salix radial growth (residual values)", cex=1))

ps

# Plot Standard error lines 
ps = ps + contourplot(SEup ~ iem.summ.temp * iem.summ.rain.10, 
                 data = MyData_s,
                 cuts=10,
                 at = c(0.2), #change these when you see the plot
                 pretty=TRUE,
                 lty=2,
                 zlim = range(z, finite = TRUE),
                 lwd=0.5,
                 labels = list(cex=0),
                 region=FALSE,
                 main = list("Salix radial growth (residual values)", cex = 1))

ps = ps + contourplot(SEdown ~ iem.summ.temp * iem.summ.rain.10, 
                 data=MyData_s,
                 cuts=10,
                 at = c(0.2), #change these when you see the plot
                 pretty=TRUE,
                 lty=2,
                 zlim = range(z, finite = TRUE),
                 lwd=0.5,
                 labels=list(cex=0),
                 region=FALSE,
                 main = list("Salix radial growth (residual values)", cex = 1, font=1))

ps

trellis.focus("panel", 1, 1, highlight=F)

lpoints(sd_salix_cch$iem.summ.temp, y = sd_salix_cch$iem.summ.rain.10, 
        col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3), 
        pch = 4, cex = 0.5)


# DEVELOP HEAT MAPS FOR OPTIMAL BETULA MODEL WITH INTERACTION MST:MD ####
HM_B_MST_HD = lme(resid ~ iem.summ.temp + HareIndex + 
                   iem.summ.temp * HareIndex,
                   data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                   method = "REML")

#Used in the predict grid 
HI = mean(sd_bena_cch_S$HareIndex)
MD = mean(sd_bena_cch_S$MooseDensity)
SR = mean(sd_bena_cch_S$iem.summ.rain)
ST = mean(sd_bena_cch_S$iem.summ.temp)

range(sd_bena_cch$iem.summ.temp)
range(sd_bena_cch_S$iem.summ.rain.10)
range(sd_bena_cch_S$MooseDensity)
range(sd_bena_cch$HareIndex)

# Predict Standardized Data
MyData_bm<-expand.grid(iem.summ.temp = seq(-2.2, 2.7, length = 190),
                       iem.summ.rain.10 = SR,
                       MooseDensity = MD,
                      HareIndex = seq(-0.65, 2.5, length = 190))

# Predict Non-Standardized Data
MyData_bm<-expand.grid(iem.summ.temp = seq(6, 17.5, length = 190),
                       HareIndex = seq(.5, 3.5, length = 190))


MyData_bm$Pred <- predict(Optimal_model_b, MyData_bm, level = 0) #Predicts the values based on model

# Calculate SEs
Designmat_b2 <- model.matrix(formula(Optimal_model_b)[-2], MyData_bm) ## [-2] drops response from formula
predvar_b2 <- diag(Designmat_b2 %*% vcov(Optimal_model_b) %*% t(Designmat_b2)) 
MyData_bm$SE <- sqrt(predvar_b2)
MyData_bm$SEup<-MyData_bm$SE+MyData_bm$Pred
MyData_bm$SEdown<-MyData_bm$Pred-MyData_bm$SE

# Plot
col.l = colorRampPalette(c('white', rgb(0, 80, 158, max = 255)))
z = c(0:10)
pM = contourplot(Pred ~ iem.summ.temp + HareIndex,
                 data=MyData_bm,
                 xlab="Mean Summer Temperature",
                 ylab="HareIndex",
                 pretty=TRUE,
                 lty=1,
                 zlim=range(z, finite=TRUE),
                 lwd=0.5,
                 labels=list(cex=1),
                 col.regions=col.l,
                 region=TRUE,
                 main=list("", cex=1))

pM

# Plot Standard error lines 
pM = pM + contourplot(SEup ~ iem.summ.temp * HareIndex, 
                      data = MyData_bm,
                      cuts=10,
                      at = c(0.2), #change these when you see the plot
                      pretty=TRUE,
                      lty=2,
                      zlim = range(z, finite = TRUE),
                      lwd=0.5,
                      labels = list(cex=0),
                      region=FALSE,
                      main = list("", cex = 1))

pM = pM + contourplot(SEdown ~ iem.summ.temp * HareIndex, 
                      data=MyData_bm,
                      cuts=10,
                      at = c(0.2), #change these when you see the plot
                      pretty=TRUE,
                      lty=2,
                      zlim = range(z, finite = TRUE),
                      lwd=0.5,
                      labels=list(cex=0),
                      region=FALSE,
                      main = list("", cex = 1, font=1))

pM

trellis.focus("panel", 1, 1, highlight=F)

lpoints(sd_bena_cch_S$iem.summ.temp, y = sd_bena_cch_S$HareIndex, 
        col = rgb(red = 0, green = 0, blue = 0, alpha = 0.1), 
        pch = 4, cex = 0.65)


# DEVELOP HEAT MAPS FOR OPTIMAL SALIX MODEL WITH INTERACTION MD:HI ####
HM_S_MD_HI = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + 
                   MooseDensity * HareIndex, 
                 data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                 method = "REML")

summary(HM_S_MD_HI)

range(sd_salix_cch$iem.summ.temp)
range(sd_salix_cch$iem.summ.rain.10)

# Predict
MyData_s<-expand.grid(iem.summ.temp = ST,  
                      iem.summ.rain.10 = SR, 
                      HareIndex = seq(.75, 3.25, length = 190),
                      MooseDensity = seq(0.05, 0.65, length = 190))

MyData_s$Pred <- predict(HM_S_MD_HI, MyData_s, level = 0) #Predicts the values based on model

# Calculate SEs
Designmat_s2 <- model.matrix(formula(HM_S_MD_HI)[-2], MyData_s) ## [-2] drops response from formula
predvar_s2 <- diag(Designmat_s2 %*% vcov(HM_S_MD_HI) %*% t(Designmat_s2)) 
MyData_s$SE <- sqrt(predvar_s2)
MyData_s$SEup<-MyData_s$SE+MyData_s$Pred
MyData_s$SEdown<-MyData_s$Pred-MyData_s$SE

# Plot
col.l = colorRampPalette(c('white', rgb(0, 80, 158, max = 255)))
z = c(0:10)
pMH = contourplot(Pred ~ MooseDensity * HareIndex,
                 data=MyData_s,
                 xlab="Moose Density",
                 ylab="Hare Index",
                 pretty=TRUE,
                 lty=1,
                 zlim=range(z, finite=TRUE),
                 lwd=0.5,
                 labels=list(cex=1),
                 col.regions=col.l,
                 region=TRUE,
                 main=list("", cex=1))

pMH

# Plot Standard error lines 
pMH = pMH + contourplot(SEup ~ MooseDensity * HareIndex, 
                      data = MyData_s,
                      cuts=10,
                      at = c(0.2), #change these when you see the plot
                      pretty=TRUE,
                      lty=2,
                      zlim = range(z, finite = TRUE),
                      lwd=0.5,
                      labels = list(cex=0),
                      region=FALSE,
                      main = list("", cex = 1))

pMH = pMH + contourplot(SEdown ~ MooseDensity * HareIndex, 
                      data=MyData_s,
                      cuts=10,
                      at = c(0.2), #change these when you see the plot
                      pretty=TRUE,
                      lty=2,
                      zlim = range(z, finite = TRUE),
                      lwd=0.5,
                      labels=list(cex=0),
                      region=FALSE,
                      main = list("", cex = 1, font=1))

pMH

trellis.focus("panel", 1, 1, highlight=F)

lpoints(sd_salix_cch$MooseDensity, y = sd_salix_cch$HareIndex, 
        col = rgb(red = 0, green = 0, blue = 0, alpha = 0.1), 
        pch = 4, cex = 0.65)

# DEVELOP A PLOT GRID FOR BAI:MST RELATIONSHIP ACROSS MOOSE VALUES####

#Subset the large dataset to only include relevent variables of moose density, MST and BAI
str(sd_bena_cch)

moose_plot = subset(sd_bena_cch, select = c("ShrubID", "Section", "resid", "iem.summ.temp", "MooseDensity"))  

moose_plot$moose_bin = cut(moose_plot$MooseDensity, 3)

lme_temp_b = lmer(resid ~ iem.summ.temp + (1 + iem.summ.temp|Section/ShrubID), data = moose_plot)

ggplot(moose_plot, aes(x = iem.summ.temp, y = resid, colour = ShrubID)) +
  facet_wrap(~moose_bin, nrow=1) +   # a panel for each sites
  geom_point(alpha = 0.5) +
  theme_classic() +
  geom_line(data = cbind(sd_bena_cch, pred = predict(lme_temp_b)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  theme(legend.position = "none",
        panel.spacing = unit(2, "lines"))  # adding space between panels)



# Plot Points #####

growth = tapply(sd_salix_cch$resid, list(sd_salix_cch$SectionYear), FUN = "mean")
names <- rownames(growth)
rownames(growth) <- NULL
growth <- cbind(names,growth)
colnames(growth)[colnames(growth)=="names"] <- "SectionYear"

temp<-tapply(sd_salix_cch$iem.summ.temp,list(sd_salix_cch$SectionYear), FUN = "mean")   
names <- rownames(temp)
rownames(temp) <- NULL
temp <- cbind(names,temp)
colnames(temp)[colnames(temp)=="names"] <- "SectionYear"

precip<-tapply(sd_salix_cch$iem.summ.rain.10,list(sd_salix_cch$SectionYear), FUN = "mean")   
names <- rownames(precip)
rownames(precip) <- NULL
precip <- cbind(names,precip)
colnames(precip)[colnames(precip)=="names"] <- "SectionYear"

s <- count(sd_salix_cch, c('SectionYear'))
plotpoints<-cbind(growth, temp, precip, s, by="SectionYear")

str(plotpoints)
plotpoints$temp<-as.numeric(as.character(plotpoints$temp))
plotpoints$precip<-as.numeric(as.character(plotpoints$precip))
plotpoints$growth<-as.numeric(as.character(plotpoints$growth))

#plotpoints$freq<-as.numeric(as.character(plotpoints$freq))
#plotpoints$freq<-(plotpoints$freq)/10

ps

trellis.focus("panel", 1, 1, highlight=F)


###**** Can't plot this by growth becasue residual values can be ngative
points(plotpoints$temp, y = plotpoints$precip,
       #cex=c(plotpoints$freq),
       # outline color
       #col=c(col.l(10), by=plotpoints$growth),
       pch=19)


lpoints(sd_salix_cch$iem.summ.temp, y = sd_salix_cch$iem.summ.rain.10, 
        col = rgb(red = 0, green = 0, blue = 0, alpha = 0.3), 
        pch = 4, cex = 0.5)

