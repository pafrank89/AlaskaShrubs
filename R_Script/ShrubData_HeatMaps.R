install.packages("qualityTools")
library(qualityTools)

# DEVELOP HEAT MAPS FOR BETULA MODEL WITH INTERACTION MST:HI STANDARDIZED DATA####
HM_B_MST_HD = lme(resid ~ iem.summ.temp + HareIndex + 
                   iem.summ.temp * HareIndex,
                   data = sd_bena_cch_S, random = ~ 1|Section/ShrubID,
                   method = "REML")

range(sd_bena_cch_S$resid)

#Used in the predict grid 
HI = mean(sd_bena_cch_S$HareIndex)
MD = mean(sd_bena_cch_S$MooseDensity)
SR = mean(sd_bena_cch_S$iem.summ.rain)
ST = mean(sd_bena_cch_S$iem.summ.temp)

range(sd_bena_cch_S$iem.summ.temp)
range(sd_bena_cch_S$iem.summ.rain.10)
range(sd_bena_cch_S$MooseDensity)
range(sd_bena_cch_S$HareIndex)

# Predict Standardized Data
MyData_bh<-expand.grid(iem.summ.temp = seq(-2.2, 2.7, length = 190),
                       iem.summ.rain.10 = SR,
                       MooseDensity = MD, #seq(-1.6, 2.75, length = 190))
                       HareIndex = seq(-0.75, 2.75, length = 190))

MyData_bh$Pred <- predict(Optimal_model_b, MyData_bh, level = 0) #Predicts the values based on model

# Calculate SEs
Designmat_b1 <- model.matrix(formula(Optimal_model_b)[-2], MyData_bh) ## [-2] drops response from formula
predvar_b1 <- diag(Designmat_b1 %*% vcov(Optimal_model_b) %*% t(Designmat_b1)) 
MyData_bh$SE <- sqrt(predvar_b1)
MyData_bh$SEup<-MyData_bh$SE+MyData_bh$Pred
MyData_bh$SEdown<-MyData_bh$Pred-MyData_bh$SE

# Plot
col.l = colorRampPalette(c('white', rgb(0, 80, 158, max = 255)))
z = c(0:10)
pH = contourplot(Pred ~ iem.summ.temp + HareIndex,
                 data=MyData_bh,
                 xlab="Mean Summer Temperature",
                 ylab="Snowshoe Hare Population Index",
                 pretty=TRUE,
                 lty=1,
                 zlim=range(z, finite=TRUE),
                 lwd=0.5,
                 labels=list(cex=1),
                 col.regions=col.l,
                 region=TRUE,
                 main=list("", cex=1))

pH

# Plot Standard error lines 
pH = pH + contourplot(SEup ~ iem.summ.temp * HareIndex, 
                      data = MyData_bh,
                      cuts=10,
                      at = c(7.5), #change these when you see the plot
                      pretty=TRUE,
                      lty=2,
                      zlim = range(z, finite = TRUE),
                      lwd=0.5,
                      labels = list(cex=0),
                      region=FALSE,
                      main = list("", cex = 1))

pH

pH = pH + contourplot(SEdown ~ iem.summ.temp * HareIndex, 
                      data=MyData_bh,
                      cuts=10,
                      at = c(6.5), #change these when you see the plot
                      pretty=TRUE,
                      lty=2,
                      zlim = range(z, finite = TRUE),
                      lwd=0.5,
                      labels=list(cex=0),
                      region=FALSE,
                      main = list("", cex = 1, font=1))

pH

trellis.focus("panel", 1, 1, highlight=F)

lpoints(sd_bena_cch_S$iem.summ.temp, y = sd_bena_cch_S$HareIndex, 
        col = rgb(red = 0, green = 0, blue = 0, alpha = 0.1), 
        pch = 4, cex = 0.65)

# DEVELOP HEAT MAPS FOR BETULA MODEL WITH INTERACTION MST:MD STANDARDIZED DATA####

range(sd_bena_cch_S$resid)

#Used in the predict grid 
HI = mean(sd_bena_cch_S$HareIndex)
MD = mean(sd_bena_cch_S$MooseDensity)
SR = mean(sd_bena_cch_S$iem.summ.rain)
ST = mean(sd_bena_cch_S$iem.summ.temp)

range(sd_bena_cch_S$iem.summ.temp)
range(sd_bena_cch_S$iem.summ.rain.10)
range(sd_bena_cch_S$MooseDensity)
range(sd_bena_cch_S$HareIndex)

# Predict Standardized Data
MyData_bm<-expand.grid(iem.summ.temp = seq(-2.2, 2.7, length = 190),
                       iem.summ.rain.10 = SR,
                       MooseDensity = seq(-1.6, 2.75, length = 190),
                       HareIndex = HI)

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
pM = contourplot(Pred ~ iem.summ.temp + MooseDensity,
                 data=MyData_bm,
                 xlab="Mean Summer Temperature",
                 ylab="Moose Density",
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
pM = pM + contourplot(SEup ~ iem.summ.temp * MooseDensity, 
                      data = MyData_bm,
                      cuts=10,
                      at = c(7.5), #change these when you see the plot
                      pretty=TRUE,
                      lty=2,
                      zlim = range(z, finite = TRUE),
                      lwd=0.5,
                      labels = list(cex=0),
                      region=FALSE,
                      main = list("", cex = 1))

pM

pM = pM + contourplot(SEdown ~ iem.summ.temp * MooseDensity, 
                      data=MyData_bm,
                      cuts=10,
                      at = c(6.5), #change these when you see the plot
                      pretty=TRUE,
                      lty=2,
                      zlim = range(z, finite = TRUE),
                      lwd=0.5,
                      labels=list(cex=0),
                      region=FALSE,
                      main = list("", cex = 1, font=1))

pM

trellis.focus("panel", 1, 1, highlight=F)

lpoints(sd_bena_cch_S$iem.summ.temp, y = sd_bena_cch_S$MooseDensity, 
        col = rgb(red = 0, green = 0, blue = 0, alpha = 0.1), 
        pch = 4, cex = 0.65)

# DEVELOP HEAT MAPS FOR SALIX MODEL WITH INTERACTION MST:HI STANDARDIZED DATA####

range(sd_salix_cch_S$resid)

#Used in the predict grid 
HIs = mean(sd_salix_cch_S$HareIndex)
MDs = mean(sd_salix_cch_S$MooseDensity)
SRs = mean(sd_salix_cch_S$iem.summ.rain)
STs = mean(sd_salix_cch_S$iem.summ.temp)

range(sd_salix_cch_S$iem.summ.temp)
range(sd_salix_cch_S$iem.summ.rain.10)
range(sd_salix_cch_S$MooseDensity)
range(sd_salix_cch_S$HareIndex)

# Predict Standardized Data
MyData_sh<-expand.grid(iem.summ.temp = seq(-1.9, 2.5, length = 190),
                       iem.summ.rain.10 = SRs,
                       MooseDensity = MDs, #seq(-1.2, 3.2, length = 190))
                       HareIndex = seq(-0.65, 2.8, length = 190))

MyData_sh$Pred <- predict(Optimal_model_s, MyData_sh, level = 0) #Predicts the values based on model

# Calculate SEs
Designmat_s1 <- model.matrix(formula(Optimal_model_s)[-2], MyData_sh) ## [-2] drops response from formula
predvar_s1 <- diag(Designmat_s1 %*% vcov(Optimal_model_s) %*% t(Designmat_s1)) 
MyData_sh$SE <- sqrt(predvar_s1)
MyData_sh$SEup<-MyData_sh$SE+MyData_sh$Pred
MyData_sh$SEdown<-MyData_sh$Pred-MyData_sh$SE

# Plot
col.l = colorRampPalette(c('white', rgb(0, 80, 158, max = 255)))
z = c(0:10)
pHs = contourplot(Pred ~ iem.summ.temp + HareIndex,
                 data=MyData_sh,
                 xlab="Mean Summer Temperature",
                 ylab="Snowshoe Hare Population Index",
                 pretty=TRUE,
                 lty=1,
                 zlim=range(z, finite=TRUE),
                 lwd=0.5,
                 labels=list(cex=1),
                 col.regions=col.l,
                 region=TRUE,
                 main=list("", cex=1))

pHs

# Plot Standard error lines 
pHs = pHs + contourplot(SEup ~ iem.summ.temp * HareIndex, 
                      data = MyData_sh,
                      cuts=10,
                      at = c(7.5), #change these when you see the plot
                      pretty=TRUE,
                      lty=2,
                      zlim = range(z, finite = TRUE),
                      lwd=0.5,
                      labels = list(cex=0),
                      region=FALSE,
                      main = list("", cex = 1))

pHs

pHs = pHs + contourplot(SEdown ~ iem.summ.temp * HareIndex, 
                      data=MyData_sh,
                      cuts=10,
                      at = c(6.5), #change these when you see the plot
                      pretty=TRUE,
                      lty=2,
                      zlim = range(z, finite = TRUE),
                      lwd=0.5,
                      labels=list(cex=0),
                      region=FALSE,
                      main = list("", cex = 1, font=1))

pHs

trellis.focus("panel", 1, 1, highlight=F)

lpoints(sd_salix_cch_S$iem.summ.temp, y = sd_salix_cch_S$HareIndex, 
        col = rgb(red = 0, green = 0, blue = 0, alpha = 0.1), 
        pch = 4, cex = 0.65)

# DEVELOP HEAT MAPS FOR SALIX MODEL WITH INTERACTION MST:MD STANDARDIZED DATA####
range(sd_salix_cch_S$resid)

#Used in the predict grid 
HIs = mean(sd_salix_cch_S$HareIndex)
MDs = mean(sd_salix_cch_S$MooseDensity)
SRs = mean(sd_salix_cch_S$iem.summ.rain)
STs = mean(sd_salix_cch_S$iem.summ.temp)

range(sd_salix_cch_S$iem.summ.temp)
range(sd_salix_cch_S$iem.summ.rain.10)
range(sd_salix_cch_S$MooseDensity)
range(sd_salix_cch_S$HareIndex)

# Predict Standardized Data
MyData_sm<-expand.grid(iem.summ.temp = seq(-1.9, 2.5, length = 190),
                       iem.summ.rain.10 = SRs,
                       MooseDensity = seq(-1.2, 3.2, length = 190),
                       HareIndex = HIs)

MyData_sm$Pred <- predict(Optimal_model_s, MyData_sm, level = 0) #Predicts the values based on model

# Calculate SEs
Designmat_s2 <- model.matrix(formula(Optimal_model_s)[-2], MyData_sm) ## [-2] drops response from formula
predvar_s2 <- diag(Designmat_s2 %*% vcov(Optimal_model_s) %*% t(Designmat_s2)) 
MyData_sm$SE <- sqrt(predvar_s2)
MyData_sm$SEup<-MyData_sm$SE+MyData_sm$Pred
MyData_sm$SEdown<-MyData_sm$Pred-MyData_sm$SE

# Plot
col.l = colorRampPalette(c('white', rgb(0, 80, 158, max = 255)))
z = c(0:10)
pMs = contourplot(Pred ~ iem.summ.temp + MooseDensity,
                 data=MyData_sm,
                 xlab="Mean Summer Temperature",
                 ylab="Moose Density",
                 pretty=TRUE,
                 lty=1,
                 zlim=range(z, finite=TRUE),
                 lwd=0.5,
                 labels=list(cex=1),
                 col.regions=col.l,
                 region=TRUE,
                 main=list("", cex=1))

pMs

# Plot Standard error lines 
pMs = pMs + contourplot(SEup ~ iem.summ.temp * MooseDensity, 
                      data = MyData_sm,
                      cuts=10,
                      at = c(7.5), #change these when you see the plot
                      pretty=TRUE,
                      lty=2,
                      zlim = range(z, finite = TRUE),
                      lwd=0.5,
                      labels = list(cex=0),
                      region=FALSE,
                      main = list("", cex = 1))

pMs

pMs = pMs + contourplot(SEdown ~ iem.summ.temp * MooseDensity, 
                      data=MyData_sm,
                      cuts=10,
                      at = c(6.5), #change these when you see the plot
                      pretty=TRUE,
                      lty=2,
                      zlim = range(z, finite = TRUE),
                      lwd=0.5,
                      labels=list(cex=0),
                      region=FALSE,
                      main = list("", cex = 1, font=1))

pMs

trellis.focus("panel", 1, 1, highlight=F)

lpoints(sd_salix_cch_S$iem.summ.temp, y = sd_salix_cch_S$MooseDensity, 
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

