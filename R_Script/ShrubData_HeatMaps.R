# DEVELOP HEAT MAPS FOR OPTIMAL BETULA MODEL ####
Optimal_model_b = lme(resid ~ iem.summ.temp * iem.summ.rain.10 + iem.summ.temp + iem.summ.rain.10 +
                MooseDensity +
                iem.summ.temp:MooseDensity,
                data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                method = "REML")

summary(Optimal_model_b)

HI = mean(sd_bena_cch$HareIndex)
MD = mean(sd_bena_cch$MooseDensity)
range(sd_bena_cch$iem.summ.temp)
range(sd_bena_cch$iem.summ.rain.10)

# Predict
MyData_b<-expand.grid(iem.summ.temp = seq(6, 18, length = 190),   #min and max of temp
                    iem.summ.rain.10 = seq(3, 48, length = 190), #min and max of pre
                    MooseDensity = MD)


MyData_b$Pred <- predict(Optimal_model_b, MyData_b, level = 0) #Predicts the values based on model

# Calculate SEs
Designmat <- model.matrix(formula(Optimal_model_b)[-2], MyData_b) ## [-2] drops response from formula
predvar <- diag(Designmat %*% vcov(Optimal_model_b) %*% t(Designmat)) 
MyData_b$SE <- sqrt(predvar)
MyData_b$SEup<-MyData_b$SE+MyData_b$Pred
MyData_b$SEdown<-MyData_b$Pred-MyData_b$SE

# Plot
col.l = colorRampPalette(c( 'white', rgb(0, 80, 158, max = 255)))
z = c(0:10)
pb = contourplot(Pred ~ iem.summ.temp + iem.summ.rain.10,
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

trellis.focus("panel", 1, 1, highlight=F)

lpoints(sd_bena_cch$iem.summ.temp, y = sd_bena_cch$iem.summ.rain.10, 
        col = rgb(red = 0, green = 0, blue = 0, alpha = 0.2), 
        pch = 4, cex = 0.6)

# DEVELOP HEAT MAPS FOR OPTIMAL SALIX MODEL ####

Optimal_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                  MooseDensity * HareIndex, 
                  data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                  method = "REML")

summary(Optimal_model_s)

MDs = mean(sd_salix_cch$MooseDensity)
range(sd_salix_cch$iem.summ.temp)
range(sd_salix_cch$iem.summ.rain.10)

# Predict
MyData_s<-expand.grid(iem.summ.temp = seq(6, 18, length = 190),   #min and max of temp
                    iem.summ.rain.10 = seq(3, 48, length = 190), #min and max of pre
                    MooseDensity = MDs,
                    HareIndex = HI)


MyData_s$Pred <- predict(Optimal_model_s, MyData_s, level = 0) #Predicts the values based on model

# Calculate SEs
Designmat <- model.matrix(formula(Optimal_model_s)[-2], MyData_s) ## [-2] drops response from formula
predvar <- diag(Designmat %*% vcov(Optimal_model_s) %*% t(Designmat)) 
MyData_s$SE <- sqrt(predvar)
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
HI = mean(sd_bena_cch$HareIndex)
MD = mean(sd_bena_cch$MooseDensity)
SR = mean(sd_bena_cch$iem.summ.rain)
ST = mean(sd_bena_cch$iem.summ.temp)
range(sd_bena_cch$iem.summ.temp)
range(sd_bena_cch$iem.summ.rain.10)
range(sd_bena_cch$MooseDensity)

# Predict
MyData_b<-expand.grid(iem.summ.temp = seq(6, 18, length = 190),
                      iem.summ.rain.10 = SR, 
                      MooseDensity = seq(0.05, 0.65, length = 190))


MyData_b$Pred <- predict(Optimal_model_b, MyData_b, level = 0) #Predicts the values based on model

# Calculate SEs
Designmat <- model.matrix(formula(Optimal_model_b)[-2], MyData_b) ## [-2] drops response from formula
predvar <- diag(Designmat %*% vcov(Optimal_model_b) %*% t(Designmat)) 
MyData_b$SE <- sqrt(predvar)
MyData_b$SEup<-MyData_b$SE+MyData_b$Pred
MyData_b$SEdown<-MyData_b$Pred-MyData_b$SE

# Plot
col.l = colorRampPalette(c('white', rgb(0, 80, 158, max = 255)))
z = c(0:10)
pM = contourplot(Pred ~ iem.summ.temp + MooseDensity,
                 data=MyData_b,
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

pM = pM + contourplot(SEdown ~ iem.summ.temp * MooseDensity, 
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

pM

trellis.focus("panel", 1, 1, highlight=F)

lpoints(sd_bena_cch$iem.summ.temp, y = sd_bena_cch$MooseDensity, 
        col = rgb(red = 0, green = 0, blue = 0, alpha = 0.1), 
        pch = 4, cex = 0.65)


# DEVELOP HEAT MAPS FOR OPTIMAL SALIX MODEL WITH INTERACTION MD:HI ####
HI = mean(sd_salix_cch$HareIndex)
MD = mean(sd_salix_cch$MooseDensity)
SR = mean(sd_salix_cch$iem.summ.rain)
ST = mean(sd_salix_cch$iem.summ.temp)
range(sd_salix_cch$iem.summ.temp)
range(sd_salix_cch$iem.summ.rain.10)

# Predict
MyData_s<-expand.grid(iem.summ.temp = ST,  
                      iem.summ.rain.10 = SR, 
                      HareIndex = seq(.75, 3.25, length = 190),
                      MooseDensity = seq(0.05, 0.65, length = 190))

MyData_s$Pred <- predict(Optimal_model_s, MyData_s, level = 0) #Predicts the values based on model

# Calculate SEs
Designmat <- model.matrix(formula(Optimal_model_s)[-2], MyData_s) ## [-2] drops response from formula
predvar <- diag(Designmat %*% vcov(Optimal_model_s) %*% t(Designmat)) 
MyData_s$SE <- sqrt(predvar)
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

