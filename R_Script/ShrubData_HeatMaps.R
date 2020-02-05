# DEVELOP HEAT MAPS FOR OPTIMAL BETULA MODEL ####
CH1H2_model_b = lme(resid ~ iem.summ.temp * iem.summ.rain.10 + iem.summ.temp + iem.summ.rain.10 +
                HareIndex + MooseDensity, 
                data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                method = "REML")

summary(CH1H2_model_b)

HI = mean(sd_bena_cch$HareIndex)
MD = mean(sd_bena_cch$MooseDensity)
range(sd_bena_cch$iem.summ.temp)
range(sd_bena_cch$iem.summ.rain.10)

# Predict
MyData_b<-expand.grid(iem.summ.temp = seq(6, 18, length = 100),   #min and max of temp
                    iem.summ.rain.10 = seq(3, 48, length = 100), #min and max of pre
                    HareIndex = HI, MooseDensity = MD)


MyData_b$Pred <- predict(CH1H2_model_b, MyData_b, level = 0) #Predicts the values based on model

# Calculate SEs
Designmat <- model.matrix(formula(CH1H2_model_b)[-2], MyData_b) ## [-2] drops response from formula
predvar <- diag(Designmat %*% vcov(CH1H2_model_b) %*% t(Designmat)) 
MyData_b$SE <- sqrt(predvar)
MyData_b$SEup<-MyData_b$SE+MyData_b$Pred
MyData_b$SEdown<-MyData_b$Pred-MyData_b$SE

# Plot
col.l = colorRampPalette(c( 'white','darkgreen'))
z = c(0:10)
pb = contourplot(Pred ~ iem.summ.temp + iem.summ.rain.10,
               data=MyData_b,
               xlab="mean summer temperature",
               ylab="mean summer precip",
               pretty=TRUE,
               lty=1,
               zlim=range(z, finite=TRUE),
               lwd=0.5,
               labels=list(cex=1),
               col.regions=col.l,
               region=TRUE,
               main=list("Betula radial growth (residual values)", cex=1))

pb

# Plot Standard error lines 
pb = pb + contourplot(SEup ~ iem.summ.temp + iem.summ.rain.10, 
                 data = MyData_b,
                 cuts=10,
                 at = c(0.2), #change these when you see the plot
                 pretty=TRUE,
                 lty=2,
                 zlim = range(z, finite = TRUE),
                 lwd=0.5,
                 labels = list(cex=0),
                 region=FALSE,
                 main = list("Betula radial growth (residual values)", cex = 1))

pb = pb + contourplot(SEdown ~ iem.summ.temp + iem.summ.rain.10, 
                 data=MyData_b,
                 cuts=10,
                 at = c(0.2), #change these when you see the plot
                 pretty=TRUE,
                 lty=2,
                 zlim = range(z, finite = TRUE),
                 lwd=0.5,
                 labels=list(cex=0),
                 region=FALSE,
                 main = list("Betula radial growth (residual values)", cex = 1, font=1))

pb

# DEVELOP HEAT MAPS FOR OPTIMAL BETULA MODEL WITH INTERACTIONS####
CH_I_model_b = lme(resid ~ iem.summ.temp * iem.summ.rain.10 + iem.summ.temp + iem.summ.rain.10 +
                    HareIndex + MooseDensity +
                    iem.summ.temp:HareIndex, 
                    data = sd_bena_cch, random = ~ 1|Section/ShrubID,
                    method = "REML")

summary(CH_I_model_b)

HI = mean(sd_bena_cch$HareIndex)
MD = mean(sd_bena_cch$MooseDensity)
range(sd_bena_cch$iem.summ.temp)
range(sd_bena_cch$iem.summ.rain.10)

# Predict
MyData_b<-expand.grid(iem.summ.temp = seq(6, 18, length = 100),   #min and max of temp
                      iem.summ.rain.10 = seq(3, 48, length = 100), #min and max of pre
                      HareIndex = seq(1, 3, length = 100),
                      MooseDensity = MD)


MyData_b$Pred <- predict(CH_I_model_b, MyData_b, level = 0) #Predicts the values based on model

# Calculate SEs
Designmat <- model.matrix(formula(CH_I_model_b)[-2], MyData_b) ## [-2] drops response from formula
predvar <- diag(Designmat %*% vcov(CH_I_model_b) %*% t(Designmat)) 
MyData_b$SE <- sqrt(predvar)
MyData_b$SEup<-MyData_b$SE+MyData_b$Pred
MyData_b$SEdown<-MyData_b$Pred-MyData_b$SE

# Plot
col.l = colorRampPalette(c( 'white','darkgreen'))
z = c(0:10)
pb = contourplot(Pred ~ iem.summ.temp + HareIndex,
                 data=MyData_b,
                 xlab="mean summer temperature",
                 ylab="Hare Index",
                 pretty=TRUE,
                 lty=1,
                 zlim=range(z, finite=TRUE),
                 lwd=0.5,
                 labels=list(cex=1),
                 col.regions=col.l,
                 region=TRUE,
                 main=list("Betula radial growth (residual values)", cex=1))

pb

# Plot Standard error lines 
pb = pb + contourplot(SEup ~ iem.summ.temp + iem.summ.rain.10, 
                      data = MyData_b,
                      cuts=10,
                      at = c(0.2), #change these when you see the plot
                      pretty=TRUE,
                      lty=2,
                      zlim = range(z, finite = TRUE),
                      lwd=0.5,
                      labels = list(cex=0),
                      region=FALSE,
                      main = list("Betula radial growth (residual values)", cex = 1))

pb = pb + contourplot(SEdown ~ iem.summ.temp + iem.summ.rain.10, 
                      data=MyData_b,
                      cuts=10,
                      at = c(0.2), #change these when you see the plot
                      pretty=TRUE,
                      lty=2,
                      zlim = range(z, finite = TRUE),
                      lwd=0.5,
                      labels=list(cex=0),
                      region=FALSE,
                      main = list("Betula radial growth (residual values)", cex = 1, font=1))

pb

# DEVELOP HEAT MAPS FOR OPTIMAL SALIX MODEL ####

CH1_model_s = lme(resid ~ iem.summ.temp + iem.summ.rain.10 + iem.summ.temp * iem.summ.rain.10 +
                  MooseDensity, 
                  data = sd_salix_cch, random = ~ 1|Section/ShrubID,
                  method = "REML")

summary(CH1_model_s)

MDs = mean(sd_salix_cch$MooseDensity)
range(sd_salix_cch$iem.summ.temp)
range(sd_salix_cch$iem.summ.rain.10)

# Predict
MyData_s<-expand.grid(iem.summ.temp = seq(6, 18, length = 100),   #min and max of temp
                    iem.summ.rain.10 = seq(3, 48, length = 100), #min and max of pre
                    MooseDensity = MDs)


MyData_s$Pred <- predict(CH1_model_s, MyData_s, level = 0) #Predicts the values based on model

# Calculate SEs
Designmat <- model.matrix(formula(CH1_model_s)[-2], MyData_s) ## [-2] drops response from formula
predvar <- diag(Designmat %*% vcov(CH1_model_s) %*% t(Designmat)) 
MyData_s$SE <- sqrt(predvar)
MyData_s$SEup<-MyData_s$SE+MyData_s$Pred
MyData_s$SEdown<-MyData_s$Pred-MyData_s$SE


# Plot
col.l = colorRampPalette(c( 'white','darkgreen'))
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
ps = ps + contourplot(SEup ~ iem.summ.temp + iem.summ.rain.10, 
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

ps = ps + contourplot(SEdown ~ iem.summ.temp + iem.summ.rain.10, 
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


# PLOT SAMPLES POINTS ON CONTURE GRAPHS ####

growth = tapply(sd_bena_cch$resid,list(sd_bena_cch$yearsitecombo, mean)) 


names <- rownames(growth)

rownames(growth) <- NULL

growth <- cbind(names,growth)

colnames(growth)[colnames(growth)=="names"] <- "YearCal"

deer<-tapply(denfPin$RZdeertotalkm2,list(denfPin$YearCal),mean)  

names <- rownames(deer)

rownames(deer) <- NULL

deer <- cbind(names,deer)

colnames(deer)[colnames(deer)=="names"] <- "YearCal"

temp<-tapply(denfPin$mean.temp,list(denfPin$YearCal),mean)   

names <- rownames(temp)

rownames(temp) <- NULL
temp <- cbind(names,temp)
colnames(temp)[colnames(temp)=="names"] <- "YearCal"
s<- count(denfPin, c('YearCal'))
plotpoints<-cbind(growth, deer, temp, s, by="YearCal")

plotpoints$deer<-as.numeric(as.character(plotpoints$deer))
plotpoints$temp<-as.numeric(as.character(plotpoints$temp))
plotpoints$freq<-as.numeric(as.character(plotpoints$freq))
plotpoints$growth<-as.numeric(as.character(plotpoints$growth))

plotpoints$freq<-(plotpoints$freq)/2




p
trellis.focus("panel", 1, 1, highlight=F)
lpoints(plotpoints$temp, y = plotpoints$deer, 
        cex=c(plotpoints$freq),
        # outline color
        col=c(col.l(15), by=plotpoints$growth),
        
        pch=19) # type
lpoints(plotpoints$temp, y = plotpoints$deer, 
        cex=c(plotpoints$freq),
        col="black", # outline color
        bg=c('col.l', by=plotpoints$growth),
        pch=21) # type