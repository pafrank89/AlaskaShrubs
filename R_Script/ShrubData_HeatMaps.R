
sd_final_cch$iem.summ.rain.10 = sd_final_cch$iem.summ.rain/10

#My final Model
CH2_model = lme(resid ~ iem.summ.temp + iem.summ.rain + iem.summ.temp * iem.summ.rain +
                HareIndex, 
                data = sd_final_cch, random = ~ 1|Section/ShrubID,
                method = "ML")

HI<-mean(sd_final_cch$HareIndex)

range(sd_final_cch$iem.summ.temp)
range(sd_final_cch$iem.summ.rain.10)

# 4. Predict
MyData<-expand.grid(iem.summ.temp = seq(6, 18, length = 100),   #min and max of temp
                    iem.summ.rain = seq(30, 480, length = 100), #min and max of pre
                    HareIndex = HI
                    
)
MyData$Pred <- predict(CH2_model, MyData, level = 0) #Predicts the values based on model

# Calculate SEs
Designmat <- model.matrix(formula(CH2_model)[-2], MyData) ## [-2] drops response from formula
predvar <- diag(Designmat %*% vcov(CH2_model) %*% t(Designmat)) 
MyData$SE <- sqrt(predvar)
MyData$SEup<-MyData$SE+MyData$Pred
MyData$SEdown<-MyData$Pred-MyData$SE


# 5. Plot
col.l <- colorRampPalette(c( 'white','darkgreen'))
z<-c(0:10)
p<-contourplot(Pred ~ iem.summ.temp + iem.summ.rain,
               data=MyData,
               xlab="mean summer temperature",
               ylab="mean summer precip",
               pretty=TRUE,
               lty=1,
               zlim=range(z, finite=TRUE),
               lwd=0.5,
               labels=list(cex=1),
               col.regions=col.l,
               region=TRUE,
               main=list("Shrub radial growth (Age standardized BAI mm2)", cex=1)
)

p

#Plot Standard error lines 
p<-p+contourplot(SEup ~ iem.summ.temp + iem.summ.rain, 
                 data=MyData,
                 cuts=10,
                 at = c(74), #change these when you see the plot
                 pretty=TRUE,
                 lty=2,
                 zlim = range(z, finite = TRUE),
                 lwd=0.5,
                 labels=list(cex=0),
                 region=FALSE,
                 main = list("Shrub radial growth (Age standardized BAI (mm2))", cex = 1)
)
p<-p+contourplot(SEdown ~ iem.summ.temp + iem.summ.rain, 
                 data=MyData,
                 cuts=10,
                 at = c(74), #change these when you see the plot
                 pretty=TRUE,
                 lty=2,
                 zlim = range(z, finite = TRUE),
                 lwd=0.5,
                 labels=list(cex=0),
                 region=FALSE,
                 main = list("Shrub radial growth (Age standardized BAI (mm2))", cex = 1
                             , font=1)
)



p



# plot color by growth & size by sample size:

growth<-tapply(denfPin$res,list(denfPin$YearCal),mean)     
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