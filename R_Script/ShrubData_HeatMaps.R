install.packages("qualityTools")
library(qualityTools)

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

MyData_bm$Pred <- predict(HM_B_MST_HD, MyData_bm, level = 0) #Predicts the values based on model

# Calculate SEs
Designmat_b2 <- model.matrix(formula(HM_B_MST_HD)[-2], MyData_bm) ## [-2] drops response from formula
predvar_b2 <- diag(Designmat_b2 %*% vcov(HM_B_MST_HD) %*% t(Designmat_b2)) 
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


pM = pM + contourplot(SEdown ~ iem.summ.temp * HareIndex, 
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

lpoints(sd_bena_cchS$iem.summ.temp, y = sd_bena_cch$HareIndex, 
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

