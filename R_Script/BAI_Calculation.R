# Shrub Basal Area Increment (BAI) Calculation
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-11-21

install.packages("dplR")
install.packages("reshape2")
install.packages('plyr')
install.packages('gam')
install.packages('foreach')
install.packages('nlme')
install.packages('lme4')
install.packages('aomisc')
install.packages('lmerTest') 
install.packages("MuMIn")
install.packages("modelr")

library(dplR)
library(dplyr)
library(reshape2)
library(plyr)
library(gam)
library(foreach)
library(ggplot2)
library(nlme)
library(lme4)
library(lmtest)
library(lmerTest)
library(MuMIn)
library(aomisc)
library(modelr)

#BAI Calculation using measured ring widths (mm) from Image J.
#Ring widths are listed from the bark to the pith, so the function bai.out in the dplR package will be used.

# 1. CALCULATE THE MEAN OF THE 4 MEASURED RADII ####

#Calculates mean ring width per year from the  4 measured radii
rw_mean = aggregate(x = rw_base,
                      by = list(rw_base$ShrubID),
                      FUN = mean,
                      na.rm = TRUE)

#Removes unused colums from the aggregated dataset 
rw_mean = select (rw_mean, -c (2))

#Changes the column name output from the aggregate function to ShrubID

colnames(rw_mean)[colnames(rw_mean)=="Group.1"] = "ShrubID"

# 2. TRANSPOSE MEAN RING WIDTH DATA ####

#Transposes the data into the format reqired for the bai.out function
rw_mean_t = as.data.frame (t(rw_mean))
  
  #Changes the column names of the newly transposed data frame and deletes the unnecesary first row
    ShrubID = rw_mean$ShrubID

    colnames(rw_mean_t) = ShrubID

    rw_mean_t = rw_mean_t[-c(1), ]


      # 2.1. SUBSET RW DATA BY SPECIES ####
    
#Creates a new data frames from the full dataset based on species

rw_mean_BENA = data.frame(rw_mean_t %>% dplyr:: select (grep("BENA", names(rw_mean_t))), check.names = FALSE)    

rw_mean_SALIX = data.frame(rw_mean_t %>% dplyr:: select (grep("SA", names(rw_mean_t))), check.names = FALSE)

    #rw_mean_SAPU = data.frame(rw_mean_t %>% dplyr:: select (grep("SAPU", names(rw_mean_t))), check.names = FALSE)
    
    #rw_mean_SABE = data.frame(rw_mean_t %>% dplyr:: select (grep("SABE", names(rw_mean_t))), check.names = FALSE)
    
    #rw_mean_SAGL = data.frame(rw_mean_t %>% dplyr:: select (grep("SAGL", names(rw_mean_t))), check.names = FALSE)

      # 2.2. SUBSET RW DATA BY SITE ####
                
#Creates a new data frames from the full dataset based on the site number

mean_rwl_60 = data.frame(rw_mean_t %>% dplyr:: select (grep("60", names(rw_mean_t))), check.names = FALSE)

mean_rwl_72 = data.frame(rw_mean_t %>% dplyr:: select (grep("72", names(rw_mean_t))), check.names = FALSE)

mean_rwl_86 = data.frame(rw_mean_t %>% dplyr:: select (grep("86", names(rw_mean_t))), check.names = FALSE)

mean_rwl_97 = data.frame(rw_mean_t %>% dplyr:: select (grep("97", names(rw_mean_t))), check.names = FALSE)

mean_rwl_102 = data.frame(rw_mean_t %>% dplyr:: select (grep("102", names(rw_mean_t))), check.names = FALSE)

mean_rwl_111 = data.frame(rw_mean_t %>% dplyr:: select (grep("111", names(rw_mean_t))), check.names = FALSE)

mean_rwl_121 = data.frame(rw_mean_t %>% dplyr:: select (grep("121", names(rw_mean_t))), check.names = FALSE)

mean_rwl_131 = data.frame(rw_mean_t %>% dplyr:: select (grep("131", names(rw_mean_t))), check.names = FALSE)

mean_rwl_147 = data.frame(rw_mean_t %>% dplyr:: select (grep("147", names(rw_mean_t))), check.names = FALSE)

mean_rwl_151 = data.frame(rw_mean_t %>% dplyr:: select (grep("151", names(rw_mean_t))), check.names = FALSE)

mean_rwl_158 = data.frame(rw_mean_t %>% dplyr:: select (grep("158", names(rw_mean_t))), check.names = FALSE)

mean_rwl_173 = data.frame(rw_mean_t %>% dplyr:: select (grep("173", names(rw_mean_t))), check.names = FALSE)

mean_rwl_184 = data.frame(rw_mean_t %>% dplyr:: select (grep("184", names(rw_mean_t))), check.names = FALSE)

mean_rwl_191 = data.frame(rw_mean_t %>% dplyr:: select (grep("191", names(rw_mean_t))), check.names = FALSE)

mean_rwl_203 = data.frame(rw_mean_t %>% dplyr:: select (grep("203", names(rw_mean_t))), check.names = FALSE)

mean_rwl_209 = data.frame(rw_mean_t %>% dplyr:: select (grep("209", names(rw_mean_t))), check.names = FALSE)

mean_rwl_222 = data.frame(rw_mean_t %>% dplyr:: select (grep("222", names(rw_mean_t))), check.names = FALSE)

mean_rwl_235 = data.frame(rw_mean_t %>% dplyr:: select (grep("235", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_240 = data.frame(rw_mean_t %>% dplyr:: select (grep("240", names(rw_mean_t))), check.names = FALSE)

mean_rwl_255 = data.frame(rw_mean_t %>% dplyr:: select (grep("255", names(rw_mean_t))), check.names = FALSE)

mean_rwl_261 = data.frame(rw_mean_t %>% dplyr:: select (grep("261", names(rw_mean_t))), check.names = FALSE)

mean_rwl_268 = data.frame(rw_mean_t %>% dplyr:: select (grep("268", names(rw_mean_t))), check.names = FALSE)

mean_rwl_282 = data.frame(rw_mean_t %>% dplyr:: select (grep("282", names(rw_mean_t))), check.names = FALSE)

      # 2.3. SUBSET RW DATA BY SITE & SPECIES ####

#Creates a new data frames from the BENA dataset based on the site number 
mean_rwl_60_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("60", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_72_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("72", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_86_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("86", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_97_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("97", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_102_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("102", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_111_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("111", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_121_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("121", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_131_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("131", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_147_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("147", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_151_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("151", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_158_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("158", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_173_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("173", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_184_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("184", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_191_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("191", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_203_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("203", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_209_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("209", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_222_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("222", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_235_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("235", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_240_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("240", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_255_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("255", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_261_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("261", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_268_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("268", names(rw_mean_BENA))), check.names = FALSE)

mean_rwl_282_BENA = data.frame(rw_mean_BENA %>% dplyr:: select (grep("282", names(rw_mean_BENA))), check.names = FALSE)

#Creates a new data frames from the SALIX dataset based on the site number 
mean_rwl_60_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("60", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_72_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("72", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_86_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("86", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_97_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("97", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_102_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("102", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_111_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("111", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_121_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("121", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_131_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("131", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_147_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("147", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_151_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("151", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_158_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("158", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_173_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("173", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_184_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("184", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_191_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("191", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_203_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("203", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_209_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("209", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_222_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("222", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_235_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("235", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_240_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("240", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_255_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("255", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_261_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("261", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_268_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("268", names(rw_mean_SALIX))), check.names = FALSE)

mean_rwl_282_SALIX = data.frame(rw_mean_SALIX %>% dplyr:: select (grep("282", names(rw_mean_SALIX))), check.names = FALSE)

# 3. CREATE RWL FILES ####

#Remove rows that only have NA values 
rw_mean_t = rw_mean_t[rowSums(is.na(rw_mean_t)) != ncol(rw_mean_t), ]

#Create a temporary csv file from the mean ring width legnth data,
#Then uses the csv2rwl function to convert to a rwl file usable in dplR
tmpName = tempfile()
    
write.csv( rw_mean_t,file = tmpName)
    
rw_mean_t = csv2rwl(tmpName)


# 4. CREATE DIAM FILES ####

#Calculates mean legnth of each of the 4 radii
diam_mean = aggregate(x = diam,
                      by = list(diam$ShrubID),
                      FUN = mean,
                      na.rm = TRUE)

#Removes unused colums from the aggregated dataset 
diam_mean = select (diam_mean, -c (2))

#Changes the column name output from the aggregate function to ShrubID
colnames(diam_mean)[colnames(diam_mean)=="Group.1"] = "ShrubID"

#Multiplies the averaged radii values by 2 to get the diameter of the stem - the bark
diam_mean$diam = diam_mean$Radius * 2

#Deletes the old radii column
diam_mean = select (diam_mean, -c (2, 3))

#For the bai calculation we need a dataframe with two colums:
# 1 ShrubID which exactly matchs the ShrubID from the RWL file
# 2 Diameter measurments in a column labeled "diam"

#To get the exact column labels from the RWL file we will transpose and extract them
rw_shrubID = as.data.frame (t(rw_mean_t))

rw_shrubID = row.names(rw_shrubID)

diam_mean = cbind(rw_shrubID, diam_mean)

diam_mean = select (diam_mean, -c (0,2))

colnames(diam_mean)[colnames(diam_mean)=="rw_shrubID"] = "ShrubID"

# 5. CALCULATE BAI ####

#Use the bai.out tool from dplR to calculate the basal area increment going from 
#the bark to the pith. 

bai = bai.out(rwl = rw_mean_t, diam = diam_mean)

  #bai_no_diam = bai.out(rwl = rw_mean_t) 
  #After talks with Kata on 01/15/2020 I've decided to specify a diam for each shrub. The diam file is the mean radius for 
  #each shrub (including the pith) x2. If a diam is not specified the bai.out calculation will sum all the increments from
  #the rwl file and multiply it by 2. In my data this would not include the pit in the overall diameter, therefore we will
  #include the diam file to get the most accurate representation of the shrub. 

# 6. TRANSPOSE BAI DATA ####

#Transposes the data into a format which can be melted and joined back to the shrub data
bai = data.frame (t(bai))

# 7. TRUNCATE BAI DATA ####

#Remove the first row, 2019, which can not be used in the analysis
bai$X2019 = NULL

# 8. ADD SHRUB-ID AND RENAME COLUMNS  ####

#Adds the Shrub ID values to the transposed BAI data set 
bai = cbind(ShrubID, bai)

#Removes the row.name and then renames all the columns   
bai = setNames(cbind(rownames(bai), bai, row.names = NULL), 
            c("Shrub.ID", "ShrubID", "2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1984","1983","1982","1981","1980","1979","1978","1977","1976","1975","1974","1973","1972","1971","1970","1969","1968","1967","1966","1965","1964","1963","1962","1961","1960","1959","1958","1957","1956","1955","1954","1953"))

#Removes the Shrub ID column which was added during the BAI calculation
bai$Shrub.ID = NULL

# 9. MELT BIA DATA AND RW DATA  ####

#Melts the BAI data, ignoring all NA values
bai_melt = melt(bai, id="ShrubID", na.rm = TRUE)

#Changes the field names of the melted data set to Year and BAI
colnames(bai_melt)[colnames(bai_melt)=="variable"] = "Year"
colnames(bai_melt)[colnames(bai_melt)=="value"] = "BAI"

#Removes 2019 from the ring width data prior to melting
rw_mean$`2019` = NULL

#Melts the ring width data, ignoring all NA values
rw_mean_melt = melt(rw_mean, id = "ShrubID", na.rm = TRUE)

#Changes the field names of the melted data set to Year and BAI
colnames(rw_mean_melt)[colnames(rw_mean_melt)=="variable"] = "Year"
colnames(rw_mean_melt)[colnames(rw_mean_melt)=="value"] = "RingWidth"


# 10. JOIN SHRUB DATA TO THE MELTED BAI AND RW DATA  ####

#Adds the melted ring width data to the melted bai data
bai_rw_all = cbind(rw_mean_melt$RingWidth, bai_melt)

#Reorders the columns after the ring width values are added
bai_rw_all = bai_rw_all[,c(2,3,1,4)]

#Renames the added column 
colnames(bai_rw_all)[colnames(bai_rw_all)=="rw_mean_melt$RingWidth"] = "RingWidth"

#Joins the ring width and bai data to the shrub age using the ShrubID as the key 
bai_rw_age = join(bai_rw_all, age, by='ShrubID', type='left', match='all')

# Joins the ring width and bai data to the shrub data using the ShrubID as the key 
sd_join = join(bai_rw_age, shrub_data_ss, by='ShrubID', type='left', match='all')

str(sd_join)

#Creates an Age Column which is the year - the date of stem establishment

  #Year column must be made numeric first
  sd_join$Year = as.numeric(as.character(sd_join$Year))

sd_join$Age = sd_join$Year - sd_join$Estab

write.csv(sd_join, "/Users/peterfrank/Documents/Master's Thesis/DataAnalysis/AlaskaShrubs/R_Data/Shrub_BAI.csv")


# 11. SUBSET DATA BY GENUS & SPECIES ####

#Subsets the data by species
sd_bena = subset(sd_join, Species == "BENA")

sd_salix = subset(sd_join, grepl("^SA", sd_join$Species)) 

#Removes all age values less than 5 years 
sd_bena5 = filter(sd_bena, Age > 5)

sd_salix5 = filter(sd_salix, Age > 5)

#Other Potential Subsets
  #sd_bena5_255 = subset(sd_bena5, Section == 20)
  #sd_sapu = subset(sd_join, Species == "SAPU", select = ShrubID : Species)
  #sd_join_sabe = subset(sd_join, Species == "SABE", select = ShrubID : Species)
  #sd_sagl = subset(sd_join, Species == "SAGL", select = ShrubID : Species)

# 12. PLOT BAI VS AGE ####

#Plots BAI as a function of age on a single graph
par(mfrow=c(1,1))
    
plot(BAI ~ Age, data = sd_join,
      col = "black", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)", ylim=c(0, 95))
    
#Plots BAI as a function of age on two graphs by genus
par(mfrow=c(1,2))

plot(BAI ~ Age, data = sd_bena5,
     col = "black", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)", ylim=c(0, 50), main = "Betula")

plot(BAI ~ Age, data = sd_salix5,
     col = "blue", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)", ylim=c(0, 90), main = "Salix")

#Plots BAI as a function of age on two graphs by genus in log-log scale
par(mfrow=c(1,2))

plot(log(BAI) ~ log(Age), data = sd_bena5,
     col = "black", pch = 1, ylab = "ln Basal Area Increment", xlab = "ln Ring Age", cex.lab = 1.5) #, main = "Betula")

abline (lmBena, col = "red")

summary(lmBena)

plot(log(BAI) ~ log(Age), data = sd_salix5,
     col = "blue", pch = 1, ylab = "ln Basal Area Increment", xlab = "ln Ring Age") #, main = "Salix")

abline (lmSalix,  col = "red")

summary(lmSalix)


#Plots BAI as a function of age on four graphs by species
par(mfrow=c(2,2))

plot(BAI ~ Age, data = sd_bena5,
     col = "black", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)", ylim=c(0, 93), main = "Betula nana")
 
plot(BAI ~ Age, data = sd_sapu5,
     col = "blue", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)", ylim=c(0, 93), main = "Salix pulchra")

plot(BAI ~ Age, data = sd_sabe5,
     col = "blue", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)", ylim=c(0, 93), main = "Salix bebbiana")

plot(BAI ~ Age, data = sd_sagl5,
     col = "blue", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)", ylim=c(0, 93), main = "Salix glauca")

#Check the distributions of age and BAI data on a histogram
#Note that both age and BAI are not normally distributed, the distribution is right (positive) skewed toward small BAI or young individuals.
par(mfrow=c(2,2))
    
hist(sd_bena5$Age, xlab = "Ring Age (years)", main = "Betula nana Age Distribution")
hist(sd_bena5$BAI, xlab = "Basal Area Increment", main = "Betula nana BAI Distribution")

hist(sd_salix5$Age, xlab = "Ring Age (years)", main = "Salix spp. Age Distribution")
hist(sd_salix5$BAI, xlab = "Basal Area Increment", main = "Salix spp. BAI Distribution")

# 13. CREATE LINEAR REGRESSION MODELS ####
lmBena = lm(log(BAI) ~ log(Age), data = sd_bena5)
lmSalix = lm(log(BAI) ~ log(Age), data = sd_salix5)

#Generate the Summary Statistic
summary(lmBena)
summary(lmSalix)

#Model assumption testing show us that we have very strong right skew in the data

    #Plot Risiduals vs Fitter and Normal Q-Q for Betula
    par(mfrow=c(2,2))
    plot (lmBena)
    
    #Plot Risiduals vs Fitter and Normal Q-Q for Salix
    par(mfrow=c(2,2))
    plot (lmSalix)
    
    #Run a Durbin Watson test to check for autocorrelation in the residuals (closer to 2 = better)
    dwtest(lmBena)
    dwtest(lmSalix)

# 14. CREATE LINEAR MIXED EFFECTS MODELS ####
    
#Create a linear mixed effects model which models BAI as a function of age
#The mixed effects model is used becuase it allows us to model the fixed effects of BAI and age
#while accounting for the random effects of the individual shrub and the section. The random effects
#allow for the possibility that our covariates have effects that vary from unit (section, shrub) to unit.
#We are also log transforming both BAI and age to reslove 
  
#Betula nana linear mixed effects model 
lmeBena = lme (log(BAI) ~ log(Age), random =~ 1 | Section/ShrubID, data = sd_bena5)    

# Summarizes the linear mixed effects model
summary(lmeBena)
r.squaredGLMM(lmeBena)
summary (lmerTest::lmer (log(BAI) ~ log(Age) + (1 | Section/ShrubID), data = sd_bena5))

# Plots the the Residuals vs Fitted graph and Normal Q-Q Plot for the lme
plot (lmeBena)
qqnorm (lmeBena)

#Plots the log transformed data and generates a the linear model 
ggplot(sd_bena5, aes(x = log(Age), y = log(BAI))) + geom_point() + stat_smooth(method = "lm", size = 1)

#------------------------------------------------------------------------------------------------------#

#Salix spp linear mixed effects model 
lmeSalix = lme (log(BAI) ~ log(Age), random =~ 1 | Section/ShrubID, data = sd_salix5)  

# Summarizes the linear mixed effects model
summary(lmeSalix)
r.squaredGLMM(lmeSalix)
summary (lmerTest::lmer (log(BAI) ~ log(Age) + (1 | Section/ShrubID), data = sd_salix5))

# Plots the the Residuals vs Fitted graph and Normal Q-Q Plot for the lme
plot (lmeSalix)
qqnorm(lmeSalix)

#Plots the log transformed data and generates a the linear model 
ggplot(sd_salix5, aes(x = log(Age), y = log(BAI))) + geom_point() + stat_smooth(method = "lm", size = 1)
    
    
# 15. EXTRACT RESIDUALS FROM LINEAR MIXED EFFECTS MODEL ####

#Extract the residuals from the linear model: his is variation that can be expected to be explained by something else than age.

sd_bena_res = add_residuals(sd_bena5, lmBena)

sd_salix_res = add_residuals(sd_salix5, lmSalix)

#Transform residuals back into BAI scale from the log scale

#sd_bena_res$resid_t = exp(1)^(sd_bena_res$resid)

#sd_salix_res$resid_t = exp(1)^(sd_salix_res$resid)

#Combine the two species back into one dataset

sd_all = rbind(sd_bena_res, sd_salix_res)



