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
<<<<<<< HEAD
=======
library(gam)
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68
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
<<<<<<< HEAD
rw_mean$ShrubID = NULL
=======
rw_mean = select (rw_mean, -c (2))
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68

#Changes the column name output from the aggregate function to ShrubID

colnames(rw_mean)[colnames(rw_mean)=="Group.1"] = "ShrubID"

# 2. TRANSPOSE MEAN RING WIDTH DATA ####

#Transposes the data into the format reqired for the bai.out function
rw_mean_t = as.data.frame (t(rw_mean))
  
  #Changes the column names of the newly transposed data frame and deletes the unnecesary first row
    ShrubID = rw_mean$ShrubID

    colnames(rw_mean_t) = ShrubID

    rw_mean_t = rw_mean_t[-c(1), ]
<<<<<<< HEAD
=======


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

>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68
# 3. CREATE RWL FILES ####

#Remove rows that only have NA values 
rw_mean_t = rw_mean_t[rowSums(is.na(rw_mean_t)) != ncol(rw_mean_t), ]

#Create a temporary csv file from the mean ring width legnth data,
#Then uses the csv2rwl function to convert to a rwl file usable in dplR
tmpName = tempfile()
    
write.csv( rw_mean_t,file = tmpName)
    
rwl_out = csv2rwl(tmpName)


# 4. CALCULATE RING WIDTH INDEX (RWI) ####

<<<<<<< HEAD
rwi_spline = detrend(rwl_out, method = "Spline", make.plot = TRUE)

rwi_negexp = detrend(rwl_out, method = "ModNegExp", make.plot = TRUE)
=======
rwi_spline = detrend(rw_mean_t, method = "Spline", make.plot = TRUE)

rwi_negexp = detrend(rw_mean_t, method = "ModNegExp", make.plot = TRUE)
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68

# 5. CREATE DIAM & D2PITH FILES ####

#Calculates mean legnth of each of the 4 radii
<<<<<<< HEAD

#diam_mean = aggregate(x = diam,
                      #by = list(diam$ShrubID),
                      #FUN = mean,
                      #na.rm = TRUE)
=======
diam_mean = aggregate(x = diam,
                      by = list(diam$ShrubID),
                      FUN = mean,
                      na.rm = TRUE)
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68

pith_mean = aggregate(x = pith,
                      by = list(pith$`Shrub ID`),
                      FUN = mean,
                      na.rm = TRUE)

#Removes unused colums from the aggregated dataset 
<<<<<<< HEAD
#diam_mean$ShrubID = NULL

pith_mean$`Shrub ID` = NULL

#Changes the column name output from the aggregate function to ShrubID
#colnames(diam_mean)[colnames(diam_mean)=="Group.1"] = "ShrubID"
=======
diam_mean = select (diam_mean, -c (2))

pith_mean = select (pith_mean, -c (2))

#Changes the column name output from the aggregate function to ShrubID
colnames(diam_mean)[colnames(diam_mean)=="Group.1"] = "ShrubID"
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68

colnames(pith_mean)[colnames(pith_mean)=="Group.1"] = "ShrubID"

#Multiplies the averaged radii values by 2 to get the diameter of the stem - the bark
<<<<<<< HEAD
#diam_mean$diam = diam_mean$Radius * 2

#Deletes the old radii column
#diam_mean$Radii = NULL

#diam_mean$Radius = NULL
=======
diam_mean$diam = diam_mean$Radius * 2

#Deletes the old radii column
diam_mean = select (diam_mean, -c (2, 3))
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68

#For the bai calculation we need a dataframe with two colums:
# 1 ShrubID which exactly matchs the ShrubID from the RWL file
# 2 Diameter measurments in a column labeled "diam"

#To get the exact column labels from the RWL file we will transpose and extract them
<<<<<<< HEAD
rw_shrubID = as.data.frame (t(rwl_out))

rw_shrubID = row.names(rw_shrubID)

#diam_mean = cbind(rw_shrubID, diam_mean)

d2pith = cbind(rw_shrubID, pith_mean)

#diam_mean$shrubID = NULL

d2pith$ShrubID = NULL

#colnames(diam_mean)[colnames(diam_mean)=="rw_shrubID"] = "ShrubID"
=======
rw_shrubID = as.data.frame (t(rw_mean_t))

rw_shrubID = row.names(rw_shrubID)

diam_mean = cbind(rw_shrubID, diam_mean)

d2pith = cbind(rw_shrubID, pith_mean)

diam_mean = select (diam_mean, -c (0,2))

d2pith$ShrubID = NULL

colnames(diam_mean)[colnames(diam_mean)=="rw_shrubID"] = "ShrubID"
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68

colnames(d2pith)[colnames(d2pith)=="rw_shrubID"] = "ShrubID"

colnames(d2pith)[colnames(d2pith)=="Pith"] = "d2pith"


# 6. CALCULATE BAI ####

### 6.1 Reorded RWL File ###
# The bai.out function in dplr contains an error which makes the bai.in and bai.out functions preform the same calculation
# which is always calculating from the pith to the bark. Therefore we must reorder the RWL file such that the the earliest
# year of the RWL file is on the topmost row

<<<<<<< HEAD
Years = rownames(rwl_out)

rwl_in = cbind(Years, rwl_out)
=======
Years = rownames(rw_mean_t)

rwl_in = cbind(Years, rw_mean_t)
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68

rwl_in<-rwl_in %>% arrange(Years)

rownames(rwl_in) = rwl_in$Years

rwl_in$Years = NULL

#Use the bai.out tool from dplR to calculate the basal area increment going from 
#the bark to the pith. 

bai = bai.in(rwl = rwl_in, d2pith = d2pith)

<<<<<<< HEAD
str(bai)

=======
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68
write.csv(bai, "/Users/peterfrank/Desktop/baiIN_d2pith.csv")

#baiOUT_diam = bai.out(rwl = rwl_out, diam = diam_mean)

#write.csv(baiOUT_diam, "/Users/peterfrank/Desktop/baiOUT_diam.csv")

### TEST ALL PAIRS OF IN AND OUT DATA ###

# Use bai.in function with data aranged from the earliest year to the latest 
#baiIN = bai.in(rwl = rwl_in)

#write.csv(baiIN, "/Users/peterfrank/Desktop/baiIN.csv")

# Use bai.in function with data aranged from the latest year to the earliest 
#baiIN_wOUT = bai.in(rwl = rwl_out)

#write.csv(baiIN_wOUT, "/Users/peterfrank/Desktop/baiIN_wOUT.csv")

# Use bai.out function with data aranged from the latest year to the earliest 
#baiOUT = bai.out(rwl = rwl_out)

#write.csv(baiOUT, "/Users/peterfrank/Desktop/baiOUT.csv")

# Use bai.out function with data aranged from the earliest year to the latest
#baiOUT_wIN = bai.out(rwl = rwl_in)

#write.csv(baiOUT_wIN, "/Users/peterfrank/Desktop/baiOUT_wIN.csv")

  #bai_no_diam = bai.out(rwl = rw_mean_t) 
  #After talks with Kata on 01/15/2020 I've decided to specify a diam for each shrub. The diam file is the mean radius for 
  #each shrub (including the pith) x2. If a diam is not specified the bai.out calculation will sum all the increments from
  #the rwl file and multiply it by 2. In my data this would not include the pit in the overall diameter, therefore we will
  #include the diam file to get the most accurate representation of the shrub. 

# 7. TRANSPOSE BAI & RWI DATA ####

# Reverse the BAI data to go from oldest to youngest
Years = rownames(bai)

bai = cbind(Years, bai)

bai<-bai %>% arrange(desc(Years))

rownames(bai) = bai$Years

bai$Years = NULL

#Transposes the data into a format which can be melted and joined back to the shrub data
bai = data.frame (t(bai))

rwi_spline = data.frame (t(rwi_spline))

rwi_negexp = data.frame (t(rwi_negexp))

# 8. TRUNCATE BAI & RWI DATA ####

#Remove the first row, 2019, which can not be used in the analysis
bai$X2019 = NULL

rwi_spline$X2019 = NULL

rwi_negexp$X2019 = NULL

# 9. ADD SHRUB-ID AND RENAME COLUMNS  ####

#Adds the Shrub ID values to the transposed BAI data set 
bai = cbind(ShrubID, bai)

rwi_spline = cbind(ShrubID, rwi_spline)

rwi_negexp = cbind(ShrubID, rwi_negexp)

#Removes the row.name and then renames all the columns   
bai = setNames(cbind(rownames(bai), bai, row.names = NULL), 
            c("Shrub.ID", "ShrubID", "2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1984","1983","1982","1981","1980","1979","1978","1977","1976","1975","1974","1973","1972","1971","1970","1969","1968","1967","1966","1965","1964","1963","1962","1961","1960","1959","1958","1957","1956","1955","1954","1953"))

rwi_spline = setNames(cbind(rownames(rwi_spline), rwi_spline, row.names = NULL), 
               c("Shrub.ID", "ShrubID", "2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1984","1983","1982","1981","1980","1979","1978","1977","1976","1975","1974","1973","1972","1971","1970","1969","1968","1967","1966","1965","1964","1963","1962","1961","1960","1959","1958","1957","1956","1955","1954","1953"))

rwi_negexp = setNames(cbind(rownames(rwi_negexp), rwi_negexp, row.names = NULL), 
               c("Shrub.ID", "ShrubID", "2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1984","1983","1982","1981","1980","1979","1978","1977","1976","1975","1974","1973","1972","1971","1970","1969","1968","1967","1966","1965","1964","1963","1962","1961","1960","1959","1958","1957","1956","1955","1954","1953"))

#Removes the Shrub ID column which was added during the BAI calculation
bai$Shrub.ID = NULL

rwi_spline$Shrub.ID = NULL

rwi_negexp$Shrub.ID = NULL

# 10. MELT BIA & RWI DATA AND RW DATA  ####

#Melts the BAI data, ignoring all NA values
bai_melt = melt(bai, id="ShrubID", na.rm = TRUE)

rwi_s_melt = melt(rwi_spline, id="ShrubID", na.rm = TRUE)

rwi_n_melt = melt(rwi_negexp, id="ShrubID", na.rm = TRUE)

#Changes the field names of the melted data set to Year and BAI
colnames(bai_melt)[colnames(bai_melt)=="variable"] = "Year"
colnames(bai_melt)[colnames(bai_melt)=="value"] = "BAI"

colnames(rwi_s_melt)[colnames(rwi_s_melt)=="variable"] = "Year"
colnames(rwi_s_melt)[colnames(rwi_s_melt)=="value"] = "RWI_Spline"

colnames(rwi_n_melt)[colnames(rwi_n_melt)=="variable"] = "Year"
colnames(rwi_n_melt)[colnames(rwi_n_melt)=="value"] = "RWI_NegExp"

#Removes 2019 from the ring width data prior to melting
rw_mean$`2019` = NULL

#Melts the ring width data, ignoring all NA values
rw_mean_melt = melt(rw_mean, id = "ShrubID", na.rm = TRUE)

#Changes the field names of the melted data set to Year and BAI
colnames(rw_mean_melt)[colnames(rw_mean_melt)=="variable"] = "Year"
colnames(rw_mean_melt)[colnames(rw_mean_melt)=="value"] = "RingWidth"


# 11. JOIN SHRUB DATA TO THE MELTED BAI, RWI & RW DATA  ####

#Adds the melted ring width data to the melted bai data
bai_rwi_rw_all = cbind(rw_mean_melt$RingWidth, rwi_s_melt$RWI_Spline, rwi_n_melt$RWI_NegExp, bai_melt)

#Reorders the columns after the ring width values are added
bai_rwi_rw_all = bai_rwi_rw_all[,c(4,5,1,2,3,6)]

<<<<<<< HEAD
=======
bai_rwi_rw_all$`rwi_n_melt$RWI_NegExp`

>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68
#Renames the added column 
colnames(bai_rwi_rw_all)[colnames(bai_rwi_rw_all)=="rw_mean_melt$RingWidth"] = "RingWidth"

colnames(bai_rwi_rw_all)[colnames(bai_rwi_rw_all)=="rwi_s_melt$RWI_Spline"] = "RWI_Spline"

colnames(bai_rwi_rw_all)[colnames(bai_rwi_rw_all)=="rwi_n_melt$RWI_NegExp"] = "RWI_NegExp"

#Joins the ring width and bai data to the shrub age using the ShrubID as the key 
bai_rwi_rw_age = join(bai_rwi_rw_all, age, by='ShrubID', type='left', match='all')

# Joins the ring width and bai data to the shrub data using the ShrubID as the key 
sd_join = join(bai_rwi_rw_age, shrub_data_ss, by='ShrubID', type='left', match='all')

str(sd_join)

#Creates an Age Column which is the year - the date of stem establishment

  #Year column must be made numeric first
  sd_join$Year = as.numeric(as.character(sd_join$Year))

sd_join$Age = sd_join$Year - sd_join$Estab

write.csv(sd_join, "/Users/peterfrank/Documents/Master's Thesis/DataAnalysis/AlaskaShrubs/R_Data/Shrub_BAI.csv")


# 12. SUBSET DATA BY GENUS & SPECIES ####

#Subsets the data by species
sd_bena = subset(sd_join, Species == "BENA")

<<<<<<< HEAD
range(sd_bena$BAI) # 0.05187457 54.08233346 on 02/16/2020

sd_salix = subset(sd_join, grepl("^SA", sd_join$Species)) 

range(sd_salix$BAI) # 0.067783 121.081397 on 02/16/2020
=======
range(sd_bena$BAI)

sd_salix = subset(sd_join, grepl("^SA", sd_join$Species)) 

range(sd_salix$BAI)
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68

#Removes all age values less than 5 years 
sd_bena5 = filter(sd_bena, Age > 5)

sd_salix5 = filter(sd_salix, Age > 5)

#Other Potential Subsets
  #sd_bena5_255 = subset(sd_bena5, Section == 20)
  #sd_sapu = subset(sd_join, Species == "SAPU", select = ShrubID : Species)
  #sd_join_sabe = subset(sd_join, Species == "SABE", select = ShrubID : Species)
  #sd_sagl = subset(sd_join, Species == "SAGL", select = ShrubID : Species)

# 13. PLOT BAI VS AGE ####

#Plots BAI as a function of age on a single graph
par(mfrow=c(1,1))
    
plot(BAI ~ Age, data = sd_join,
      col = "black", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)", ylim=c(0, 95))
    
#Plots BAI as a function of age on two graphs by genus
par(mfrow=c(2,3))

#Betula
plot(RingWidth ~ Age, data = sd_bena5,
     col = "black", pch = 1, ylab = "Ring Width", xlab = "Ring Age (years)", main = "Betula")

plot(RWI_Spline ~ Age, data = sd_bena5,
     col = "black", pch = 1, ylab = "Ring Width Index (Spline)", xlab = "Ring Age (years)")

plot(RWI_NegExp ~ Age, data = sd_bena5,
     col = "black", pch = 1, ylab = "Ring Width Index (Negative Expontial)", xlab = "Ring Age (years)")

plot(BAI ~ Age, data = sd_bena5,
     col = "black", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)")

plot(log(BAI) ~ Age, data = sd_bena5,
     col = "black", pch = 1, ylab = "lnBasal Area Increment", xlab = "Ring Age (years)")

plot(log(BAI) ~ log(Age), data = sd_bena5,
     col = "black", pch = 1, ylab = "lnBasal Area Increment", xlab = "ln Ring Age (years)")

#Salix
plot(RingWidth ~ Age, data = sd_salix5,
     col = "blue", pch = 1, ylab = "Ring Width", xlab = "Ring Age (years)", main = "Salix")

plot(RWI_Spline ~ Age, data = sd_salix5,
     col = "blue", pch = 1, ylab = "Ring Width Index (Spline)", xlab = "Ring Age (years)")

plot(RWI_NegExp ~ Age, data = sd_salix5,
     col = "blue", pch = 1, ylab = "Ring Width Index (Negative Expontial)", xlab = "Ring Age (years)")

plot(BAI ~ Age, data = sd_salix5,
     col = "blue", pch = 1, ylab = "Basal Area Increment", xlab = "Ring Age (years)")

plot(log(BAI) ~ Age, data = sd_salix5,
     col = "blue", pch = 1, ylab = "lnBasal Area Increment", xlab = "Ring Age (years)")

plot(log(BAI) ~ log(Age), data = sd_salix5,
     col = "blue", pch = 1, ylab = "lnBasal Area Increment", xlab = "lnRing Age (years)")

#Plots BAI as a function of age on two graphs by genus in log-log scale
par(mfrow=c(1,2))

plot(BAI ~ Age, data = sd_bena5,
     col = "black", pch = 1, ylab = "ln Basal Area Increment", xlab = "ln Ring Age", cex.lab = 1) #, main = "Betula")

abline (lmBena, col = "red")

summary(lmBena)

plot(BAI ~ Age, data = sd_salix5,
     col = "blue", pch = 1, ylab = "ln Basal Area Increment", xlab = "ln Ring Age") #, main = "Salix")

abline (lmSalix,  col = "red")

summary(lmSalix)

# LOG BAI

par(mfrow=c(1,2))

plot(log(BAI) ~ Age, data = sd_bena5,
<<<<<<< HEAD
     col = "black", pch = 1, ylab = "ln Basal Area Increment", xlab = "Ring Age (years)", cex.lab = 1.6, cex.axis = 1.25) #, main = "Betula")

abline (lmBena, col = "red", lwd = 1.5)

summary(lmBena_l)

plot(log(BAI) ~ Age, data = sd_salix5,
     col = "blue", pch = 1, ylab = "ln Basal Area Increment", xlab = "Ring Age") #, main = "Salix")

abline (lmSalix_l,  col = "red")

summary(lmSalix_l)
=======
     col = "black", pch = 1, ylab = "ln Basal Area Increment", xlab = "ln Ring Age", cex.lab = 1) #, main = "Betula")

abline (lmBena, col = "red")

summary(lmBena)

plot(log(BAI) ~ Age, data = sd_salix5,
     col = "blue", pch = 1, ylab = "ln Basal Area Increment", xlab = "ln Ring Age") #, main = "Salix")

abline (lmSalix,  col = "red")

summary(lmSalix)
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68

# LOG BAI & LOG AGE

par(mfrow=c(1,2))

plot(log(BAI) ~ log(Age), data = sd_bena5,
     col = "black", pch = 1, ylab = "ln Basal Area Increment", xlab = "ln Ring Age", cex.lab = 1) #, main = "Betula")

abline (lmBena, col = "red")

<<<<<<< HEAD
summary(lmBena_ll)
=======
summary(lmBena)
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68

plot(log(BAI) ~ log(Age), data = sd_salix5,
     col = "blue", pch = 1, ylab = "ln Basal Area Increment", xlab = "ln Ring Age") #, main = "Salix")

abline (lmSalix,  col = "red")

<<<<<<< HEAD
summary(lmSalix_ll)
=======
summary(lmSalix)
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68

#Plots BAI as a function of age on two graphs by genus in gg-plots

ggplot(sd_bena5, aes(x=Age, y = BAI)) +
      geom_point(alpha = 0.5) +
      stat_smooth(method = "lm", formula = y ~ x, size = 1)

ggplot(sd_bena5, aes(x=Age, y = log(BAI))) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ x, size = 1)


ggplot(sd_bena5, aes(x=log(Age), y = log(BAI))) +
  geom_point(alpha = 0.5) +
  stat_smooth(method = "lm", formula = y ~ x, size = 1)

#Check the distributions of age and BAI data on a histogram
#Note that both age and BAI are not normally distributed, the distribution is right (positive) skewed toward small BAI or young individuals.
par(mfrow=c(2,2))
    
hist(sd_bena5$Age, xlab = "Ring Age (years)", main = "Betula nana Age Distribution")
hist(sd_bena5$BAI, xlab = "Basal Area Increment", main = "Betula nana BAI Distribution")

hist(sd_salix5$Age, xlab = "Ring Age (years)", main = "Salix spp. Age Distribution")
hist(sd_salix5$BAI, xlab = "Basal Area Increment", main = "Salix spp. BAI Distribution")

# 14. CREATE LINEAR REGRESSION MODELS ####
#lmBena = lm(BAI ~ Age, data = sd_bena5)
#lmSalix = lm(BAI ~ Age, data = sd_salix5)

lmBena = lm(log(BAI) ~ Age, data = sd_bena5)
lmSalix = lm(log(BAI) ~ Age, data = sd_salix5)

lmBena_l = lm(log(BAI) ~ Age, data = sd_bena5)
lmSalix_l = lm(log(BAI) ~ Age, data = sd_salix5)

lmBena_ll = lm(log(BAI) ~ log(Age), data = sd_bena5)
lmSalix_ll = lm(log(BAI) ~ log(Age), data = sd_salix5)

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

<<<<<<< HEAD
# 15. EXTRACT RESIDUALS FROM LINEAR MODEL ####
=======
# 15. CREATE LINEAR MIXED EFFECTS MODELS ####
    
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
    
    
# 16. EXTRACT RESIDUALS FROM LINEAR MODEL ####
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68

#Extract the residuals from the linear model: his is variation that can be expected to be explained by something else than age.

sd_bena_res = add_residuals(sd_bena5, lmBena_l)
  
  sd_bena_res$ShrubID_year <- do.call(paste, c(sd_bena_res[c("ShrubID", "Year")], sep = "_"))

sd_salix_res = add_residuals(sd_salix5, lmSalix_l)

  sd_salix_res$ShrubID_year <- do.call(paste, c(sd_salix_res[c("ShrubID", "Year")], sep = "_"))

sd_bena_res_ll = add_residuals(sd_bena5, lmBena_ll)

  sd_bena_res_ll$ShrubID_year <- do.call(paste, c(sd_bena_res_ll[c("ShrubID", "Year")], sep = "_"))
    
  colnames(sd_bena_res_ll)[colnames(sd_bena_res_ll)=="resid"] = "resid_ll"

<<<<<<< HEAD
  sd_bena_res_ll <- select(sd_bena_res_ll, ShrubID_year, resid_ll)
=======
  sd_bena_res_ll <- sd_bena_res_ll %>%
    select(ShrubID_year, resid_ll)
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68
  
sd_salix_res_ll = add_residuals(sd_salix5, lmSalix_ll)

  sd_salix_res_ll$ShrubID_year <- do.call(paste, c(sd_salix_res_ll[c("ShrubID", "Year")], sep = "_"))

  colnames(sd_salix_res_ll)[colnames(sd_salix_res_ll)=="resid"] = "resid_ll"

  sd_salix_res_ll <- sd_salix_res_ll %>%
    select(ShrubID_year, resid_ll)  

  
sd_bena_res <- left_join(sd_bena_res, sd_bena_res_ll, by = "ShrubID_year")

  sd_bena_res$ShrubID_year = NULL

sd_salix_res <- left_join(sd_salix_res, sd_salix_res_ll, by = "ShrubID_year")

  sd_salix_res$ShrubID_year = NULL
  
#Transform residuals back into BAI scale from the log scale

#sd_bena_res$resid_t = exp(1)^(sd_bena_res$resid)

#sd_salix_res$resid_t = exp(1)^(sd_salix_res$resid)

#Combine the two species back into one dataset

sd_all = rbind(sd_bena_res, sd_salix_res)

<<<<<<< HEAD
write.csv(sd_all, "/Users/peterfrank/Documents/Master's Thesis/DataAnalysis/AlaskaShrubs/R_Data/Shrub_BAI+RESID.csv")

### DATA FOR KATA WITH AGES >5 ####
lmBena_k = lm(log(BAI) ~ Age, data = sd_bena)
lmSalix_k = lm(log(BAI) ~ Age, data = sd_salix)

sd_bena_k = add_residuals(sd_bena, lmBena_k)

sd_bena_k$ShrubID_year <- do.call(paste, c(sd_bena_k[c("ShrubID", "Year")], sep = "_"))

sd_salix_k = add_residuals(sd_salix, lmSalix_k)

sd_salix_k$ShrubID_year <- do.call(paste, c(sd_salix_k[c("ShrubID", "Year")], sep = "_"))

sd_k = rbind(sd_bena_k, sd_salix_k)

sd_k$Shrub_ID_year <- do.call(paste, c(sd_k[c("ShrubID", "Year")], sep = "_"))

#Joins the climate and shrub chronology data together
sd_k_cc = join(sd_k, iem.climateAnnual, by='Shrub_ID_year', type='left', match='all')

sd_k_cc = join(sd_k_cc, climateAnnual, by='Shrub_ID_year', type='left', match='all')

#Create an standardized 
sd_k_cc$iem.summ.rain.10 = sd_k_cc$iem.summ.rain/10

#Joins the herbivore data to the combined shrub chronology and climate data
#CCH stands for chronology, climate and herbivory 

#Joins the Hare cycle data
sd_k_cch = join(sd_k_cc, HareCycle, by='Year', type='left', match='all')

#Adds a column GMU_year which can be used to join on the Moose density data
sd_k_cch$GMU_year <- do.call(paste, c(sd_k_cch[c("GMU", "Year")], sep = "_"))

#Joins the moose density data
sd_k_cch = join(sd_k_cch, MooseDensity, by='GMU_year', type='left', match='all')


sd_k_cch$Genus = ifelse(sd_k_cch$Species == "BENA", "Betula",
                          ifelse(sd_k_cch$Species == "SAPU", "Salix",
                                 ifelse(sd_k_cch$Species == "SAGL", "Salix",
                                        ifelse(sd_k_cch$Species == "SABE", "Salix",
                                               NA))))


write.csv(sd_k, "/Users/peterfrank/Desktop/AK_DaltonHwy_Shrubs.csv")
=======
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68


