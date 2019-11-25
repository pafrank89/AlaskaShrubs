# Shrub Basal Area Increment (BAI) Calculation
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-11-21

install.packages("dplR")
install.packages("reshape2")
install.packages('plyr')

library(dplR)
library(dplyr)
library(reshape2)
library(plyr)

#BAI Calculation using measured ring widths (mm) from Image J.
#Ring widths are listed from the bark to the pith, so the function bai.out in the dplR package will be used.

# 1. CALCULATE THE MEAN OF THE 4 MEASURED RADII ####

#Calculates mean ring width per year from the  4 measured radii
MeanRadii = aggregate(x = BaseChronology,
                      by = list(BaseChronology$ShrubID),
                      FUN = mean,
                      na.rm = TRUE)

#Removes unused colums from the aggregated dataset 
MeanRadii = select (MeanRadii, -c (2))

#Changes the column name output from the aggregate function to ShrubID

colnames(MeanRadii)[colnames(MeanRadii)=="Group.1"] <- "ShrubID"

# 2. TRANSPOSE MEAN RING WIDTH DATA ####

#Transposes the data into the format reqired for the bai.out function
mean_rwl_all = as.data.frame (t(MeanRadii))
  
  #Changes the column names of the newly transposed data frame and deletes the unnecesary first row
  #As well as the 2019 row which cannot be used in the analysis
    ShrubID = MeanRadii$ShrubID

    colnames(mean_rwl_all) = ShrubID

    mean_rwl_all = mean_rwl_all[-c(1, 2), ]


      # 2.1. SUBSET RW DATA BY SPECIES ####
    
#Creates a new data frames from the full dataset based on species

mean_rwl_BENA = data.frame(mean_rwl_all %>% dplyr:: select (grep("BENA", names(mean_rwl_all))), check.names = FALSE)    

mean_rwl_SALIX = data.frame(mean_rwl_all %>% dplyr:: select (grep("SA", names(mean_rwl_all))), check.names = FALSE)

    #mean_rwl_SAPU = data.frame(mean_rwl_all %>% dplyr:: select (grep("SAPU", names(mean_rwl_all))), check.names = FALSE)
    
    #mean_rwl_SABE = data.frame(mean_rwl_all %>% dplyr:: select (grep("SABE", names(mean_rwl_all))), check.names = FALSE)
    
    #mean_rwl_SAGL = data.frame(mean_rwl_all %>% dplyr:: select (grep("SAGL", names(mean_rwl_all))), check.names = FALSE)

      # 2.2. SUBSET RW DATA BY SITE ####
                
#Creates a new data frames from the full dataset based on the site number

mean_rwl_60 = data.frame(mean_rwl_all %>% dplyr:: select (grep("60", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_72 = data.frame(mean_rwl_all %>% dplyr:: select (grep("72", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_86 = data.frame(mean_rwl_all %>% dplyr:: select (grep("86", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_97 = data.frame(mean_rwl_all %>% dplyr:: select (grep("97", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_102 = data.frame(mean_rwl_all %>% dplyr:: select (grep("102", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_111 = data.frame(mean_rwl_all %>% dplyr:: select (grep("111", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_121 = data.frame(mean_rwl_all %>% dplyr:: select (grep("121", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_131 = data.frame(mean_rwl_all %>% dplyr:: select (grep("131", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_147 = data.frame(mean_rwl_all %>% dplyr:: select (grep("147", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_151 = data.frame(mean_rwl_all %>% dplyr:: select (grep("151", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_158 = data.frame(mean_rwl_all %>% dplyr:: select (grep("158", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_173 = data.frame(mean_rwl_all %>% dplyr:: select (grep("173", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_184 = data.frame(mean_rwl_all %>% dplyr:: select (grep("184", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_191 = data.frame(mean_rwl_all %>% dplyr:: select (grep("191", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_203 = data.frame(mean_rwl_all %>% dplyr:: select (grep("203", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_209 = data.frame(mean_rwl_all %>% dplyr:: select (grep("209", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_222 = data.frame(mean_rwl_all %>% dplyr:: select (grep("222", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_235 = data.frame(mean_rwl_all %>% dplyr:: select (grep("235", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_240 = data.frame(mean_rwl_all %>% dplyr:: select (grep("240", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_255 = data.frame(mean_rwl_all %>% dplyr:: select (grep("255", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_261 = data.frame(mean_rwl_all %>% dplyr:: select (grep("261", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_268 = data.frame(mean_rwl_all %>% dplyr:: select (grep("268", names(mean_rwl_all))), check.names = FALSE)

mean_rwl_282 = data.frame(mean_rwl_all %>% dplyr:: select (grep("282", names(mean_rwl_all))), check.names = FALSE)

      # 2.3. SUBSET RW DATA BY SITE & SPECIES ####

#Creates a new data frames from the BENA dataset based on the site number 
mean_rwl_60_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("60", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_72_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("72", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_86_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("86", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_97_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("97", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_102_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("102", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_111_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("111", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_121_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("121", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_131_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("131", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_147_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("147", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_151_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("151", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_158_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("158", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_173_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("173", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_184_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("184", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_191_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("191", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_203_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("203", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_209_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("209", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_222_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("222", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_235_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("235", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_240_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("240", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_255_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("255", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_261_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("261", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_268_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("268", names(mean_rwl_BENA))), check.names = FALSE)

mean_rwl_282_BENA = data.frame(mean_rwl_BENA %>% dplyr:: select (grep("282", names(mean_rwl_BENA))), check.names = FALSE)

#Creates a new data frames from the SALIX dataset based on the site number 
mean_rwl_60_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("60", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_72_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("72", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_86_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("86", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_97_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("97", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_102_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("102", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_111_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("111", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_121_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("121", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_131_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("131", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_147_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("147", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_151_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("151", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_158_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("158", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_173_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("173", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_184_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("184", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_191_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("191", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_203_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("203", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_209_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("209", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_222_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("222", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_235_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("235", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_240_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("240", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_255_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("255", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_261_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("261", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_268_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("268", names(mean_rwl_SALIX))), check.names = FALSE)

mean_rwl_282_SALIX = data.frame(mean_rwl_SALIX %>% dplyr:: select (grep("282", names(mean_rwl_SALIX))), check.names = FALSE)

# 3. CREATE RWL FILES ####

#Remove rows that only have NA values 
mean_rwl_147_BENA = mean_rwl_147_BENA[rowSums(is.na(mean_rwl_147_BENA)) != ncol(mean_rwl_147_BENA), ]

#Create a temporary csv file from the mean ring width legnth data,
#Then uses the csv2rwl function to convert to a rwl file usable in dplR
tmpName <- tempfile()
    
write.csv( mean_rwl_147_BENA,file = tmpName)
    
mean_rwl_147_BENA = csv2rwl(tmpName)


# 4. CREATE DIAM FILES ####

#Calculates mean legnth of each of the 4 radii
MeanDiam = aggregate(x = BaseDiameter,
                      by = list(BaseDiameter$ShrubID),
                      FUN = mean,
                      na.rm = TRUE)

#Removes unused colums from the aggregated dataset 
MeanDiam = select (MeanDiam, -c (2))

#Changes the column name output from the aggregate function to ShrubID
colnames(MeanDiam)[colnames(MeanDiam)=="Group.1"] <- "ShrubID"

#Multiplies the averaged radii values by 2 to get the diameter of the stem - the bark
MeanDiam$diam = MeanDiam$Radius * 2

#Deletes the old radius column
MeanDiam = select (MeanDiam, -c (2))

#For the bai calculation we need a dataframe with two colums:
# 1 ShrubID which exactly matchs the ShrubID from the RWL file
# 2 Diameter measurments in a column labeled "diam"

#To get the exact column labels from the RWL file we will transpose and extract them
rwlNames = as.data.frame (t(mean_rwl_all))

rwlNames = row.names(rwlNames)

MeanDiam = cbind(rwlNames, MeanDiam)

MeanDiam = select (MeanDiam, -c (0,2))


# 5. CALCULATE BAI ####

#Use the bai.out tool from dplR to calculate the basal area increment going from 
#the bark to the pith. 

bai_all <- bai.out(rwl = mean_rwl_all, diam = MeanDiam)

      
# 6. TRANSPOSE BAI DATA ####

#Transposes the data into a format which can be melted and joined back to the shrub data
bai_all_t = data.frame (t(bai_all))

# 7. ADD SHRUB-ID AND RENAME COLUMNS  ####

#Sorts the shrub data in ascending order, not numerical (with the whole # e.g. 60 & up), but sequential (starting with the first digit e.g. 102 & up)
SortedShrubData = AKShrub_SubSampleData %>% arrange(ShrubID)

#Creates a vector list with all the sorted shrub ID values
ShrubID = as.vector(SortedShrubData['ShrubID'])

#Adds the Shrub ID values to the transposed BAI data set 
bai_all_f = cbind(ShrubID, bai_all_t)

#Removes the row.name and then renames all the columns   
bai_all_f = setNames(cbind(rownames(bai_all_f), bai_all_f, row.names = NULL), 
            c("Shrub.ID", "ShrubID", "2018","2017","2016","2015","2014","2013","2012","2011","2010","2009","2008","2007","2006","2005","2004","2003","2002","2001","2000","1999","1998","1997","1996","1995","1994","1993","1992","1991","1990","1989","1988","1987","1986","1985","1984","1983","1982","1981","1980","1979","1978","1977","1976","1975","1974","1973","1972","1971","1970","1969","1968","1967","1966","1965","1964","1963","1962","1961","1960","1959","1958","1957","1956","1955","1954","1953","1952","1951","1950"))

#Removes the Shrub ID column which was added during the BAI calculation
bai_all_f$Shrub.ID = NULL

# 8. MELT BIA DATA AND RW DATA  ####

#Melts the BAI data, ignoring all NA values
bai_all_melt = melt(bai_all_f, id="ShrubID", na.rm = TRUE)

#Changes the field names of the melted data set to Year and BAI
colnames(bai_all_melt)[colnames(bai_all_melt)=="variable"] <- "Year"
colnames(bai_all_melt)[colnames(bai_all_melt)=="value"] <- "BAI"

#Removes 2019 from the ring width data prior to melting
MeanRadii$`2019` = NULL

#Melts the ring width data, ignoring all NA values
MeanRadii_melt = melt(MeanRadii, id = "ShrubID", na.rm = TRUE)

#Changes the field names of the melted data set to Year and BAI
colnames(MeanRadii_melt)[colnames(MeanRadii_melt)=="variable"] <- "Year"
colnames(MeanRadii_melt)[colnames(MeanRadii_melt)=="value"] <- "RingWidth"


# 9. JOIN SHRUB DATA TO THE MELTED BAI AND RW DATA  ####

#Adds the melted ring width data to the melted bai data
bai_rwl_all = cbind(MeanRadii_melt$RingWidth, bai_all_melt)

#Reorders the columns after the ring width values are added
bai_rwl_all = bai_rwl_all[,c(2,3,1,4)]

#Renames the added column 
colnames(bai_rwl_all)[colnames(bai_rwl_all)=="MeanRadii_melt$RingWidth"] <- "RingWidth"

#Joins the ring width and bai data to the shrub data using the ShrubID as the key 
bai_rwl_age = join(bai_rwl_all, Shrub_Age, by='ShrubID', type='left', match='all')

SubSample_join = join(bai_rwl_age, AKShrub_SubSampleData, by='ShrubID', type='left', match='all')

str(SubSample_join)

SubSample_join$Year = as.numeric(as.character(SubSample_join$Year))

write.csv(SubSample_join, "/Users/peterfrank/Desktop/Master's Thesis/DataAnalysis/AlaskaShrubs/R_Data/Shrub_BAI.csv")

