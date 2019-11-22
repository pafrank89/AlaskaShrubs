# Master's Thesis Data Processing and Analysis
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-04-11

# Assign working drive
rm(list=ls())                       # removes all objects from the current workspace
getwd()                             # shows the current working drive
setwd("~/Desktop/Masters Thesis")   # sets a new working drive

# Install the required R packages 
install.packages("dplR")            # functions for performing standard tree-ring analyses
install.packages("reshape2")        # functions to transform data between wide and long formats
install.packages("dplyr")           # functions to merge and join datasets
install.packages("lme4")            # functions associated with linear mix effects models
install.packages("nlme") 


# Load the libraries of installed packages
library(dplR)
library(reshape2)
library(dplyr)
library(lme4)
library(nlme)

# Load data from CSV Files 
gr = read.csv(file = "~/Desktop/Masters Thesis/Data_Processing_Analysis/Test_Data/DaltonHwy_ShrubGrowthData.csv",
             header = TRUE,
             dec = ".")

hd = read.csv(file = "~/Desktop/Masters Thesis/Data_Processing_Analysis/Test_Data/DaltonHwy_MooseData.csv",
              header = TRUE,
              dec = ".")

temp = read.csv(file = "~/Desktop/Masters Thesis/Data_Processing_Analysis/Test_Data/DaltonHwy_TempData.csv",
                header = TRUE,
                dec = ".")

prec = read.csv(file = "~/Desktop/Masters Thesis/Data_Processing_Analysis/Test_Data/DaltonHwy_PrecipData.csv",
                header = TRUE,
                dec = ".")

#################################### CALCULATE BAI ##################################################################################

#Establishe the field names of which will be isolated from the larger growth dataset
IncrementFields = c("Shrub.ID","X2018","X2017","X2016","X2015","X2014","X2013","X2012","X2011","X2010","X2009","X2008","X2007","X2006","X2005","X2004","X2003","X2002","X2001","X2000","X1999","X1998","X1997","X1996","X1995","X1994","X1993","X1992","X1991","X1990","X1989","X1988","X1987","X1986","X1985","X1984","X1983","X1982","X1981","X1980","X1979","X1978","X1977","X1976","X1975","X1974","X1973","X1972","X1971","X1970","X1969","X1968","X1967","X1966","X1965","X1964","X1963","X1962","X1961","X1960","X1959","X1958","X1957","X1956","X1955")

#Create a new dataset with only the fields related to Increment Data
IncrementData <- gr[IncrementFields]

# Establish the Shrub.ID field as the row heading and then deletes the original Shrub.ID field
rownames(IncrementData) <- IncrementData[,1]
IncrementData <- IncrementData[,-1] 

# Calculate the BAI using the bai.in function from the dplR package
BAIdata<-bai.in(IncrementData)

# Record values for the ShrubIDs from the row headings, then attache them as a field
ShrubID <- rownames(BAIdata)
rownames(BAIdata) <- NULL
BAIdata <- cbind(ShrubID,BAIdata)
colnames(BAIdata)[colnames(BAIdata)=="ShrubID"] <- "Shrub.ID"

# Convert the BAIdata to long format using the melt function from the reshape2 package
BAIdata_melt<-melt(BAIdata, id="Shrub.ID")

# Change the field names of the melted data set to Year
colnames(BAIdata_melt)[colnames(BAIdata_melt)=="variable"] <- "Year"
colnames(BAIdata_melt)[colnames(BAIdata_melt)=="value"] <- "Growth"

# Remove the X in front of the Year values
BAIdata_melt$Year<-substr(BAIdata_melt$Year, 2, 5)

# Combine the melted data to the original growth dataset using the Shrub.ID field
gr<-cbind(BAIdata_melt, gr, by="Shrub.ID")
gr$Shrub.ID <- NULL  # Delete the extra Shrub.ID field added from the cbind opperation
gr$X2019<-NULL
gr$X2018<-NULL
gr$X2017<-NULL
gr$X2016<-NULL
gr$X2015<-NULL
gr$X2014<-NULL
gr$X2013<-NULL
gr$X2012<-NULL
gr$X2011<-NULL
gr$X2010<-NULL
gr$X2009<-NULL
gr$X2008<-NULL
gr$X2007<-NULL
gr$X2006<-NULL
gr$X2005<-NULL
gr$X2004<-NULL
gr$X2003<-NULL
gr$X2002<-NULL
gr$X2001<-NULL
gr$X2000<-NULL
gr$X1999<-NULL
gr$X1998<-NULL
gr$X1997<-NULL
gr$X1996<-NULL
gr$X1995<-NULL
gr$X1994<-NULL
gr$X1993<-NULL
gr$X1992<-NULL
gr$X1991<-NULL
gr$X1990<-NULL
gr$X1989<-NULL
gr$X1988<-NULL
gr$X1987<-NULL
gr$X1986<-NULL
gr$X1985<-NULL
gr$X1984<-NULL
gr$X1983<-NULL
gr$X1982<-NULL
gr$X1981<-NULL
gr$X1980<-NULL
gr$X1979<-NULL
gr$X1978<-NULL
gr$X1977<-NULL
gr$X1976<-NULL
gr$X1975<-NULL
gr$X1974<-NULL
gr$X1973<-NULL
gr$X1972<-NULL
gr$X1971<-NULL
gr$X1970<-NULL
gr$X1969<-NULL
gr$X1968<-NULL
gr$X1967<-NULL
gr$X1966<-NULL
gr$X1965<-NULL
gr$X1964<-NULL
gr$X1963<-NULL
gr$X1962<-NULL
gr$X1961<-NULL
gr$X1960<-NULL
gr$X1959<-NULL
gr$X1958<-NULL
gr$X1957<-NULL
gr$X1956<-NULL
gr$X1955<-NULL

gr$Section_Year = paste(gr$Section , "-", gr$Year)
gr$GMU_Year = paste(gr$GMU , "-", gr$Year)

#################################### PREPARE CLIMATE DATA #########################################################################
temp_melt<-melt(temp, id="Section")
prec_melt<-melt(prec, id="Section")

colnames(temp_melt)[colnames(temp_melt)=="variable"] <- "Year"
colnames(prec_melt)[colnames(prec_melt)=="variable"] <- "Year"
colnames(temp_melt)[colnames(temp_melt)=="value"] <- "MAT"
colnames(prec_melt)[colnames(prec_melt)=="value"] <- "MAP"

temp_melt$Year<-substr(temp_melt$Year, 2, 5)
prec_melt$Year<-substr(prec_melt$Year, 2, 5)

temp_melt$Section_Year = paste(temp_melt$Section, "-", temp_melt$Year)
prec_melt$Section_Year = paste(prec_melt$Section, "-", prec_melt$Year)

gr_temp = left_join(gr, temp_melt, by = c('Section_Year'), copy = TRUE)
gr_temp$Section.y <- NULL
gr_temp$Year.y <- NULL
gr_clim = left_join(gr_temp, prec_melt, by = c('Section_Year'), copy = TRUE)
gr_clim$Section <- NULL
gr_clim$Year <- NULL

#################################### PREPARE HERBIVORE DATA #########################################################################
herbivore_melt<-melt(hd, id="GMU")

colnames(herbivore_melt)[colnames(herbivore_melt)=="variable"] <- "Year"
colnames(herbivore_melt)[colnames(herbivore_melt)=="value"] <- "MooseDensity"

herbivore_melt$Year<-substr(herbivore_melt$Year, 2, 5)

herbivore_melt$GMU_Year = paste(herbivore_melt$GMU , "-", herbivore_melt$Year)

gr_final = left_join(gr_clim, herbivore_melt, by = c('GMU_Year'), copy = TRUE)

unique(herbivore_melt$GMU_Year)

#################################### DATA ANALYSIS #########################################################################

plot(gr_final$Year.x, gr_final$Growth)

plot(gr_final$MAT , gr_final$Growth)
plot(gr_final$MAP , gr_final$Growth)

plot(gr_final$Shrub.Height..cm., gr_final$Growth)

plot(gr_final$Moose.Browsing.Intensity..0.100.. , gr_final$Growth)

plot(gr_final$Shurb.Age, gr_final$Growth)

plot(gr_final$Elevation , gr_final$Growth)
plot(gr_final$Aspect , gr_final$Growth)
plot(gr_final$Growth, gr_final$Y_Cord)


##think of NDVI for productivity measure
##Connect year to individual to give an increasing age value through time
## Get rid of the NA records in the growth field

model_BIA = lme(Growth ~ Moose.Browsing.Intensity..0.100.. + MAT, #+ Moose.Browsing.Intensity..0.100.. * MAT +
            #Slope + Aspect + Shrub.Height..cm. ,    #Covariates
            ###random =~ 1 | Section.x/Shrub.ID,           #Random
            data = gr_final,
            na.action = na.omit)


model_BIA

gr_final$Moose.Browsing.Intensity..0.100.. = as.numeric(as.character(gr_final$Moose.Browsing.Intensity..0.100..))
str(gr_final)
