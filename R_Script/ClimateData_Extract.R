# Extract Climate Data from CRU-TS and SNAP Datasets
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-11-21

# Construct a climate data file that can be merged to growth and browsing data

# Data sources
# http://data.snap.uaf.edu/data/Base/AK_CAN_2km/historical/CRU_TS/
# https://catalogue.ceda.ac.uk/uuid/10d3e3640f004c578403419aac167d82
# https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.03/
 

#Packages
install.packages("sp")
install.packages("raster")
install.packages("rgdal")
install.packages("ncdf4")
install.packages("reshape2")
install.packages("RColorBrewer")
install.packages("latticeExtra")
install.packages("rasterVis")
install.packages("purrr")

library(sp)
library(raster)
library(rgdal)
library(ncdf4)
library(reshape2)
library(RColorBrewer)
library(latticeExtra)
library(rasterVis)
library(purrr)


# EXTRACT SNAP & IEM DATA ####

#Sets the ShrubID column as the row names
row.names(coordinates_NAD83) = coordinates_NAD83$ShrubID

#Deletes the extra ShrubID column
coordinates_NAD83[1] = NULL

# 1. Temperature -------------------------------------------------------------

# Finds all the files with extension "tif" in the RasterStack_TIFF Folder
snap.tmp.files <- list.files(path= "/Volumes/PF_HD/MastersThesis/ClimateData/SNAP/tas_AK_CAN_2km_CRU_TS40_historical",
                         pattern='tif', full.names=TRUE )

iem.tmp.files <- list.files(path= "/Volumes/PF_HD/MastersThesis/ClimateData/SNAP/tas_mean_C_iem_cru_TS40_1901_2015",
                             pattern='tif', full.names=TRUE )


# Create a raster stack of all tiff files 
snap.tmp = stack(snap.tmp.files) 

iem.tmp = stack(iem.tmp.files) 

# plot an example day:
plot(snap.tmp$tas_mean_C_CRU_TS40_historical_01_1901)

plot(iem.tmp$tas_mean_C_CRU_TS40_historical_01_1901)

# check that you sample points match:
points(coordinates_NAD83, pch=16, col = "Blue")

# we can also play with smaller areas:
ak.area <- extent(-50000, 400000, 1600000, 2200000) 
ak.snap.tmp <- crop(snap.tmp, ak.area)
plot(ak.snap.tmp$tas_mean_C_CRU_TS40_historical_01_1901)
points(coordinates_NAD83, pch=16, col = "Blue")

ak.iem.tmp <- crop(iem.tmp, ak.area)
plot(ak.iem.tmp$tas_mean_C_CRU_TS40_historical_01_1901)
points(coordinates_NAD83, pch=16, col = "Blue")

# Extract climate data from the RasterBrick as a data.frame
snap.tmp.shrubs <- data.frame(extract(snap.tmp, coordinates_NAD83, ncol=2))

iem.tmp.shrubs <- data.frame(extract(iem.tmp, coordinates_NAD83, ncol=2))

# Add shrub id names
row.names(snap.tmp.shrubs) <- row.names(coordinates_NAD83)

row.names(iem.tmp.shrubs) <- row.names(coordinates_NAD83)

# Lets also change the column names to be a bit easier to work with:
# name them using the convention: year followed by month: 
# 1. create two vector objects containing the years and months. 
years <- 1901:2015
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

#Create a combined vector from month and year 
#The SNAP data is structured by month, so it starts with all the January values between 1901 and 2015
#ex. 1901-Jan, 1902-Jan, 1903-Jan, ect.
eg = expand.grid(years,month)
monyr = paste(eg$Var2, eg$Var1, sep = '_')

# 2. rename the columns of our data.frame
names(snap.tmp.shrubs) <- paste(monyr)
names(iem.tmp.shrubs) <- paste(monyr)

#View(snap.tmp.shrubs)

# Modify data to long format wth columns for shrb, year, month and temp for snap data
snap.tmp.shrubs$Shrub_ID<-rownames(snap.tmp.shrubs)

rownames(snap.tmp.shrubs)<-NULL
snap.tmp.shrubsM<-melt(snap.tmp.shrubs)
colnames(snap.tmp.shrubsM)[colnames(snap.tmp.shrubsM)=="variable"] <- "year_month"
colnames(snap.tmp.shrubsM)[colnames(snap.tmp.shrubsM)=="value"] <- "tmp"
snap.tmp.shrubsM$year<-substr(snap.tmp.shrubsM$year_month, 5, 9)
snap.tmp.shrubsM$month <- substr(snap.tmp.shrubsM$year_month, 0, 3)

# Modify data to long format wth columns for shrb, year, month and temp for iem data
iem.tmp.shrubs$Shrub_ID<-rownames(iem.tmp.shrubs)

rownames(iem.tmp.shrubs)<-NULL
iem.tmp.shrubsM<-melt(iem.tmp.shrubs)
colnames(iem.tmp.shrubsM)[colnames(iem.tmp.shrubsM)=="variable"] <- "year_month"
colnames(iem.tmp.shrubsM)[colnames(iem.tmp.shrubsM)=="value"] <- "tmp"
iem.tmp.shrubsM$year<-substr(iem.tmp.shrubsM$year_month, 5, 9)
iem.tmp.shrubsM$month <- substr(iem.tmp.shrubsM$year_month, 0, 3)



# 2. Precipitation -------------------------------------------------------------

# Finds all the files with extension "tif" in the RasterStack_TIFF Folder
snap.pre.files = list.files(path= "/Volumes/PF_HD/MastersThesis/ClimateData/SNAP/pr_AK_CAN_2km_CRU_TS40_historical",
                          pattern='tif', full.names=TRUE )

iem.pre.files = list.files(path= "/Volumes/PF_HD/MastersThesis/ClimateData/SNAP/pr_total_mm_iem_cru_TS40_1901_2015",
                            pattern='tif', full.names=TRUE )

# Create a raster stack of all tiff files 
snap.pre = stack(snap.pre.files)

iem.pre = stack(iem.pre.files)

# plot an example day:
plot(snap.pre$pr_total_mm_CRU_TS40_historical_01_1901)

plot(iem.pre$pr_total_mm_CRU_TS40_historical_01_1901)

# check that you sample points match:
points(coordinates_NAD83, pch=16)

# Extract climate data from the RasterBrick as a data.frame
snap.pre.shrubs <- data.frame(extract(snap.pre, coordinates_NAD83, ncol=2))

iem.pre.shrubs <- data.frame(extract(iem.pre, coordinates_NAD83, ncol=2))

# Add shrub id names
row.names(snap.pre.shrubs) <- row.names(coordinates_NAD83)

row.names(iem.pre.shrubs) <- row.names(coordinates_NAD83)

# Lets also change the column names to be a bit easier to work with:
# name them using the convention: year followed by month: 
# 1. create two vector objects containing the years and months. 
years <- 1901:2015
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

#Create a combined vector from month and year 
#The SNAP data is structured by month, so it starts with all the January values between 1901 and 2015
#ex. 1901-Jan, 1902-Jan, 1903-Jan, ect.
eg = expand.grid(years,month)
monyr = paste(eg$Var2, eg$Var1, sep = '-')

# 2. rename the columns of our data.frame

names(snap.pre.shrubs) <- paste(monyr)

names(iem.pre.shrubs) <- paste(monyr)

#View(snap.pre.shrubs)

# Modify data to long format wth columns for shrb, year, month and precip for snap data
snap.pre.shrubs$Shrub_ID<-rownames(snap.pre.shrubs)
rownames(snap.pre.shrubs)<-NULL
snap.pre.shrubsM<-melt(snap.pre.shrubs)
colnames(snap.pre.shrubsM)[colnames(snap.pre.shrubsM)=="variable"] <- "year_month"
colnames(snap.pre.shrubsM)[colnames(snap.pre.shrubsM)=="value"] <- "pre"
snap.pre.shrubsM$year<-substr(snap.pre.shrubsM$year_month, 5, 9)
snap.pre.shrubsM$month <- substr(snap.pre.shrubsM$year_month, 0, 3)

# Modify data to long format wth columns for shrb, year, month and precip for iem data
iem.pre.shrubs$Shrub_ID<-rownames(iem.pre.shrubs)
rownames(iem.pre.shrubs)<-NULL
iem.pre.shrubsM<-melt(iem.pre.shrubs)
colnames(iem.pre.shrubsM)[colnames(iem.pre.shrubsM)=="variable"] <- "year_month"
colnames(iem.pre.shrubsM)[colnames(iem.pre.shrubsM)=="value"] <- "pre"
iem.pre.shrubsM$year<-substr(iem.pre.shrubsM$year_month, 5, 9)
iem.pre.shrubsM$month <- substr(iem.pre.shrubsM$year_month, 0, 3)



# 3. Merge SNAP & IEM climate data --------------------------------------------------

snap.climate<-merge(snap.tmp.shrubsM, snap.pre.shrubsM)

str(snap.climate)

iem.climate<-merge(iem.tmp.shrubsM, iem.pre.shrubsM)

str(iem.climate)


# 4. Calculate yearly climate variables ----------------------------------------------

# create shrub_ID_year varable
snap.climate$Shrub_ID_year <- do.call(paste, c(snap.climate[c("Shrub_ID", "year")], sep = "_"))

iem.climate$Shrub_ID_year <- do.call(paste, c(snap.climate[c("Shrub_ID", "year")], sep = "_"))

# Calculate the following variables

      # 1. Mean summer temperature June-August (pity that don't know growth period lenght...)
      # 2. Yearly mean temperature
      # 3. Mean min summer temperature June-August
      # 4. Mean max summer temperature June-August
      # 5. Precipitation sum June-August
      # 6. Precipitation sum October-March (for snow)
      # 7. Yearly frost day sums
      # 8. Mean PET June-August
      # 9. Yearly wet day sums

# extract right months
snap.climatesummer<-snap.climate[snap.climate$month=="Jun"|snap.climate$month=="Jul"|snap.climate$month=="Aug",]
snap.climatewinter<-snap.climate[snap.climate$month=="Oct"|snap.climate$month=="Nov"|snap.climate$month=="Dec"|snap.climate$month=="Jan"|snap.climate$month=="Feb"|snap.climate$month=="Mar",]

iem.climatesummer<-iem.climate[iem.climate$month=="Jun"|iem.climate$month=="Jul"|iem.climate$month=="Aug",]
iem.climatewinter<-iem.climate[iem.climate$month=="Oct"|iem.climate$month=="Nov"|iem.climate$month=="Dec"|iem.climate$month=="Jan"|iem.climate$month=="Feb"|iem.climate$month=="Mar",]

#Calculate the annual values
snap.summ.temp<-tapply(snap.climatesummer$tmp,list(snap.climatesummer$Shrub_ID_year),mean)
snap.temp<-tapply(snap.climate$tmp,list(snap.climate$Shrub_ID_year),mean)
snap.summ.rain<-tapply(snap.climatesummer$pre,list(snap.climatesummer$Shrub_ID_year),sum)
snap.wint.rain<-tapply(snap.climatewinter$pre,list(snap.climatewinter$Shrub_ID_year),sum)


iem.summ.temp<-tapply(iem.climatesummer$tmp,list(iem.climatesummer$Shrub_ID_year),mean)
iem.temp<-tapply(iem.climate$tmp,list(iem.climate$Shrub_ID_year),mean)
iem.summ.rain<-tapply(iem.climatesummer$pre,list(iem.climatesummer$Shrub_ID_year),sum)
iem.wint.rain<-tapply(iem.climatewinter$pre,list(iem.climatewinter$Shrub_ID_year),sum)

#Change the row names to Shrub_ID_Year for the newly created yearly climate data 
snap.summ.temp<-data.frame(snap.summ.temp)
Shrub_ID_year <- rownames(snap.summ.temp)
rownames(snap.summ.temp) <- NULL
snap.summ.temp <- cbind(Shrub_ID_year,snap.summ.temp)

snap.temp<-data.frame(snap.temp)
Shrub_ID_year <- rownames(snap.temp)
rownames(snap.temp) <- NULL
snap.temp <- cbind(Shrub_ID_year,snap.temp)

snap.summ.rain<-data.frame(snap.summ.rain)
Shrub_ID_year <- rownames(snap.summ.rain)
rownames(snap.summ.rain) <- NULL
snap.summ.rain <- cbind(Shrub_ID_year,snap.summ.rain)

snap.wint.rain<-data.frame(snap.wint.rain)
Shrub_ID_year <- rownames(snap.wint.rain)
rownames(snap.wint.rain) <- NULL
snap.wint.rain <- cbind(Shrub_ID_year,snap.wint.rain)

#Merge the four yearly climate variables together
snap.climateAnnual<-merge(snap.summ.temp, snap.temp)
snap.climateAnnual<-merge(snap.climateAnnual, snap.summ.rain)
snap.climateAnnual<-merge(snap.climateAnnual, snap.wint.rain)

#Change the row names to Shrub_ID_Year for the newly created yearly climate data 
iem.summ.temp<-data.frame(iem.summ.temp)
Shrub_ID_year <- rownames(iem.summ.temp)
rownames(iem.summ.temp) <- NULL
iem.summ.temp <- cbind(Shrub_ID_year,iem.summ.temp)

iem.temp<-data.frame(iem.temp)
Shrub_ID_year <- rownames(iem.temp)
rownames(iem.temp) <- NULL
iem.temp <- cbind(Shrub_ID_year,iem.temp)

iem.summ.rain<-data.frame(iem.summ.rain)
Shrub_ID_year <- rownames(iem.summ.rain)
rownames(iem.summ.rain) <- NULL
iem.summ.rain <- cbind(Shrub_ID_year,iem.summ.rain)

iem.wint.rain<-data.frame(iem.wint.rain)
Shrub_ID_year <- rownames(iem.wint.rain)
rownames(iem.wint.rain) <- NULL
iem.wint.rain <- cbind(Shrub_ID_year,iem.wint.rain)

#Merge the four yearly climate variables together
iem.climateAnnual<-merge(iem.summ.temp, iem.temp)
iem.climateAnnual<-merge(iem.climateAnnual, iem.summ.rain)
iem.climateAnnual<-merge(iem.climateAnnual, iem.wint.rain)


write.csv(snap.climateAnnual, "/Users/peterfrank/Desktop/Master's Thesis/DataAnalysis/AlaskaShrubs/R_Data/SNAP_ClimateAnnual.csv")

write.csv(iem.climateAnnual, "/Users/peterfrank/Desktop/Master's Thesis/DataAnalysis/AlaskaShrubs/R_Data/IEM_ClimateAnnual.csv")


# EXTRACT CLIMATE RESEARCH UNIT TIME SERIES (CRU-TS) DATA ####

#Sets the ShrubID column as the row names
row.names(coordinates) = coordinates$ShrubID

#Deletes the extra ShrubID column
coordinates[1] = NULL

# 1. Temperature -------------------------------------------------------------

# this is big data, read from separate folder:
setwd("/Volumes/PF_HD/MastersThesis/ClimateData")
getwd()

nc.temp <- nc_open("cru_ts4.03.1901.2018.tmp.dat.nc")
print(nc.temp)
# 2 variables: float tmp which is temperature & stn which is stations.
# 3 dimensions: lat, lon and time

# We only want to work on tmp data:
tmp <- brick("/Volumes/PF_HD/MastersThesis/ClimateData/cru_ts4.03.1901.2018.tmp.dat.nc", varname="tmp")
tmp

# Check the coordinate system of the cru-ts data
crs(tmp)

# plot an example day:
par(mfrow=c(1,1))
plot(tmp$X1901.01.16)

# we can also play with smaller areas:
ak.area <- extent(-155, -145, 65, 70) 
ak.tmp <- crop(tmp, ak.area)
plot(ak.tmp$X1901.01.16)

# check that you sample points match:
plot(ak.tmp$X1901.01.16)
points(coordinates, pch=16)

# Extract climate data from the RasterBrick as a data.frame
tmp.shrubs <- data.frame(extract(tmp, coordinates, ncol=2))

# Add shrub id names
row.names(tmp.shrubs) <- row.names(coordinates)

# Lets also change the column names to be a bit easier to work with:
# name them using the convention: year followed by month: 
# 1. create two vector objects containing the years and months. 
years <- 1901:2018
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# 2. rename the columns of our data.frame
names(tmp.shrubs) <- paste(rep(years, each=12), rep(month, times=116), sep="_")

View(tmp.shrubs)

# Modify data to long format wth columns for shrb, year, month and temp
tmp.shrubs$Shrub_ID<-rownames(tmp.shrubs)
rownames(tmp.shrubs)<-NULL
tmp.shrubsM<-melt(tmp.shrubs)
colnames(tmp.shrubsM)[colnames(tmp.shrubsM)=="variable"] <- "year_month"
colnames(tmp.shrubsM)[colnames(tmp.shrubsM)=="value"] <- "tmp"
tmp.shrubsM$year<-substr(tmp.shrubsM$year_month, 0, 4)
tmp.shrubsM$month <- substr(tmp.shrubsM$year_month, 6, 9)


# 2.1 Max Temperature -------------------------------------------------------------


nc.tempmax <- nc_open("cru_ts4.03.1901.2018.tmx.dat.nc")
print(nc.tempmax)
# 2 variables: float tmx which is tempmax & stn which is stations.
# 3 dimensions: lat, lon and time

# We only want to work on tmx data:
tmx <- brick("/Volumes/PF_HD/MastersThesis/ClimateData/cru_ts4.03.1901.2018.tmx.dat.nc", varname="tmx")
tmx

# plot an example day:
plot(tmx$X1901.01.16)

# we can also play with smaller areas:
ak.area <- extent(-155, -145, 65, 70) 
ak.tmx <- crop(tmx, ak.area)
plot(ak.tmx$X1901.01.16)

# check that you sample points match:
plot(tmx$X1901.01.16)
points(coordinates, pch=16)

# Extract climate data from the RasterBrick as a data.frame
tmx.shrubs <- data.frame(extract(tmx, coordinates, ncol=2))

# Add shrub id names
row.names(tmx.shrubs) <- row.names(coordinates)

# Lets also change the column names to be a bit easier to work with:
# name them using the convention: year followed by month: 
# 1. create two vector objects containing the years and months. 
years <- 1901:2018
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# 2. rename the columns of our data.frame
names(tmx.shrubs) <- paste(rep(years, each=12), rep(month, times=116), sep="_")
# View(tmx.shrubs)

# Modify data to long format wth columns for shrub, year, month and tempmax
tmx.shrubs$Shrub_ID<-rownames(tmx.shrubs)
rownames(tmx.shrubs)<-NULL
tmx.shrubsM<-melt(tmx.shrubs)
colnames(tmx.shrubsM)[colnames(tmx.shrubsM)=="variable"] <- "year_month"
colnames(tmx.shrubsM)[colnames(tmx.shrubsM)=="value"] <- "tmx"
tmx.shrubsM$year<-substr(tmx.shrubsM$year_month, 0, 4)
tmx.shrubsM$month <- substr(tmx.shrubsM$year_month, 6, 9)


# 2.2 Min Temperature -------------------------------------------------------------


nc.tempmin <- nc_open("cru_ts4.03.1901.2018.tmn.dat.nc")
print(nc.tempmin)
# 2 variables: float tmn which is tempmin & stn which is stations.
# 3 dimensions: lat, lon and time

# We only want to work on tmn data:
tmn <- brick("/Volumes/PF_HD/MastersThesis/ClimateData/cru_ts4.03.1901.2018.tmn.dat.nc", varname="tmn")
tmn

# plot an example day:
plot(tmn$X1901.01.16)

# we can also play with smaller areas:
ak.area <- extent(-155, -145, 65, 70) 
ak.tmn <- crop(tmn, ak.area)
plot(ak.tmn$X1901.01.16)

# check that you sample points match:
plot(tmn$X1901.01.16)
points(coordinates, pch=16)

# Extract climate data from the RasterBrick as a data.frame
tmn.shrubs <- data.frame(extract(tmn, coordinates, ncol=2))

# Add shrub id names
row.names(tmn.shrubs) <- row.names(coordinates)

# Lets also change the column names to be a bit easier to work with:
# name them using the convention: year followed by month: 
# 1. create two vector objects containing the years and months. 
years <- 1901:2018
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# 2. rename the columns of our data.frame
names(tmn.shrubs) <- paste(rep(years, each=12), rep(month, times=116), sep="_")
# View(tmn.shrubs)

# Modify data to long format wth columns for shrub, year, month and tempmin
tmn.shrubs$Shrub_ID<-rownames(tmn.shrubs)
rownames(tmn.shrubs)<-NULL
tmn.shrubsM<-melt(tmn.shrubs)
colnames(tmn.shrubsM)[colnames(tmn.shrubsM)=="variable"] <- "year_month"
colnames(tmn.shrubsM)[colnames(tmn.shrubsM)=="value"] <- "tmn"
tmn.shrubsM$year<-substr(tmn.shrubsM$year_month, 0, 4)
tmn.shrubsM$month <- substr(tmn.shrubsM$year_month, 6, 9)


# 3. Precipitation -------------------------------------------------------------


nc.rain <- nc_open("cru_ts4.03.1901.2018.pre.dat.nc")
print(nc.rain)
# 2 variables: float pre which is rain & stn which is stations.
# 3 dimensions: lat, lon and time

# We only want to work on pre data:
pre <- brick("/Volumes/PF_HD/MastersThesis/ClimateData/cru_ts4.03.1901.2018.pre.dat.nc", varname="pre")
pre

# plot an example day:
plot(pre$X1901.01.16)

# we can also play with smaller areas:
ak.area <- extent(-155, -145, 65, 70) 
ak.pre <- crop(pre, ak.area)
plot(ak.pre$X1901.01.16)

# check that you sample points match:
plot(pre$X1901.01.16)
points(coordinates, pch=16)

# Extract climate data from the RasterBrick as a data.frame
pre.shrubs <- data.frame(extract(pre, coordinates, ncol=2))

# Add shrub id names
row.names(pre.shrubs) <- row.names(coordinates)

# Lets also change the column names to be a bit easier to work with:
# name them using the convention: year followed by month: 
# 1. create two vector objects containing the years and months. 
years <- 1901:2018
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# 2. rename the columns of our data.frame
names(pre.shrubs) <- paste(rep(years, each=12), rep(month, times=116), sep="_")

# View(pre.shrubs)

# Modify data to long format wth columns for shrub, year, month and rain
pre.shrubs$Shrub_ID<-rownames(pre.shrubs)
rownames(pre.shrubs)<-NULL
pre.shrubsM<-melt(pre.shrubs)
colnames(pre.shrubsM)[colnames(pre.shrubsM)=="variable"] <- "year_month"
colnames(pre.shrubsM)[colnames(pre.shrubsM)=="value"] <- "pre"
pre.shrubsM$year<-substr(pre.shrubsM$year_month, 0, 4)
pre.shrubsM$month <- substr(pre.shrubsM$year_month, 6, 9)


# 4. Frost -------------------------------------------------------------

nc.frost <- nc_open("cru_ts4.03.1901.2018.frs.dat.nc")
print(nc.frost)
# 2 variables: float frs which is frost & stn which is stations.
# 3 dimensions: lat, lon and time

# We only want to work on frs data:
frs <- brick("/Volumes/PF_HD/MastersThesis/ClimateData/cru_ts4.03.1901.2018.frs.dat.nc", varname="frs")
frs

# plot an example day:
plot(frs$X1901.01.16)

# we can also play with smaller areas:
ak.area <- extent(-155, -145, 65, 70) 
ak.frs <- crop(frs, ak.area)
plot(ak.frs$X1901.01.16)

# check that you sample points match:
plot(frs$X1901.01.16)
points(coordinates, pch=16)

# Extract climate data from the RasterBrick as a data.frame
frs.shrubs <- data.frame(extract(frs, coordinates, ncol=2))

# Add shrub id names
row.names(frs.shrubs) <- row.names(coordinates)

# Lets also change the column names to be a bit easier to work with:
# name them using the convention: year followed by month: 
# 1. create two vector objects containing the years and months. 
years <- 1901:2018
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# 2. rename the columns of our data.frame
names(frs.shrubs) <- paste(rep(years, each=12), rep(month, times=116), sep="_")

# View(frs.shrubs)

# Modify data to long format wth columns for shrub, year, month and frost
frs.shrubs$Shrub_ID<-rownames(frs.shrubs)
rownames(frs.shrubs)<-NULL
frs.shrubsM<-melt(frs.shrubs)
colnames(frs.shrubsM)[colnames(frs.shrubsM)=="variable"] <- "year_month"
colnames(frs.shrubsM)[colnames(frs.shrubsM)=="value"] <- "frs"
frs.shrubsM$year<-substr(frs.shrubsM$year_month, 0, 4)
frs.shrubsM$month <- substr(frs.shrubsM$year_month, 6, 9)


# 5. Potential evapotranspiration -------------------------------------------------------------

nc.pet <- nc_open("cru_ts4.03.1901.2018.pet.dat.nc")
print(nc.pet)
# 2 variables: float pet which is pet & stn which is stations.
# 3 dimensions: lat, lon and time

# We only want to work on pet data:
pet <- brick("/Volumes/PF_HD/MastersThesis/ClimateData/cru_ts4.03.1901.2018.pet.dat.nc", varname="pet")
pet

# plot an example day:
plot(pet$X1901.01.16)

# we can also play with smaller areas:
ak.area <- extent(-155, -145, 65, 70) 
ak.pet <- crop(pet, ak.area)
plot(ak.pet$X1901.01.16)

# check that you sample points match:
plot(pet$X1901.01.16)
points(coordinates, pch=16)

# Extract climate data from the RasterBrick as a data.frame
pet.shrubs <- data.frame(extract(pet, coordinates, ncol=2))

# Add shrub id names
row.names(pet.shrubs) <- row.names(coordinates)

# Lets also change the column names to be a bit easier to work with:
# name them using the convention: year followed by month: 
# 1. create two vector objects containing the years and months. 
years <- 1901:2018
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# 2. rename the columns of our data.frame
names(pet.shrubs) <- paste(rep(years, each=12), rep(month, times=116), sep="_")
# View(pet.shrubs)

# Modify data to long format wth columns for shrub, year, month and pet
pet.shrubs$Shrub_ID<-rownames(pet.shrubs)
rownames(pet.shrubs)<-NULL
pet.shrubsM<-melt(pet.shrubs)
colnames(pet.shrubsM)[colnames(pet.shrubsM)=="variable"] <- "year_month"
colnames(pet.shrubsM)[colnames(pet.shrubsM)=="value"] <- "pet"
pet.shrubsM$year<-substr(pet.shrubsM$year_month, 0, 4)
pet.shrubsM$month <- substr(pet.shrubsM$year_month, 6, 9)


# 6. Wet Day Frequency -------------------------------------------------------------

nc.wet <- nc_open("cru_ts4.03.1901.2018.wet.dat.nc")
print(nc.wet)
# 2 variables: float wet which is wet & stn which is stations.
# 3 dimensions: lat, lon and time

# We only want to work on wet data:
wet <- brick("/Volumes/PF_HD/MastersThesis/ClimateData/cru_ts4.03.1901.2018.wet.dat.nc", varname="wet")
wet

# plot an example day:
plot(wet$X1901.01.16)

# we can also play with smaller areas:
ak.area <- extent(-155, -145, 65, 70) 
ak.wet <- crop(wet, ak.area)
plot(ak.wet$X1901.01.16)

# check that you sample points match:
plot(wet$X1901.01.16)
points(coordinates, pch=16)

# Extract climate data from the RasterBrick as a data.frame
wet.shrubs <- data.frame(extract(wet, coordinates, ncol=2))

# Add shrub id names
row.names(wet.shrubs) <- row.names(coordinates)

# Lets also change the column names to be a bit easier to work with:
# name them using the convention: year followed by month: 
# 1. create two vector objects containing the years and months. 
years <- 1901:2018
month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# 2. rename the columns of our data.frame
names(wet.shrubs) <- paste(rep(years, each=12), rep(month, times=116), sep="_")
# View(wet.shrubs)

# Modify data to long format wth columns for shrub, year, month and wet
wet.shrubs$Shrub_ID<-rownames(wet.shrubs)
rownames(wet.shrubs)<-NULL
wet.shrubsM<-melt(wet.shrubs)
colnames(wet.shrubsM)[colnames(wet.shrubsM)=="variable"] <- "year_month"
colnames(wet.shrubsM)[colnames(wet.shrubsM)=="value"] <- "wet"
wet.shrubsM$year<-substr(wet.shrubsM$year_month, 0, 4)
wet.shrubsM$month <- substr(wet.shrubsM$year_month, 6, 9)


# 7. Merge all climate data --------------------------------------------------

climate<-merge(tmp.shrubsM, tmx.shrubsM)
climate<-merge(climate, tmn.shrubsM)
climate<-merge(climate, pre.shrubsM)
climate<-merge(climate, frs.shrubsM)
climate<-merge(climate, pet.shrubsM)
climate<-merge(climate, wet.shrubsM)
str(climate)

# 8. Calculate yearly climate variables ----------------------------------------------

# create shrub_ID_year varable
climate$Shrub_ID_year <- do.call(paste, c(climate[c("Shrub_ID", "year")], sep = "_"))

# Calculate the following variables

    # 1. Mean summer temperature June-August (pity that don't know growth period lenght...)
    # 2. Yearly mean temperature
    # 3. Mean min summer temperature June-August
    # 4. Mean max summer temperature June-August
    # 5. Precipitation sum June-August
    # 6. Precipitation sum October-March (for snow)
    # 7. Yearly frost day sums
    # 8. Mean PET June-August
    # 9. Yearly wet day sums

# extract right months
climatesummer<-climate[climate$month=="Jun"|climate$month=="Jul"|climate$month=="Aug",]
climatewinter<-climate[climate$month=="Oct"|climate$month=="Nov"|climate$month=="Dec"|climate$month=="Jan"|climate$month=="Feb"|climate$month=="Mar",]

summ.temp<-tapply(climatesummer$tmp,list(climatesummer$Shrub_ID_year),mean)
summ.min<-tapply(climatesummer$tmn,list(climatesummer$Shrub_ID_year),mean)
summ.max<-tapply(climatesummer$tmx,list(climatesummer$Shrub_ID_year),mean)
temp<-tapply(climate$tmp,list(climate$Shrub_ID_year),mean)
summ.rain<-tapply(climatesummer$pre,list(climatesummer$Shrub_ID_year),sum)
wint.rain<-tapply(climatewinter$pre,list(climatewinter$Shrub_ID_year),sum)
frost<-tapply(climate$frs,list(climate$Shrub_ID_year),sum)
pet<-tapply(climate$pet,list(climate$Shrub_ID_year),mean)
wet<-tapply(climate$wet,list(climate$Shrub_ID_year),sum)


summ.temp<-data.frame(summ.temp)
Shrub_ID_year <- rownames(summ.temp)
rownames(summ.temp) <- NULL
summ.temp <- cbind(Shrub_ID_year,summ.temp)

temp<-data.frame(temp)
Shrub_ID_year <- rownames(temp)
rownames(temp) <- NULL
temp <- cbind(Shrub_ID_year,temp)

summ.min<-data.frame(summ.min)
Shrub_ID_year <- rownames(summ.min)
rownames(summ.min) <- NULL
summ.min <- cbind(Shrub_ID_year,summ.min)

summ.max<-data.frame(summ.max)
Shrub_ID_year <- rownames(summ.max)
rownames(summ.max) <- NULL
summ.max <- cbind(Shrub_ID_year,summ.max)

summ.rain<-data.frame(summ.rain)
Shrub_ID_year <- rownames(summ.rain)
rownames(summ.rain) <- NULL
summ.rain <- cbind(Shrub_ID_year,summ.rain)

wint.rain<-data.frame(wint.rain)
Shrub_ID_year <- rownames(wint.rain)
rownames(wint.rain) <- NULL
wint.rain <- cbind(Shrub_ID_year,wint.rain)

wet<-data.frame(wet)
Shrub_ID_year <- rownames(wet)
rownames(wet) <- NULL
wet <- cbind(Shrub_ID_year,wet)

pet<-data.frame(pet)
Shrub_ID_year <- rownames(pet)
rownames(pet) <- NULL
pet <- cbind(Shrub_ID_year,pet)

frost<-data.frame(frost)
Shrub_ID_year <- rownames(frost)
rownames(frost) <- NULL
frost <- cbind(Shrub_ID_year,frost)

climateAnnual<-merge(summ.temp, temp)
climateAnnual<-merge(climateAnnual, summ.min)
climateAnnual<-merge(climateAnnual, summ.max)
climateAnnual<-merge(climateAnnual, wint.rain)
climateAnnual<-merge(climateAnnual, pet)
climateAnnual<-merge(climateAnnual, wet)
climateAnnual<-merge(climateAnnual, frost)
climateAnnual<-merge(climateAnnual, summ.rain)


write.csv(climateAnnual, "/Users/peterfrank/Desktop/Master's Thesis/DataAnalysis/AlaskaShrubs/R_Data/CRU_ClimateAnnual.csv")


