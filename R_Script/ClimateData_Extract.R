# Extract Climate Data from CRU-TS and SNAP Datasets
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-11-21

#Construct a climate data file that can be merged to growth and browsing data



# Finds all the files with extension "tif" in the RasterStack_TIFF Folder
temp.files <- list.files(path= "~/Desktop/Masters Thesis/ClimateData/SNAP_Data/tas",
                        pattern='tif', full.names=TRUE )

precip.files = list.files(path= "~/Desktop/Masters Thesis/ClimateData/SNAP_Data/pr",
                        pattern='tif', full.names=TRUE )

temp.stack = stack(temp.files) 

precip.stack = stack(precip.files)

# Read in Shapefile for sample sites
Sample_Sites = readOGR(dsn = "~/Desktop/Masters Thesis/Sample_Sites", layer = "Sample_Sites")

data(wrld_simpl)

# Establish geographic extent for Alaksa Study Area
max.lat <- 75 
min.lat <- 50 
max.lon <- -125
min.lon <- -170
geographic.extent <- extent(x = c(min.lon, max.lon, min.lat, max.lat))

# Plot the base map
plot(wrld_simpl, 
     xlim = c(min.lon, max.lon),
     ylim = c(min.lat, max.lat),
     axes = TRUE, 
     col = "grey95")

plot(Sample_Sites, col = "blue", pch = 3, cex = 0.75)

#Extract Climate Data for sample sites
temp_extract = extract(temp.stack, Sample_Sites) 

write.csv(temp_extract, file = "~/Desktop/temp_extract.csv")

precip_extract = extract(precip.stack, Sample_Sites) 

write.csv(precip_extract, file = "~/Desktop/precip_extract.csv")



##### ARCTIC DENDRO NETWORK #####

# Construct a climate data file that can be merged to growth
# and browsing data

# Notes -------------------------------------------------------------------


# Data source
# https://catalogue.ceda.ac.uk/uuid/10d3e3640f004c578403419aac167d82
# https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.03/

# Preparations ------------------------------------------------------------

rm(list=ls())
setwd("H:/Trondheim/ARCTIC-ALPINE SHRUBS/A Data_Templates/FILLED/ArcticShrubs")
getwd()


### Load packages
# install.packages("raster", lib="C:/RpackagesQuebec")
library(sp, lib="C:/RpackagesQuebec")
library(raster, lib="C:/RpackagesQuebec")
# install.packages("ncdf4", lib="C:/RpackagesQuebec")
library(ncdf4, lib="C:/RpackagesQuebec")

library(reshape2, lib="C:/RpackagesQuebec")



# Read data ---------------------------------------------------------------


# shrub coordinates for which climate data need to be retrieved:
coordinates<-read.csv(file = "sitecoordinates.csv",
                      header = TRUE,
                      dec = ".")
coordinates$X<-NULL
colnames(coordinates)[colnames(coordinates)=="Latitude"] <- "lat"
colnames(coordinates)[colnames(coordinates)=="Longitude"] <- "lon"
row.names(coordinates) <- coordinates$Shrub_ID
coordinates[1] <- NULL

str(coordinates)

# temperature data from ceda
# tmp<-read.table(gzfile("cru_ts4.03.1901.2018.tmp.dat.gz"))
# tmpnc<-read.table(gzfile("cru_ts4.03.1901.2018.tmp.dat.nc.gz"))
# tmpstn<-read.table(gzfile("cru_ts4.03.1901.2018.tmp.stn.gz"))
# Couldn't download these versions without losing metadata;

# => a new file downloaded from http://wps-web1.ceda.ac.uk/submit/form?proc_id=Subsetter

# csv formats are extremely clumsy...

# stuggled with this for a while until I realized that there are code and even an R
# package avaiable for reading this data!
# https://www.benjaminbell.co.uk/2018/01/getting-climate-data.html

# actually best nc format IS given here after all:
# https://catalogue.ceda.ac.uk/uuid/10d3e3640f004c578403419aac167d82
# this is nc file, can be unzipped with 7-zip. 



# CLIMATE -----------------------------------------------------------------



# * Temperature -------------------------------------------------------------

# this is big data, read from separate folder:
setwd("T:/vm/inh/botanisk/Bruker/Kata/ArcticShurbsClimate")
getwd()

nc.temp <- nc_open("cru_ts4.03.1901.2018.tmp.dat.nc")
print(nc.temp)
# 2 variables: float tmp which is temperature & stn which is stations.
# 3 dimensions: lat, lon and time

# We only want to work on tmp data:
tmp <- brick("T:/vm/inh/botanisk/Bruker/Kata/ArcticShurbsClimate/cru_ts4.03.1901.2018.tmp.dat.nc", varname="tmp")
tmp

# plot an example day:
plot(tmp$X1901.01.16)

# we can also play with smaller areas:
uk.area <- extent(-12, 4, 48, 64)
uk <- crop(tmp, uk.area)
plot(uk$X1901.01.16)

# check that you sample points match:
plot(tmp$X1901.01.16)
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
# View(tmp.shrubs)

# Modify data to long format wth columns for shrb, year, month and temp
tmp.shrubs$Shrub_ID<-rownames(tmp.shrubs)
rownames(tmp.shrubs)<-NULL
tmp.shrubsM<-melt(tmp.shrubs)
colnames(tmp.shrubsM)[colnames(tmp.shrubsM)=="variable"] <- "year_month"
colnames(tmp.shrubsM)[colnames(tmp.shrubsM)=="value"] <- "tmp"
tmp.shrubsM$year<-substr(tmp.shrubsM$year_month, 0, 4)
tmp.shrubsM$month <- substr(tmp.shrubsM$year_month, 6, 9)




# * Max Temperature -------------------------------------------------------------


nc.tempmax <- nc_open("cru_ts4.03.1901.2018.tmx.dat.nc")
print(nc.tempmax)
# 2 variables: float tmx which is tempmax & stn which is stations.
# 3 dimensions: lat, lon and time

# We only want to work on tmx data:
tmx <- brick("T:/vm/inh/botanisk/Bruker/Kata/ArcticShurbsClimate/cru_ts4.03.1901.2018.tmx.dat.nc", varname="tmx")
tmx

# plot an example day:
plot(tmx$X1901.01.16)

# we can also play with smaller areas:
uk.area <- extent(-12, 4, 48, 64)
uk <- crop(tmx, uk.area)
plot(uk$X1901.01.16)

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


# * Min Temperature -------------------------------------------------------------


nc.tempmin <- nc_open("cru_ts4.03.1901.2018.tmn.dat.nc")
print(nc.tempmin)
# 2 variables: float tmn which is tempmin & stn which is stations.
# 3 dimensions: lat, lon and time

# We only want to work on tmn data:
tmn <- brick("T:/vm/inh/botanisk/Bruker/Kata/ArcticShurbsClimate/cru_ts4.03.1901.2018.tmn.dat.nc", varname="tmn")
tmn

# plot an example day:
plot(tmn$X1901.01.16)

# we can also play with smaller areas:
uk.area <- extent(-12, 4, 48, 64)
uk <- crop(tmn, uk.area)
plot(uk$X1901.01.16)

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


# * Precipitation -------------------------------------------------------------


nc.rain <- nc_open("cru_ts4.03.1901.2018.pre.dat.nc")
print(nc.rain)
# 2 variables: float pre which is rain & stn which is stations.
# 3 dimensions: lat, lon and time

# We only want to work on pre data:
pre <- brick("T:/vm/inh/botanisk/Bruker/Kata/ArcticShurbsClimate/cru_ts4.03.1901.2018.pre.dat.nc", varname="pre")
pre

# plot an example day:
plot(pre$X1901.01.16)

# we can also play with smaller areas:
uk.area <- extent(-12, 4, 48, 64)
uk <- crop(pre, uk.area)
plot(uk$X1901.01.16)

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


# * Frost -------------------------------------------------------------


nc.frost <- nc_open("cru_ts4.03.1901.2018.frs.dat.nc")
print(nc.frost)
# 2 variables: float frs which is frost & stn which is stations.
# 3 dimensions: lat, lon and time

# We only want to work on frs data:
frs <- brick("T:/vm/inh/botanisk/Bruker/Kata/ArcticShurbsClimate/cru_ts4.03.1901.2018.frs.dat.nc", varname="frs")
frs

# plot an example day:
plot(frs$X1901.01.16)

# we can also play with smaller areas:
uk.area <- extent(-12, 4, 48, 64)
uk <- crop(frs, uk.area)
plot(uk$X1901.01.16)

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


# * Potential evapotranspiration -------------------------------------------------------------


nc.pet <- nc_open("cru_ts4.03.1901.2018.pet.dat.nc")
print(nc.pet)
# 2 variables: float pet which is pet & stn which is stations.
# 3 dimensions: lat, lon and time

# We only want to work on pet data:
pet <- brick("T:/vm/inh/botanisk/Bruker/Kata/ArcticShurbsClimate/cru_ts4.03.1901.2018.pet.dat.nc", varname="pet")
pet

# plot an example day:
plot(pet$X1901.01.16)

# we can also play with smaller areas:
uk.area <- extent(-12, 4, 48, 64)
uk <- crop(pet, uk.area)
plot(uk$X1901.01.16)

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


# * Wet -------------------------------------------------------------


nc.wet <- nc_open("cru_ts4.03.1901.2018.wet.dat.nc")
print(nc.wet)
# 2 variables: float wet which is wet & stn which is stations.
# 3 dimensions: lat, lon and time

# We only want to work on wet data:
wet <- brick("T:/vm/inh/botanisk/Bruker/Kata/ArcticShurbsClimate/cru_ts4.03.1901.2018.wet.dat.nc", varname="wet")
wet

# plot an example day:
plot(wet$X1901.01.16)

# we can also play with smaller areas:
uk.area <- extent(-12, 4, 48, 64)
uk <- crop(wet, uk.area)
plot(uk$X1901.01.16)

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


# * Merge all climate data --------------------------------------------------

climate<-merge(tmp.shrubsM, tmx.shrubsM)
climate<-merge(climate, tmn.shrubsM)
climate<-merge(climate, pre.shrubsM)
climate<-merge(climate, frs.shrubsM)
climate<-merge(climate, pet.shrubsM)
climate<-merge(climate, wet.shrubsM)
str(climate)

# * Calculate yearly climate variables ----------------------------------------------

# create shrub_ID_year varable
climate$Shrub_ID_year <- do.call(paste, c(climate[c("Shrub_ID", "year")], sep = "_"))

# Calculate
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

write.csv(climateAnnual, "H:/Trondheim/ARCTIC-ALPINE SHRUBS/A Data_Templates/FILLED/A_COMBININGALL/climateAnnual.csv")


# NPP ----------------------------------------------------

library(sp, lib="C:/RpackagesQuebec")
library(raster, lib="C:/RpackagesQuebec")
library(RColorBrewer, lib="C:/RpackagesQuebec")
library(latticeExtra, lib="C:/RpackagesQuebec")
library(rasterVis, lib="C:/RpackagesQuebec")
library(rgdal, lib="C:/RpackagesQuebec")
library(purrr, lib="C:/RpackagesQuebec")

# Data of productivity (NPP, July 2016) g/m2/day
proddat<-raster('MOD17A2_M_PSN_2016-07-01_rgb_3600x1800.FLOAT.tiff')
coordinatesnpp$NPP_July2016<-extract(proddat,coordinates)

write.csv(coordinatesnpp, "H:/Trondheim/ARCTIC-ALPINE SHRUBS/A Data_Templates/FILLED/A_COMBININGALL/Productivity.csv")


# MAP ---------------------------------------------------------------------

library(rgdal)
library(raster)

# Polar projection
polarproj<-'+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 '
# Make the coordinates as spatial points
coord_sp<-SpatialPoints(coordinates,proj4string = CRS('+proj=longlat + datum=WGS1984'))
# Project to polar projection
coord_pp<-project(coord_sp@coords,polarproj)

# Country outlines
noreco_countries <- c('NO', 'SE', 'FI','RU','CA','IS','GL','SJ','MN','JP') 
noreco_shp1 <- do.call("bind", lapply(noreco_countries, function(x)  raster::getData('GADM', country=x, level=0)))
# Only Alaska from USA #had to add raster::getData due to error in reading the argument US
us<-raster::getData('GADM',country='US',level=1)
alaska<-us[us$NAME_1=='Alaska',]
# Bind
noreco_shp<-bind(noreco_shp1,alaska)
# Project
noreco_shppp<-spTransform(noreco_shp,CRS=crs(polarproj))

# Plot (play around with scaling to zoom in if required...)
plot(noreco_shppp) #High res country outlines plot very slow...
points(coord_pp,col=2,pch=16)
