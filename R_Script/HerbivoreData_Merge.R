# Herbivore Temporal Data 
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-11-21

#MooseDensity = MooseDensity[-c(4,5,6,7,8), ] 

# Melt moose density data  
MooseDensity<-melt(MooseDensity, id="GMU", na.rm = TRUE)

#Rename the columns which were given generic names in the melt
colnames(MooseDensity)[colnames(MooseDensity)=="variable"] <- "Year"
colnames(MooseDensity)[colnames(MooseDensity)=="value"] <- "MooseDensity"

#Add a new column combining GMU and Year on which to join to the larger dataset
MooseDensity$GMU_year <- do.call(paste, c(MooseDensity[c("GMU", "Year")], sep = "_"))

#Deletes the Year and GMU columns which are now merged, so that they don't show up in the big dataset
MooseDensity[1] = NULL
MooseDensity[1] = NULL

#Reorders the columns 
MooseDensity = MooseDensity[,c(2,1)]

