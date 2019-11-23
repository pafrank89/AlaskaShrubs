# Herbivore Temporal Data 
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-11-21

# Melt moose density data  
MooseDensity_Data<-melt(MooseDensity_Data, id="GMU", na.rm = TRUE)

#Rename the columns which were given generic names in the melt
colnames(MooseDensity_Data)[colnames(MooseDensity_Data)=="variable"] <- "Year"
colnames(MooseDensity_Data)[colnames(MooseDensity_Data)=="value"] <- "MooseDensity"

#Add a new column combining GMU and Year on which to join to the larger dataset
MooseDensity_Data$GMU_year <- do.call(paste, c(MooseDensity_Data[c("GMU", "Year")], sep = "_"))

#Deletes the Year and GMU columns which are now merged, so that they don't show up in the big dataset
MooseDensity_Data[1] = NULL
MooseDensity_Data[1] = NULL

#Reorders the columns 
MooseDensity_Data = MooseDensity_Data[,c(2,1)]

