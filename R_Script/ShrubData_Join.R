# Merge Shrub, Climate and Herbivory Datasets
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-11-21

#Join together the Shrub chronology, site characteristic, climate and herbivory data 
#into a final dataset ready for alanysis

#Creates a field in the shrub chronology data "Shrub_ID_Year" which can be used to join to the climate data
SubSample_join$Shrub_ID_year <- do.call(paste, c(SubSample_join[c("ShrubID", "Year")], sep = "_"))

#Joins the climate and shrub chronology data together
Shrub_Climate_Join = join(SubSample_join, climateAnnual, by='Shrub_ID_year', type='left', match='all')

#Joins the herbivore data to the combined shrub chronology and climate data
  #CCH stands for chronology, climate and herbivory 

#Joins the Hare cycle data
ShrubCCH_Data = join(Shrub_Climate_Join, HareCycle_Data, by='Year', type='left', match='all')

#Adds a column GMU_year which can be used to join on the Moose density data
ShrubCCH_Data$GMU_year <- do.call(paste, c(ShrubCCH_Data[c("GMU", "Year")], sep = "_"))

#Joins the moose density data
ShrubCCH_Data = join(ShrubCCH_Data, MooseDensity_Data, by='GMU_year', type='left', match='all')


write.csv(ShrubCCH_Data, "/Users/peterfrank/Desktop/Master's Thesis/DataAnalysis/AlaskaShrubs/R_Data/Shrub_CCH.csv")
