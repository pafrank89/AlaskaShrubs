# Merge Shrub, Climate and Herbivory Datasets
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-11-21

#Join together the Shrub chronology, site characteristic, climate and herbivory data 
#into a final dataset ready for alanysis

#Creates a field in the shrub chronology data "Shrub_ID_Year" which can be used to join to the climate data
sd_all$Shrub_ID_year <- do.call(paste, c(sd_final[c("ShrubID", "Year")], sep = "_"))

#Joins the climate and shrub chronology data together
sd_all_cc = join(sd_all, iem.climateAnnual, by='Shrub_ID_year', type='left', match='all')

sd_all_cc = join(sd_all_cc, climateAnnual, by='Shrub_ID_year', type='left', match='all')

#Joins the herbivore data to the combined shrub chronology and climate data
  #CCH stands for chronology, climate and herbivory 

#Joins the Hare cycle data
sd_all_cch = join(sd_all_cc, HareCycle, by='Year', type='left', match='all')

#Adds a column GMU_year which can be used to join on the Moose density data
sd_all_cch$GMU_year <- do.call(paste, c(sd_all_cch[c("GMU", "Year")], sep = "_"))

#Joins the moose density data
sd_all_cch = join(sd_all_cch, MooseDensity, by='GMU_year', type='left', match='all')

#Subset the data to remove records which are outside the range of climatic or herbivore temporal data
sd_final_cch = filter(sd_all_cch, Year >= 1987)
sd_final_cch = filter(sd_final_cch, Year <= 2015)

write.csv(sd_final_cch, "/Users/peterfrank/Documents/Master's Thesis/DataAnalysis/AlaskaShrubs/R_Data/Shrub_CCH.csv")

