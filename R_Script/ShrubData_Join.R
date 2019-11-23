# Merge Shrub, Climate and Herbivory Datasets
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-11-21

#Join together the Shrub chronology, site characteristic, climate and herbivory data 
#into a final dataset ready for alanysis

#Creates a field in the shrub chronology data "Shrub_ID_Year" which can be used to join to the climate data
SubSample_join$Shrub_ID_year <- do.call(paste, c(SubSample_join[c("Shrub_ID", "Year")], sep = "_"))

#Joins the climate and shrub chronology data together
Shrub_Climate_Join = join(SubSample_join, climateAnnual, by='Shrub_ID_year', type='left', match='all')
