# Master's Thesis Shrub Data Subsample
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-10-06

install.packages("splitstackshape")
library(splitstackshape)

#STRATIFIED SUBSAMPLE SHRUBS: 
#The objective is to subsample 2 shrubs of each species per transect
#for a total of 6 shrubs per transect and 24 shrubs per site. 

StratShrubData = stratified(ShrubData, c("SiteID", "Species"), size = 2)

write.csv(StratShrubData, file = "StratShrubData")
