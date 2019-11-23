# Herbivore Temporal Data 
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

# Processes herbivore data 


#### 1. HERBIVORE DATA ####

colnames(Herbivore_Data)[colnames(Herbivore_Data)=="X1"] <- "GMU"

herbivore_melt<-melt(Herbivore_Data, id="GMU", na.rm = TRUE)