# Shrub Chronology Cross Dating
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-11-20

install.packages("dplR")
install.packages("utils")

library(dplR)
library(dplyr)
library(utils)

#Develops a chronology for a rwl file
crn_147_SALIX <- chron(mean_rwl_147_SALIX)

#Plots the chronology file using 
plot(crn_147_SALIX, add.spline=TRUE, nyrs=15)

#Reorders the rwl file based on the row.names so that the smallest year is first.
#This is necessary to vizualize the Spaghetti Plot 
mean_rwl_147_SALIX <- mean_rwl_147_SALIX[ order(row.names(mean_rwl_147_SALIX)), ]

#Creates a Spaghetti Plot of the ring widths 
spag.plot(mean_rwl_147_SALIX, zfac = 1)

#Creates a correlation plot for the base ring width legnths 
corr_147_SALIX <- corr.rwl.seg(mean_rwl_147_SALIX, seg.length = 2, pcrit=0.01)

####C_4_corr_147_SALIX <- corr.series.seg(rwl= mean_rwl_147_SALIX, series="X147.4.C.SAPU",
                          ####+ seg.length = 4)

#------------------------------------------------------------------------------------------

#Develops a chronology for a rwl file
crn_147_BENA <- chron(mean_rwl_147_BENA)

#Plots the chronology file using 
plot(crn_147_BENA, add.spline=TRUE, nyrs=15)

#Reorders the rwl file based on the row.names so that the smallest year is first.
#This is necessary to vizualize the Spaghetti Plot 
mean_rwl_147_BENA <- mean_rwl_147_BENA[ order(row.names(mean_rwl_147_BENA)), ]

#Creates a Spaghetti Plot of the ring widths 
spag.plot(mean_rwl_147_BENA, zfac = 1)

#Creates a correlation plot for the base ring width legnths 
corr_147_BENA <- corr.rwl.seg(mean_rwl_147_BENA, seg.length = 2, pcrit=.99)
