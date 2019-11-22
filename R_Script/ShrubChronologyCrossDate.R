# Shrub Chronology Cross Dating
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-11-20

install.packages("dplR")

library(dplR)
library(dplyr)

#BAI Calculation using measured ring widths (mm) from Image J.
#Ring widths are listed from the bark to the pith, so the function bai.out in the dplR package will be used.

data(co021)


write.csv(all.rwl, file = "all_rwl")
