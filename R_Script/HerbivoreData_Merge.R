# Herbivore Temporal Data 
# Peter Frank 
# peterfr@stud.ntnu.no
# 2019-11-21

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

# Reorders the columns 
MooseDensity = MooseDensity[,c(2,1)]

# Plot Moose Density over Time ####

<<<<<<< HEAD
  #loess24 = loess(GMU_24A ~ Year, data = GMU_MooseDensity_Graph, span = 0.2)
  #loess20 = loess(GMU_20F ~ Year, data = GMU_MooseDensity_Graph, span = 0.2)
  #loess26 = loess(GMU_26B ~ Year, data = GMU_MooseDensity_Graph, span = 0.2)
=======
  #loess24 = loess(GMU_24A ~ Year, data = MD_Graph, span = 0.2)
  #loess20 = loess(GMU_20F ~ Year, data = MD_Graph, span = 0.2)
  #loess26 = loess(GMU_26B ~ Year, data = MD_Graph, span = 0.2)
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68

  #smooth24 = predict(loess24)
  #smooth20 = predict(loess20)
  #smooth26 = predict(loess26)

<<<<<<< HEAD
#lines(smooth24, x = GMU_MooseDensity_Graph$Year, col = "blue", lwd = 2)

par(bg = rgb(221, 231, 238, max = 255))

plot(GMU_24A ~ Year, data = GMU_MooseDensity_Graph, main = "Game Management Unit Moose Density",
     col = "blue", ylim=c(0,.65), xlim=c(1985, 2020), type = "o",
     ylab = "Moose Density (moose/km²)", xlab = "Year", cex.lab = 1.5)

par(new=TRUE)

plot(GMU_20F ~ Year, data = GMU_MooseDensity_Graph, 
=======
#lines(smooth24, x = MD_Graph$Year, col = "blue", lwd = 2)

plot(GMU_24A ~ Year, data = MD_Graph, main = "Game Management Unit Moose Density",
     col = "blue", ylim=c(0,.65), xlim=c(1985, 2020), type = "o",
     ylab = "Moose Density (moose/km²)", xlab = "Year")

par(new=TRUE)

plot(GMU_20F ~ Year, data = MD_Graph, 
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68
     col = "red", type = "o", xlab = "", ylab = "", axes=FALSE, ylim=c(0,.65), xlim=c(1985, 2020))

par(new=TRUE)

<<<<<<< HEAD
plot(GMU_26B ~ Year, data = GMU_MooseDensity_Graph, 
=======
plot(GMU_26B ~ Year, data = MD_Graph, 
>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68
     col = "forest green", type = "o", xlab = "", ylab = "", axes=FALSE, ylim=c(0,.65), xlim=c(1985, 2020))


legend("topright",legend=c("24A (4,146 km²)","20F (6,267 km²)", "26B (16,332 km²)"),
       text.col=c("blue", "red", "forest green"), 
<<<<<<< HEAD
       lty = c(1, 1, 1), col=c("blue", "red", "forest green"), bty = "n", cex=1.5)


# Plot Hare Cycle over Time ####

loessHare = loess(HareIndex ~ Year, data = HareCycle, span = .25)

smoothHare = predict(loessHare)

plot(HareIndex ~ Year, data = HareCycle, main = "",
     col = "white", ylim=c(0,4), xlim=c(1985, 2020), type = "o",
     ylab = "Hare Population Index", xlab = "Year")

lines(smoothHare, x = HareCycle$Year, col = "blue", lwd = 2)
=======
       lty = c(1, 1, 1), col=c("blue", "red", "forest green"), bty = "n", cex=1)

>>>>>>> 66da247bda7c4fa105412c7d66a446f439545b68
