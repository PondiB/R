#calculate slope length
#author: John Mutua

#load packages
require(raster) 
require(rgdal)

#set working directory
setwd("D:/ToBackup/Projects/SWAT/ArcSWAT_Projects/Sasumua_data/ISRIC2Cropsyst_Sasumua_Clustered")
layers<-list.files(".", pattern='tif')

slp<-raster("Sasumua_Slope.tif")
slp_perc<-slp*100

#create function (f) to calculate slope length
f <- function(x) {
  y <- -0.156 * x^2 + 11.2 * x + 0.1
  y[x > 33] <- 200
  y
}

#apply f on slope raster
slpl <- calc(slp_perc, f)

#write raster
writeRaster(slpl, filename = "Sasumua_SlopeLength.tif", format = "GTiff", overwrite = TRUE)

#plot
plot(slpl)
