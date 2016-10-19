#calculate slope 
#author: John Mutua

#load packages
require(raster) 
require(rgdal)

#set working directory
setwd("D:/ToBackup/Projects/SWAT/ArcSWAT_Projects/Sasumua_data/ISRIC2Cropsyst_NEW")
layers<-list.files(".", pattern='tif')

dem<-raster("DEM.tif")
plot(dem)

#calculate slope
slp <- terrain(dem, "slope")
plot(slp)

#write slope raster
writeRaster(slp, filename = "Tana_Slope.tif", format = "GTiff", overwrite = TRUE)
