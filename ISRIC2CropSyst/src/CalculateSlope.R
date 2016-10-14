#calculate slope 
#author: John Mutua

#load packages
require(raster) 
require(rgdal)

#set working directory
setwd("D:/ToBackup/Projects/Water_Fund/ThikaChania/CropSyst_Script/data")
layers<-list.files(".", pattern='tif')

dem<-raster("Sasumua_DEM90m.tif")
plot(dem)

#calculate slope
slp <- terrain(dem, "slope")
plot(slp)

#write slope raster
writeRaster(slp, filename = "Sasumua_Slope.tif", format = "GTiff", overwrite = TRUE)

# #write slope in percent
# slp_per<-slp*100
# writeRaster(slp_per, filename = "Sasumua_Slope_Perc.tif", format = "GTiff", overwrite = TRUE)
# plot(slp_per)
