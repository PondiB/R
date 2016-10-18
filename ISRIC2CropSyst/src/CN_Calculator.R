#calculate Curve Number
#author: John Mutua

require(raster) 
require(rgdal)

setwd("D:/ToBackup/Projects/SWAT/ArcSWAT_Projects/Sasumua_data/ISRIC2Cropsyst_Sasumua_Clustered")
layers<-list.files(".", pattern='tif')


Clay_m2 = -0.0054
Clay_m1 = 1.166173862
Clay_b = 33.83923291
Silt_m2 = -0.0016
Silt_m1 = 0.203162484
Silt_b = -3.91902717

CLAY=raster("af_CLYPPT_T__M_sd1_250m.tif")
SILT=raster("af_SLTPPT_T__M_sd1_250m.tif")

#calculate SAND
SAND = 100 - CLAY - SILT

CN = round((Clay_m2 * CLAY ^ 2 + Clay_m1 * CLAY + Clay_b) + (Silt_m2 * SILT ^ 2 + Silt_m1 * SILT + Silt_b), 1)

###plot(CN)

writeRaster(CN, filename = "Sasumua_CN.tif", format = "GTiff", overwrite = TRUE)

plot(CN)
