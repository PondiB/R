#load rasters
require('raster')
require('rgdal')

#set working directory
setwd('D:/ToBackup/Projects/Land Degradation Neutrality_Namibia/LULC/Bush_Density/Outputs')

#import rasters
rasList <- list.files('.', pattern = '.tif$', full.names = TRUE)
bd1=raster('otjo_bd1.tif')
bd2=raster('otjo_bd2.tif')
bd3=raster('otjo_bd3.tif')


#smooth rasters using focal operation
bd1_s <- focal(bd1, w=matrix(1, 3, 3), mean, na.rm=TRUE, NAonly=FALSE)
bd2_s <- focal(bd2, w=matrix(1, 3, 3), mean, na.rm=TRUE, NAonly=FALSE)
bd3_s <- focal(bd3, w=matrix(1, 3, 3), mean, na.rm=TRUE, NAonly=FALSE)

#round raster values
bd1_s<-round(bd1_s, digits = 0)
bd2_s<-round(bd2_s, digits = 0)
bd3_s<-round(bd3_s, digits = 0)

#write rasters
writeRaster(bd1_s, filename='bd1_smooth1', format='GTiff', overwrite=TRUE, NAflag=-9999)
writeRaster(bd2_s, filename='bd2_smooth1', format='GTiff', overwrite=TRUE, NAflag=-9999)
writeRaster(bd3_s, filename='bd3_smooth1', format='GTiff', overwrite=TRUE, NAflag=-9999)
