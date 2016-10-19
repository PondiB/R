#reduction in CN in response to interventions
#author: John Mutua

require(raster) 
require(rgdal)

setwd("D:/ToBackup/Projects/SWAT/ArcSWAT_Projects/Sasumua_data/ISRIC2Cropsyst_NEW")
layers<-list.files(".", pattern='tif')
CN=raster("Tana_CN.tif")

# Crop residue; reduction by -2.1
cr<-CN-2.1
writeRaster(cr, filename = "CN_CropResidue.tiff", format = "GTiff", overwrite=TRUE)
plot(cr)

# Contours	reduction by -2.5
C<-CN-2.5
writeRaster(C, filename = "CN_Contours.tiff", format = "GTiff", overwrite=TRUE)
plot(c)

# Contours + Crop residues; reduction by -3.5
c_cr<-CN-3.5
writeRaster(c_cr, filename = "CN_ContoursCropResidues.tiff", format = "GTiff", overwrite=TRUE)
plot(c_cr)

# Contours + Terraces	-5.7
c_t<-CN-5.7
writeRaster(c_t, filename = "CN_ContoursTerraces.tiff", format = "GTiff", overwrite=TRUE)
plot(c_t)

# Contours + Terraces + Crop residues	-7.0
c_t_cr<-CN-7.0
writeRaster(c_t_cr, filename = "CN_ContoursTerracesCropResidues.tiff", format = "GTiff", overwrite=TRUE)
plot(c_t_cr)
