#reduction in CN in response to interventions
#author: John Mutua

require(raster) 
require(rgdal)

setwd("D:/ToBackup/Projects/Water_Fund/ThikaChania/CropSyst_Script/data")
layers<-list.files(".", pattern='tif')
CN=raster("CN.tif")

# Crop residue; reduction by -2.1
cr<-CN*0.979
writeRaster(cr, filename = "D:/ToBackup/Projects/Water_Fund/ThikaChania/CropSyst_runs/cr.tiff", 
            format = "GTiff", overwrite=TRUE)
plot(cr)

# Contours	reduction by -2.5
C<-CN*0.975
writeRaster(C, filename = "D:/ToBackup/Projects/Water_Fund/ThikaChania/CropSyst_runs/c.tiff", 
            format = "GTiff", overwrite=TRUE)
plot(c)

# Contours + Crop residues; reduction by -3.5
c_cr<-CN*0.965
writeRaster(c_cr, filename = "D:/ToBackup/Projects/Water_Fund/ThikaChania/CropSyst_runs/c_cr.tiff", 
            format = "GTiff", overwrite=TRUE)
plot(c_cr)

# Contours + Terraces	-5.7
c_t<-CN*0.943
writeRaster(c_t, filename = "D:/ToBackup/Projects/Water_Fund/ThikaChania/CropSyst_runs/c_t.tiff", 
            format = "GTiff", overwrite=TRUE)
plot(c_t)

# Contours + Terraces + Crop residues	-7.0
c_t_cr<-CN*0.93
writeRaster(c_t_cr, filename = "D:/ToBackup/Projects/Water_Fund/ThikaChania/CropSyst_runs/c_t_cr.tiff", 
            format = "GTiff", overwrite=TRUE)
plot(c_t_cr)
