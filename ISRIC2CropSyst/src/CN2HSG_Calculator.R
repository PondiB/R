#calculate HSG
#author: John Mutua

#load packages
require(raster) 
require(rgdal)
require(rasterVis)
require(colorspace)

#set working directory
setwd("D:/ToBackup/Projects/Water_Fund/ThikaChania/CropSyst_Script/data")
layers<-list.files(".", pattern='tif')
CN=raster("CN.tif")

# reclassify the values into 4 HSG groups 
# build the matrix for the reclassification
m <- matrix(c(0, 36, 1, 
              36, 65, 2, 
              65, 85, 3, 
              85, Inf, 4),ncol=3, byrow=TRUE)

#reclassify
HSG <- reclassify(CN, m)

# #add a Raster Atribute Table(RAT) and define the raster as categorical
# HSG <- ratify(HSG)

# #configure the RAT: first create a RAT data.frame using the
# #levels method; second, set the values for each class (to be
# #used by levelplot); third, assign this RAT to the raster
# #using again levels
# rat <- levels(HSG)[[1]]
# rat$classes <- c("B", "C")
# levels(HSG) <- rat
# levelplot(HSG, col.regions=terrain_hcl(4))

#write and plot(r2)
writeRaster(HSG , filename = "HSG.tif", format = "GTiff", overwrite = TRUE)
plot(HSG)
