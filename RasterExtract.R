##Extract raster values from rasters using a point shapefile

require("raster"); require("tiff"); require("sp"); require("rgdal")

#Path to .tif files directory
setwd("Z:/CIAT GIS DATASETS/Regional Level/Africa/ISRIC_250m")
isric_soil = list.files(getwd(), pattern = "\\.tif$")  # Get only the .tif files

#Stack all .tif in the directory
ras_stack <- stack(isric_soil)

#Read the point point shape file
pt <- readOGR(dsn = "D:/ToBackup/Data/Scientist requests/Teresa", layer = "Sasumua_farms")
p <- spTransform(pt, crs(ras_stack))

#Extract raster values and convert to data frame
df1 <- extract(ras_stack, p)
df1 <- as.data.frame(df1)

#set the name of the rows with the lable of the points
rownames(df1)<-p@data$Site

#Write the .csv file
write.csv(df1, file = "D:/ToBackup/Data/Scientist requests/Teresa/Soil_Sasumua_farms.csv", row.names=TRUE)
