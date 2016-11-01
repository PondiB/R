# calculate crown cover using ndvi and landsat bands as covariates
# author: Mutua John
# email: j.y.mutua@cgiar.org

# workspace clearance
rm(list = ls(all = TRUE))

# set the random seed 
set.seed(500)

# list of packages for the session
.packages = c("sp", "rgdal", "raster","randomForest", "plyr", "xlsx", "xlsxjars", "dplyr", "caret", "car")
# install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
# Load packages into session 
lapply(.packages, require, character.only=TRUE)

# set working directory
setwd("C:/Users/jymutua/WorkDocs/Bush_Density_Mapping")

# read in data from excel sheet
BushData <- read.xlsx("Field_data/Bush_Density_Sampling_Points.xlsx", sheetName = "BushDensity", header=TRUE)

# calculate values by finding the median in crown cover
# 1 plot 0.01ha; 4 plots 0.04ha
BushData$crown_cover <- apply(BushData[,4:7], 1, median, na.rm=TRUE)

# create new data frame with the columns you need
CrownCover<-BushData[,c("Waypoint_No","Latitude","Longitude", "crown_cover")]

# round columns and remove NAs
CrownCover<-CrownCover %>% mutate_each(funs(round(.,0)), crown_cover)
CrownCover<-CrownCover[complete.cases(CrownCover),]

# export data to .csv
write.csv(CrownCover, file = "Otjo_CrownCover_trainData.csv",row.names=FALSE)

#get long and lat from your data.frame. Make sure that the order is in lon/lat.
#convert the dataraframe into a spatial point dataframe
xy <- CrownCover[,c(3,2)]
trainDatageo <- SpatialPointsDataFrame(coords = xy, data = CrownCover,
                                       proj4string = CRS("+proj=longlat 
                                                         +datum=WGS84"))
trainData <- spTransform(trainDatageo, CRS('+proj=utm +zone=33 +south 
                                           +datum=WGS84 +units=m +no_defs 
                                           +ellps=WGS84 +towgs84=0,0,0'))
# check names of fields
names(trainData)

# ----import input data----
rasList <- list.files("Raster_data/", pattern = ".tif$", full.names = TRUE)

# #first stack, will enable us calculate NDVI
# rStack <- stack("Raster_data/Otjozondjupa_TOA.tif")
# names(rStack) <- c(paste0("B", 2:7, coll = ""))
# 
# #calculate NDVI to be used in the analysis
# NDVI <- (rStack[[4]] - rStack[[3]]) / (rStack[[4]] + rStack[[3]])
# writeRaster(NDVI, filename="Otjozondjupa_NDVI", format="GTiff", overwrite=TRUE)

# create a raster stack
rStack <- stack(rasList[2:3])

# rename rstack contents
names(rStack) <- c("NDVI", "band2", "band3", "band4", "band5", "band6", "band7")

# import Bush Area raster
Otjo_BushArea <- raster("Raster_data/Other_data/Otjozondjupa_BushArea_2016.tif")

# check various stuff
crs(rStack)
crs(Otjo_BushArea)
crs(trainData)
extent(rStack)
extent(Otjo_BushArea)
extent(trainData)

# set extent of the rasters to match that of training data
trainData@bbox <- bbox(Otjo_BushArea)

# mask the covariates or read in covariates
covs <- mask(rStack, Otjo_BushArea)

# assign raster values to training data
v<-as.data.frame(extract(covs,trainData))
trainData@data=data.frame(trainData@data, v[match(rownames(trainData@data),rownames(v)),])

# rename fields in training dataset
names(trainData) <- c("waypoint_no", "latitude", "longitude", "crown_cover", 
                      "NDVI","band2","band3","band4","band5","band6","band7")

# remove NAs
trainData@data<-trainData@data[complete.cases(trainData@data),]

# export extracted data to .csv
write.csv(trainData@data, file = "Otjo_CrownCover_MF_trainData.csv",row.names=FALSE)

#set up you rf model
tcontrol <- trainControl(method="boot", number=100)
cc_model <- train(as.factor(crown_cover) ~ NDVI + band2, method = "rf", trControl = tcontrol, data =  trainData@data)

# check for error convergence
plot(cc_model)
print(cc_model)

# plot mean decrease in variable importance
varImpPlot(cc_model, type=1)

#predict the model
beginCluster()
prediction <- clusterR(covs, raster::predict, args = list(model = cc_model))
endCluster()

#save your output
writeRaster(prediction, filename="Otjozondjupa_CrownCover_2016_new", format="GTiff", overwrite=TRUE)
