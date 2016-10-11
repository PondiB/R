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
setwd("C:/Users/jymutua/Documents/Bush_Density_Mapping")

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

# convert the dataframe into a spatial points dataframe
# get long and lat from your data.frame. Make sure that the order is in lon/lat.
xy <- CrownCover[,c(3,2)]
trainData <- SpatialPointsDataFrame(coords = xy, data = CrownCover,
                                    proj4string = CRS("+proj=utm +zone=33 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# check names of fields
names(trainData)

# ----import input data----
rasList <- list.files("Raster_data/", pattern = ".tif$", full.names = TRUE)

# create a raster stack
rstack <- stack(rasList[2:3])

# rename rstack contents
names(rstack) <- c("NDVI", "band2", "band3", "band4", "band5", "band6", "band7")

# import Bush Area raster
Otjo_BushArea <- raster("C:/Users/jymutua/Documents/Bush_Density_Mapping/Raster_data/Other_data/Otjozondjupa_BushArea_2016.tif")

# check various stuff
crs(rstack)
crs(Otjo_BushArea)
crs(trainData)
extent(rstack)
extent(Otjo_BushArea)
extent(trainData)

# set extent of the rasters to match that of training data
e<-extent(trainData)
extent(rstack)<-e
extent(Otjo_BushArea)<-e

# mask the covariates or read in covariates
covs <- mask(rstack, Otjo_BushArea)
writeRaster(covs, filename = "Otjozondjupa_Stack_2016.tif", format = "GTiff", overwrite = TRUE)

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

# randomly select index numbers and use that for splitting the data
# set 75% as training nad 25% as test data
trainIndex=sample(1:nrow(trainData@data),size=0.75*nrow(trainData@data))
trainingSet=trainData@data[trainIndex,]
testingSet=trainData@data[-trainIndex,]

# predict crown_cover
accuracies<-c()

# construct the rf model
cc_model <- randomForest(x=trainingSet[,c(5:6)],
                         y=as.factor(trainingSet[,"crown_cover"]), 
                         ntree=2000, proximity=TRUE, importance=TRUE)
# check for error convergence
plot(cc_model)
print(cc_model)

# plot mean decrease in variable importance
varImpPlot(cc_model, type=1)

# Predict Model
predict(covs, cc_model, filename="Otjozondjupa_CrownCover_2016", format='GTiff', type="response", 
        index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

# check accuracy
prediction <- predict(cc_model, testingSet)
testingSet$rightPred <- prediction == testingSet$crown_cover
t<-table(prediction, testingSet$crown_cover)
print(t)
accuracy <- sum(testingSet$rightPred)/nrow(testingSet)
accuracies <- c(accuracies,accuracy)
print(accuracy)
