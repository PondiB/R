# perform a random forest classification

# workspace clearance
rm(list = ls(all = TRUE))

# set the random seed 
set.seed(500)

# List of packages for the session
.packages = c("sp", "rgdal", "raster","randomForest", "plyr", "xlsx", "dplyr", "caret", "car")
# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
# Load packages into session 
lapply(.packages, require, character.only=TRUE)

# set working directory
setwd("D:/Users/DemoA/WorkDocs/BD_data/Bush_Density_Mapping")

# read in data from excel sheet
BushData <- read.xlsx("D:/Users/DemoA/WorkDocs/BD_data/Bush_Density_Mapping/Field_data/Bush_Density_Sampling_Points.xlsx", 
                      sheetName = "BushDensity", stringsAsFactors = FALSE, header=TRUE, as.data.frame=TRUE)

# calculate values by finding the median in crown cover and count of shrubs
# 1 plot 0.01ha; 4 plots 0.04ha
BushData$crown_cover <- apply(BushData[,4:7], 1, median, na.rm=TRUE)
BushData$shrubs_less_1.5 <- apply(BushData[,8:11], 1, sum, na.rm=TRUE)
BushData$shrubs_more_1.5_no_stem <- apply(BushData[,16:19], 1, sum, na.rm=TRUE)
BushData$shrubs_more_1.5_stem <- apply(BushData[,24:27], 1, sum, na.rm=TRUE)

# create new data frame with the columns you need
BushData_clean<-BushData[,c("Waypoint_No","Latitude","Longitude", "crown_cover",
                            "shrubs_less_1.5","shrubs_more_1.5_no_stem","shrubs_more_1.5_stem")]

# to calculate crown cover
# crown cover for year 2016, defined as crown cover for all vegetation taller than 1.5 in height. 
# encoded as a percentage per output grid cell, in the range 0-100.
BushData_clean$shrubs_more_1.5 <- apply(BushData_clean[,6:7], 1, sum, na.rm=TRUE)

# round columns
BushData_clean<-BushData_clean %>% mutate_each(funs(round(.,0)), 
                                               crown_cover, shrubs_less_1.5, shrubs_more_1.5_no_stem, shrubs_more_1.5_stem, shrubs_more_1.5)

# remove NAs
BushData_clean<-BushData_clean[complete.cases(BushData_clean),]

# export data to .csv
write.csv(BushData_clean, file = "Otjozondjupa_trainData.csv",row.names=FALSE)

# read point shapefile
trainData <- readOGR(getwd(), layer = "trainData")

# # convert csv to shapefile
# # create a crs object
# utm33sCRS <- crs("+proj=utm +zone=33 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
# 
# # note that the longitude and latitude columns are in columns 3 and 2
# trainData <- SpatialPointsDataFrame(BushData_clean[,3:2],
#                                     BushData_clean,
#                                     proj4string = utm33sCRS)
# 
# # write trainData as a shapefile
# writeOGR(trainData, ".", "trainData", driver="ESRI Shapefile", overwrite=TRUE)

# ----import input data----
rasList <- list.files("Raster_data/", pattern = ".tif$", full.names = TRUE)

# create a raster stack
rstack <- stack(rasList[2:3])

# rename rstack contents
names(rstack) <- c("NDVI", "band2", "band3", "band4", "band5", "band6", "band7")

# import Bush Area raster
Otjo_BushArea <- raster("D:/Users/DemoA/WorkDocs/BD_data/Bush_Density_Mapping/Raster_data/Otjozondjupa_BushArea_2016.tif")

# check various stuff
crs(rstack)
crs(Otjo_BushArea)
crs(trainData)
extent(rstack)
extent(Otjo_BushArea)
extent(trainData)

# set extent of the training data to match covs
trainData@bbox <- bbox(Otjo_BushArea)

# mask the covariates or read in covariates
covs <- mask(rstack, Otjo_BushArea)
writeRaster(covs, filename = "Otjozondjupa_Stack_2016.tif", format = "GTiff", overwrite = TRUE)

# assign raster values to training data
v<-as.data.frame(extract(covs,trainData))
trainData@data=data.frame(trainData@data, v[match(rownames(trainData@data),rownames(v)),])

# rename fields in training dataset
names(trainData) <- c("waypoint_no", "latitude", "longitude", "crown_cover", 
                      "shrubs_less1.5","shrubs_more1.5_no_stem","shrubs_more1.5_stem","shrubs_more1.5",
                      "NDVI","band2","band3","band4","band5","band6","band7")

# remove NAs
trainData@data<-trainData@data[complete.cases(trainData@data),]

# Export extracted data to .csv
write.csv(trainData@data, file = "Otjozondjupa_MF_trainData.csv",row.names=FALSE)

# randomly select index numbers and use that for splitting the data
# set 75% as training nad 25% as test data
trainIndex=sample(1:nrow(trainData@data),size=0.75*nrow(trainData@data))
trainingSet=trainData@data[trainIndex,]
testingSet=trainData@data[-trainIndex,]

# predict crown_cover
accuracies<-c()

# construct the rf model
cc_model <- randomForest(x=trainingSet[,c(9:10)],
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

