# calculate bush density using count data using ndvi and crown cover data as covariates
# author: Mutua John
# email: j.y.mutua@cgiar.org

# workspace clearance
rm(list = ls(all = TRUE))

# set the random seed 
set.seed(500)

# List of packages for session
.packages = c("sp","rgdal","raster","randomForest","plyr","xlsx","xlsxjars","dplyr","caret","car")
# Install CRAN packages (if not already installed)
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
# Load packages into session 
lapply(.packages, require, character.only=TRUE)

# set working directory
setwd("C:/Users/jymutua/WorkDocs/Bush_Density_Mapping")

# read in data from excel sheet
BushData <- read.xlsx("Field_data/Bush_Density_Sampling_Points.xlsx", sheetName = "BushDensity", header=TRUE)

# calculate values by finding the sum of values for counts
# 1 plot 0.01 ha; 4 plots 0.04 ha
BushData$shrubs_less_1.5 <- apply(BushData[,8:11], 1, sum, na.rm=TRUE)
BushData$shrubs_more_1.5_no_stem <- apply(BushData[,16:19], 1, sum, na.rm=TRUE)
BushData$shrubs_more_1.5_stem <- apply(BushData[,24:27], 1, sum, na.rm=TRUE)

# create new data frame with the columns you need
BushData_clean<-BushData[,c("Waypoint_No","Latitude","Longitude",
                            "shrubs_less_1.5","shrubs_more_1.5_no_stem","shrubs_more_1.5_stem")]

# add two new columns of shrubs more than 1.5 and all shrubs in general
BushData_clean$shrubs_more_1.5 <- apply(BushData_clean[,5:6], 1, sum, na.rm=TRUE)
BushData_clean$shrubs_all <- apply(BushData_clean[,4:6], 1, sum, na.rm=TRUE)

# round columns
BushData_clean<-BushData_clean %>% mutate_each(funs(round(.,0)), shrubs_less_1.5, 
                                               shrubs_more_1.5_no_stem, shrubs_more_1.5_stem, 
                                               shrubs_more_1.5, shrubs_all)

# remove NAs
BushData_clean<-BushData_clean[complete.cases(BushData_clean),]

# export data to .csv
write.csv(BushData_clean, file = "Otjozondjupa_BushData_trainData.csv",row.names=FALSE)

# # get long and lat from your data.frame. Make sure that the order is in lon/lat.
# xy <- BushData_clean[,c(3,2)]
# trainData <- SpatialPointsDataFrame(coords = xy, data = BushData_clean,
#                                     proj4string = CRS("+proj=utm +zone=33 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# read point shapefile
trainData <- readOGR(getwd(), layer = "BD_trainData")

# check names of fields
names(trainData)

# rename trainData
names(trainData) <- c("Waypoint_No","Latitude","Longitude","shrubs_less_1.5",
                      "shrubs_more_1.5_no_stem", "shrubs_more_1.5_stem", "shrubs_more_1.5", "shrubs_all")

# ----import input data----
rasList <- list.files("Raster_data/", pattern = ".tif$", full.names = TRUE)

# create a raster stack
rstack <- stack(rasList)

# rename rstack contents
names(rstack) <- c("crown_cover","NDVI","band2","band3","band4","band5","band6","band7")

# import Bush Area raster
Otjo_BushArea <- raster("C:/Users/jymutua/WorkDocs/Bush_Density_Mapping/Raster_data/Other_data/Otjozondjupa_BushArea_2016.tif")

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
names(trainData) <- c("waypoint_no","latitude","longitude","shrubs_less1.5",
                      "shrubs_more1.5_no_stem","shrubs_more1.5_stem","shrubs_more1.5", "shrubs_all",
                      "crown_cover","NDVI","band2","band3","band4","band5","band6","band7")

# remove NAs
trainData@data<-trainData@data[complete.cases(trainData@data),]

# export extracted data to .csv
write.csv(trainData@data, file = "Otjozondjupa_MF_trainData.csv",row.names=FALSE)

# randomly select index numbers and use that for splitting the data
# set 75% as training nad 25% as test data
trainIndex=sample(1:nrow(trainData@data),size=0.75*nrow(trainData@data))
trainingSet=trainData@data[trainIndex,]
testingSet=trainData@data[-trainIndex,]

# plot  variables
scatterplot(NDVI ~ shrubs_less1.5+crown_cover, data=trainingSet, 
            xlab="Count of Shrubs less than 1.5m",ylab="NDVI", main="NDVI versus Count of Shrubs (less than 1.5m)")

scatterplot(NDVI ~ shrubs_more1.5, data=trainingSet, 
            xlab="Count of shrub (more than 1.5m)",ylab="NDVI", main="NDVI versus Count of shrub (more than 1.5m)")

scatterplot(shrubs_more1.5 ~ crown_cover, data=trainingSet, 
            xlab="Crown cover",ylab="shrubs_more1.5", main="shrubs_more1.5 versus Crown cover")

scatterplot(shrubs_less1.5 ~ crown_cover, data=trainingSet, 
            xlab="Crown cover",ylab="shrubs_less1.5", main="shrubs_less1.5 versus Crown cover")

#############################################
# set up the model using randomly sampled data
# predict shrubs_less1.5
accuracies1<-c()

# construct the rf model for shrubs less than 1.5m
model1 <- randomForest(x=trainingSet[,c(9:10)],
                       y=trainingSet[,"shrubs_less1.5"], 
                       ntree=2000, proximity=TRUE, importance=TRUE)
# check for error convergence
plot(model1)
print(model1)

# plot mean decrease in variable importance
varImpPlot(model1, type=1)

# predict Model
predict(covs, model1, filename="otjo_bd1", format='GTiff', type="response", 
        index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

# check accuracy
prediction1 <- predict(model1, testingSet)
testingSet$rightPred1 <- prediction1 == testingSet$shrubs_less1.5
t1<-table(prediction1, testingSet$shrubs_less1.5)
print(t1)
accuracy1 <- sum(testingSet$rightPred1)/nrow(testingSet)
accuracies1 <- c(accuracies1,accuracy1)
print(accuracy1)

# predict shrubs_more1.5
accuracies2<-c()

# construct the rf model for shrubs more than 1.5m
model2 <- randomForest(x=trainingSet[,c(9:10)],
                       y=trainingSet[,"shrubs_more1.5"], 
                       ntree=2000, proximity=TRUE, importance=TRUE)
# check for error convergence
plot(model2)
print(model2)

# plot mean decrease in variable importance
varImpPlot(model2, type=1)

# Predict Model
predict(covs, model2, filename="otjo_bd2", format='GTiff', type="response", 
        index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

# check accuracy
prediction2 <- predict(model2, testingSet)
testingSet$rightPred2 <- prediction2 == testingSet$shrubs_more1.5
t2<-table(prediction2, testingSet$shrubs_more1.5)
print(t2)
accuracy2 <- sum(testingSet$rightPred2)/nrow(testingSet)
accuracies2 <- c(accuracies2,accuracy2)
print(accuracy2)

# predict shrubs_all
accuracies3<-c()

# construct the rf model for all shrubs
model3 <- randomForest(x=trainingSet[,c(9:10)],
                       y=trainingSet[,"shrubs_all"], 
                       ntree=2000, proximity=TRUE, importance=TRUE)
# check for error convergence
plot(model3)
print(model3)

# plot mean decrease in variable importance
varImpPlot(model3, type=1)

# Predict Model
predict(covs, model3, filename="otjo_bd3", format='GTiff', type="response", 
        index=1, na.rm=TRUE, progress="window", overwrite=TRUE)

# check accuracy
prediction3 <- predict(model3, testingSet)
testingSet$rightPred3 <- prediction3 == testingSet$shrubs_all
t3<-table(prediction3, testingSet$shrubs_all)
print(t3)
accuracy3 <- sum(testingSet$rightPred3)/nrow(testingSet)
accuracies3 <- c(accuracies3,accuracy3)
print(accuracy3)

