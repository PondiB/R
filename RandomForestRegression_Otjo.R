#calculate bush density using count data with ndvi and crown cover data as covariates
#author: Mutua John
#email: j.y.mutua@cgiar.org

#workspace clearance
rm(list = ls(all = TRUE))

#set the random seed 
set.seed(500)

#list of packages for session
#packages will be installed if not already installed
#packages will then be loaded into the session
.packages = c("sp","rgdal","raster","randomForest","plyr","xlsx","xlsxjars",
              "dplyr","caret","car", "e1071","snow")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

#set working directory
setwd("C:/Bush_Density_Mapping")

#read in data from excel sheet
BushData <- read.xlsx("Field_data/Bush_Density_Sampling_Points.xlsx", sheetName = "BushDensity", header=TRUE)

#calculate values by finding the sum of values for counts
#1 plot 0.01 ha; 4 plots 0.04 ha
BushData$shrubs_less_1.5 <- apply(BushData[,8:11], 1, sum, na.rm=TRUE)
BushData$shrubs_more_1.5_no_stem <- apply(BushData[,16:19], 1, sum, na.rm=TRUE)
BushData$shrubs_more_1.5_stem <- apply(BushData[,24:27], 1, sum, na.rm=TRUE)

#create new data frame with the columns you need
BushData_clean<-BushData[,c("Waypoint_No","Latitude","Longitude", "shrubs_less_1.5", 
                            "shrubs_more_1.5_no_stem", "shrubs_more_1.5_stem")]

#add two new columns of shrubs more than 1.5 and all shrubs in general
BushData_clean$shrubs_more_1.5 <- apply(BushData_clean[,5:6], 1, sum, na.rm=TRUE)
BushData_clean$shrubs_all <- apply(BushData_clean[,4:6], 1, sum, na.rm=TRUE)

#round columns
BushData_clean<-BushData_clean %>% mutate_each(funs(round(.,0)), shrubs_less_1.5, 
                                               shrubs_more_1.5_no_stem, shrubs_more_1.5_stem, 
                                               shrubs_more_1.5, shrubs_all)

#remove NAs
BushData_clean<-BushData_clean[complete.cases(BushData_clean),]

#export data to .csv
write.csv(BushData_clean, file = "Otjo_BushData.csv",row.names=FALSE)

#get long and lat from your data.frame. Make sure that the order is in lon/lat.
#convert the dataraframe into a spatial point dataframe
xy <- BushData_clean[,c(3,2)]
trainDatageo <- SpatialPointsDataFrame(coords = xy, data = BushData_clean,
                                    proj4string = CRS("+proj=longlat 
                                                      +datum=WGS84"))
trainData <- spTransform(trainDatageo, CRS('+proj=utm +zone=33 +south 
                                           +datum=WGS84'))

#check names of fields
names(trainData)

#rename trainData
names(trainData) <- c("waypoint_No","latitude","longitude","shrubs_less_1.5", "shrubs_more_1.5_no_stem", 
                      "shrubs_more_1.5_stem", "shrubs_more_1.5", "shrubs_all")

#----import input data----
rasList <- list.files("Raster_data/", pattern = ".tif$", full.names = TRUE)

#create a raster stack and rename the contents
rstack <- stack(rasList)
names(rstack) <- c("crown_cover","NDVI","band2","band3","band4","band5","band6","band7")

#import Bush Area raster
Otjo_BushArea <- raster("Raster_data/Other_data/Otjozondjupa_BushArea_2016.tif")

#check various stuff
crs(rstack)
crs(Otjo_BushArea)
crs(trainData)
extent(rstack)
extent(Otjo_BushArea)
extent(trainData)

#set extent of the training data to match covs
trainData@bbox <- bbox(Otjo_BushArea)

#mask the covariates or read in covariates
covs <- mask(rstack, Otjo_BushArea)

#assign raster values to training data
v<-as.data.frame(extract(covs,trainData))
trainData@data=data.frame(trainData@data, v[match(rownames(trainData@data),
                                                  rownames(v)),])

#rename fields in training dataset
#remove NAs
names(trainData) <- c("waypoint_no","latitude","longitude","shrubs_less1.5", "shrubs_more1.5_no_stem",
                      "shrubs_more1.5_stem", "shrubs_more1.5", "shrubs_all", "crown_cover","NDVI",
                      "band2","band3","band4","band5","band6","band7")
trainData@data<-trainData@data[complete.cases(trainData@data),]
write.csv(trainData@data, file = "Otjo_RF_trainData.csv",row.names=FALSE)

#let's plot  some of the variables
scatterplot(NDVI ~ shrubs_less1.5|crown_cover, data=trainData@data, 
            xlab="Count of Shrubs less than 1.5m",ylab="NDVI", main="NDVI versus Count of Shrubs (less than 1.5m)")
scatterplot(NDVI ~ shrubs_more1.5, data=trainData@data, 
            xlab="Count of Shrubs more than 1.5m",ylab="NDVI", main="NDVI versus Count of shrubs more than 1.5m")
scatterplot(NDVI ~ shrubs_all, data=trainData@data, 
            xlab="Count of shrub (all)",ylab="NDVI", main="NDVI versus Count of shrubs")

#############################################
#build the three models by fitting the model using the 'train' function from the 'caret' package.
#use Bootstrap resampling method to estimate model accuracy.
tcontrol1 <- trainControl(method="boot", number=100)
model1 <- train(shrubs_less1.5~crown_cover+NDVI,method='rf', trControl = tcontrol1, data=trainData@data)
				
tcontrol2 <- trainControl(method="boot", number=100)
model2 <- train(shrubs_more1.5~crown_cover+NDVI,method='rf', trControl = tcontrol2, data=trainData@data)
				
tcontrol3 <- trainControl(method="boot", number=100)
model3 <- train(shrubs_all~crown_cover+NDVI,method='rf', trControl = tcontrol3, data=trainData@data)


#print the 3 models
print(model1)
print(model2)
print(model3)

#predict the three models
beginCluster()
prediction1 <- clusterR(covs, raster::predict, args = list(model = model1))
prediction2 <- clusterR(covs, raster::predict, args = list(model = model2))
prediction3 <- clusterR(covs, raster::predict, args = list(model = model3))
endCluster()

#save the predicted images as GeoTIFFs
writeRaster(prediction1, "otjo_bd1.tif", overwrite=TRUE)
writeRaster(prediction2, "otjo_bd2.tif", overwrite=TRUE)
writeRaster(prediction3, "otjo_bd3.tif", overwrite=TRUE)

#plot the predictions.
plot(prediction1, main="Density for shrubs less than 1.5m", axes=FALSE)
plot(prediction2, main="Density for shrubs more than 1.5m", axes=FALSE)
plot(prediction3, main="Density for all shrubs", axes=FALSE)
