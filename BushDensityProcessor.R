## ----echo=FALSE----------------------------------------------------------
#Bush density mapping
#John Mutua, CIAT

## ------------------------------------------------------------------------
#clear your work space
rm(list = ls(all = TRUE))

## ----echo=FALSE----------------------------------------------------------
library("knitr")
opts_knit$set(root.dir= "C:/LDN_Workshop/Sample_dataset/Bush_Density_Mapping")

## ----setup, echo=FALSE, include=FALSE, cache=FALSE-----------------------
muffleError <- function(x,options) {}
knit_hooks$set(error=muffleError)

## ------------------------------------------------------------------------
#set the random seed
set.seed(211134)

## ------------------------------------------------------------------------
#set the start of data processing
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")

## ------------------------------------------------------------------------
#set working directory
setwd("C:/LDN_Workshop/Sample_dataset/Bush_Density_Mapping")

## ------------------------------------------------------------------------
#load packages
.packages = c("sp","rgdal","raster","randomForest","plyr","xlsx","xlsxjars",
              "dplyr","caret","car", "e1071","snow")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

## ----help, eval=FALSE----------------------------------------------------
## #this is how you get more help on functions
## help(calc)
## ?calc

## ------------------------------------------------------------------------
#read in data from excel sheet
raw.d <- read.xlsx("Field_data/Otji_BD_Sampling_Points.xlsx", sheetName = 
                        "Sheet1", header=TRUE)

## ------------------------------------------------------------------------
#calculate values
raw.d$shrubs_less_1.5 <- apply(raw.d[,8:11], 1, sum, na.rm=TRUE)
raw.d$shrubs_more_1.5_no_stem <- apply(raw.d[,16:19], 1, sum, na.rm=TRUE)
raw.d$shrubs_more_1.5_stem <- apply(raw.d[,24:27], 1, sum, na.rm=TRUE)

## ------------------------------------------------------------------------
#create new dataframe with columns you need
raw.d<-raw.d[,c("Waypoint_No","Latitude","Longitude",
                            "shrubs_less_1.5","shrubs_more_1.5_no_stem",
                            "shrubs_more_1.5_stem")]

## ------------------------------------------------------------------------
#add two new columns of shrubs > 1.5 and all shrubs in general
raw.d$shrubs_more_1.5 <- apply(raw.d[,5:6], 1, sum, na.rm=TRUE)
raw.d$shrubs_all <- apply(raw.d[,4:6], 1, sum, na.rm=TRUE)

## ------------------------------------------------------------------------
#round columns
raw.d<-raw.d %>% 
  mutate_each(funs(round(.,0)), shrubs_less_1.5, shrubs_more_1.5, 
              shrubs_all)
raw.d<-raw.d[,c("Waypoint_No","Latitude","Longitude","shrubs_less_1.5", 
        "shrubs_more_1.5", "shrubs_all")]

## ------------------------------------------------------------------------
#compute shrubs per hectare
raw.d$shrubs_less_1.5 <- raw.d$shrubs_less_1.5*25
raw.d$shrubs_more_1.5 <- raw.d$shrubs_more_1.5*25
raw.d$shrubs_all <- raw.d$shrubs_all*25

## ------------------------------------------------------------------------
#remove all NAs
raw.d<-raw.d[complete.cases(raw.d),]

## ------------------------------------------------------------------------
#plot histograms of the three variables
hist(raw.d$shrubs_less_1.5, col = "lightblue", xlab="Count", main="Shrubs, [<1.5]")
hist(raw.d$shrubs_more_1.5, col = "lightblue", xlab="Count", main="Shrubs, [>1.5]")
hist(raw.d$shrubs_all, col = "lightblue", xlab="Count", main="Shrubs, [all]")

## ------------------------------------------------------------------------
#export data to .csv
write.csv(raw.d, file = "Otji_BushData_trainData.csv",row.names=FALSE)

## ------------------------------------------------------------------------
#make shapefiles
xy <- raw.d[,c(3,2)]
trainDatageo <- SpatialPointsDataFrame(coords = xy, data = raw.d,
                                    proj4string = CRS("+proj=longlat 
                                                      +datum=WGS84"))
trainData <- spTransform(trainDatageo, CRS('+proj=utm +zone=33 +south 
                                           +datum=WGS84'))

## ------------------------------------------------------------------------
#rename fields
names(trainData)
names(trainData) <- c("Waypoint_No","Latitude","Longitude","shrubs_less_1.5",
                      "shrubs_more_1.5", "shrubs_all")

## ------------------------------------------------------------------------
#import the rest of input data, stack and rename contents
r.list<-list.files(".", pattern = ".tif$", full.names = TRUE)
r.stack <- stack(r.list)
names(r.stack) <- c("cc","ndvi","b2","b3","b4","b5","b6","b7")

## ------------------------------------------------------------------------
#import the bush area mask
o.mask <- raster("Other_data/Otji_BushArea_2016.tif")

## ------------------------------------------------------------------------
#set extent of the training data to match covs
trainData@bbox <- bbox(o.mask )

## ------------------------------------------------------------------------
#plot the points on top of `layer 3` of the raster stack
plot(r.stack[[3]])
plot(trainData, add=TRUE, col = "red", pch = 3)

## ------------------------------------------------------------------------
#mask and remove NAs in the covariates
covs <- mask(r.stack, o.mask )
covs <- na.omit(covs)

## ------------------------------------------------------------------------
#assign raster values to the training data
v<-as.data.frame(extract(covs,trainData))
trainData@data=data.frame(trainData@data, v[match(rownames(trainData@data),
                                                  rownames(v)),])

## ------------------------------------------------------------------------
#rename fields in the training dataset, remove NAs, write the dataset 
names(trainData) <- c("waypoint.no","lat","lon","shrubs.l1.5","shrubs.g1.5",
                      "shrubs.all","cc","ndvi","b2","b3","b4","b5","b6","b7")
trainData@data<-trainData@data[complete.cases(trainData@data),]
write.csv(trainData@data, file = "Otji_MF_trainData.csv",row.names=FALSE)

## ------------------------------------------------------------------------
#compute summary statistics
summary(trainData$shrubs.all)
skewness(trainData$shrubs.all, na.rm=T)

## ------------------------------------------------------------------------
#QQ plot
qqnorm(trainData$shrubs.all)
qqline(trainData$shrubs.all)

## ------------------------------------------------------------------------
#compute correlation coefficients and plot correlations
cor(trainData@data[,4:14])
pairs(trainData@data[,4:14])

## ------------------------------------------------------------------------
#correlate count of shrubs with NDVI and Landsat 8 band 2-7
cor(trainData@data$shrubs.l1.5,trainData@data$ndvi)
cor(trainData@data$shrubs.all,trainData@data$ndvi)
cor(trainData@data$shrubs.l1.5,trainData@data$cc)
cor(trainData@data$shrubs.all,trainData@data$cc)
cor(trainData@data$shrubs.l1.5,trainData@data$b2)
cor(trainData@data$shrubs.all,trainData@data$b2)
cor(trainData@data$shrubs.l1.5,trainData@data$b3)
cor(trainData@data$shrubs.all,trainData@data$b3)
cor(trainData@data$shrubs.l1.5,trainData@data$b6)
cor(trainData@data$shrubs.all,trainData@data$b6)
cor(trainData@data$shrubs.l1.5,trainData@data$b7)
cor(trainData@data$shrubs.all,trainData@data$b7)

## ------------------------------------------------------------------------
#select covariates based on correlation analysis
d <- trainData@data[,c("waypoint.no","lat","lon","shrubs.l1.5","shrubs.all",
                       "cc", "ndvi")]
names(d)

## ------------------------------------------------------------------------
#fit the model for shrubs <1.5m
tcontrol1 <- trainControl(method="boot", number=100)
model1 <- train(shrubs.l1.5~cc+ndvi,method='rf', trControl = tcontrol1, data=d)

## ------------------------------------------------------------------------
#fit the model for all shrubs
tcontrol3 <- trainControl(method="boot", number=100)
model3 <- train(shrubs.all~cc+ndvi,method='rf',trControl = tcontrol3, data=d)

## ------------------------------------------------------------------------
#print models
print(model1)
print(model3)

## ------------------------------------------------------------------------
#predict models
beginCluster()
prediction1 <- clusterR(covs, raster::predict, args = list(model = model1))
prediction3 <- clusterR(covs, raster::predict, args = list(model = model3))
endCluster()

## ------------------------------------------------------------------------
#compute the density for shrubs >1.5m
prediction2 <- prediction3 - prediction1

## ------------------------------------------------------------------------
#round the numbers
prediction1<-round(prediction1, digits = 0)
prediction2<-round(prediction2, digits = 0)
prediction3<-round(prediction3, digits = 0)
writeRaster(prediction1, "otji_bd1.tif", overwrite=TRUE)
writeRaster(prediction2, "otji_bd2.tif", overwrite=TRUE)
writeRaster(prediction3, "otji_bd3.tif", overwrite=TRUE)

## ------------------------------------------------------------------------
#plot the three maps
plot(prediction1, main="Density for shrubs <1.5m", axes=FALSE)
plot(prediction2, main="Density for shrubs >1.5m", axes=FALSE)
plot(prediction3, main="Density for shrubs", axes=FALSE)

## ------------------------------------------------------------------------
#check amount of time spent
timeDiff <- Sys.time() - startTime
cat("\nProcessing time", format(timeDiff), "\n")

