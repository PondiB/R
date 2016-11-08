## ----echo=FALSE----------------------------------------------------------
#Land Use Land Cover classification
#John Mutua, CIAT

## ------------------------------------------------------------------------
#clear your workspace
rm(list = ls(all = TRUE))

## ----echo=FALSE----------------------------------------------------------
library("knitr")
opts_knit$set(root.dir = "C:/LDN_Workshop/Sample_dataset/Land_Use_Land_Cover")

## ----setup, include=FALSE, cache=FALSE-----------------------------------
muffleError <- function(x,options) {}
knit_hooks$set(error=muffleError)

## ------------------------------------------------------------------------
#set the start of spatial data processing
startTime <- Sys.time()
cat("Start time", format(startTime),"\n")

## ------------------------------------------------------------------------
#set your working directory
setwd("C:/LDN_Workshop/Sample_dataset/Land_Use_Land_Cover")

## ------------------------------------------------------------------------
#load packages
.packages = c("rgdal","raster","caret")
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])
lapply(.packages, require, character.only=TRUE)

## ------------------------------------------------------------------------
#set random seed
set.seed(322)

## ------------------------------------------------------------------------
#import the image into R
img <- brick("TOA/Otji_TOA.tif")
names(img) <- c(paste0("B", 2:7, coll = ""))
img

## ------------------------------------------------------------------------
#calculate NDVI and save the output for use in the next session
NDVI <- (img[[4]] - img[[3]]) / (img[[4]] + img[[3]])
writeRaster(NDVI, "Otji_NDVI.tif", overwrite=TRUE)
plot(NDVI, main="NDVI map - Otjiwarongo")

## ------------------------------------------------------------------------
#make a natural colour visualization of the Landsat image
plotRGB(img * (img >= 0), r = 4, g = 3, b = 2, scale = 10000)

## ------------------------------------------------------------------------
#import the training data into R
trainData <- shapefile("Training_data/Otji_trainingData.shp")
responseCol <- "LC_Code"

## ------------------------------------------------------------------------
#plot the training data.
plot(trainData, main="Distribution of training data", axes=FALSE)

## ------------------------------------------------------------------------
#extract the pixel values in the training areas
trainSet = data.frame(matrix(vector(), nrow = 0, ncol = length(names(img)) + 1))
for (i in 1:length(unique(trainData[[responseCol]]))){
  category <- unique(trainData[[responseCol]])[i]
  categorymap <- trainData[trainData[[responseCol]] == category,]
  dataSet <- extract(img, categorymap)
  
  if(is(trainData, "SpatialPointsDataFrame")){
    dataSet <- cbind(dataSet, class = as.numeric(category))
    trainSet <- rbind(trainSet, dataSet)
  }
  if(is(trainData, "SpatialPolygonsDataFrame")){
    dataSet <- lapply(dataSet, function(x){cbind(x, class = 
                                                   as.numeric(rep(category, 
                                                                  nrow(x))))})
    df <- do.call("rbind", dataSet)
    trainSet <- rbind(trainSet, df)
  }
}

## ------------------------------------------------------------------------
#partition the data into training and testing
inData <- createDataPartition(y = trainSet$class, p = 0.7, list = FALSE)
training <- trainSet[inData,]
testing <- trainSet[-inData,]

## ------------------------------------------------------------------------
#how does the class look like
table(training$class)
table(testing$class)

## ------------------------------------------------------------------------
#randomly sample from the training data.frame
train_sample <- training[sample(1:nrow(training), 1000), ]
table(train_sample$class)

## ------------------------------------------------------------------------
#fit the trandom forest model
rf_Otji <- train(as.factor(class) ~ B3 + B4 + B5, method = "rf", data =  
                   train_sample)

## ------------------------------------------------------------------------
#cluster the predictions
beginCluster()
prediction <- clusterR(img, raster::predict, args = list(model = rf_Otji))
endCluster()

## ------------------------------------------------------------------------
#test accuracy of testing dataset
prediction_2 <- predict(rf_Otji, testing)
confusionMatrix(prediction_2, testing$class)$overall[1]
confusionMatrix(prediction_2, testing$class)$byClass[, 1]

## ------------------------------------------------------------------------
#save your classified image as a GeoTIFF
writeRaster(prediction, "Otji_classified.tif", overwrite=TRUE)

## ------------------------------------------------------------------------
#visualize your classified image
cols <- c("dark green", "forestgreen", "yellow", "magenta", "blue", "red")
plot(prediction, col=cols, legend=FALSE, main="Land Use Land Cover-Otjiwarongo")
legend("bottomright", 
       legend=c("Forest", "Woodland", "Bushland", "Grassland", "Water body", 
                "Bare land"), fill=cols, bg="white", 
       title = "Land cover 2016")

## ------------------------------------------------------------------------
#check the amount of time you spent conducting this analysis
timeDiff <- Sys.time() - startTime
cat("\nProcessing time", format(timeDiff), "\n")

